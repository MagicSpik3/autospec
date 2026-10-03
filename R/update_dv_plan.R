#' Create or update the DV plan from the specs
#'
#' The one step between the specs and the plan, safe to run at any time. Reads
#' the spec workbooks afresh, saves the catalogue, and drafts the plan. When a
#' plan already exists, every sign-off whose spec text has not changed is kept,
#' the old plan is copied to a `plan_backups` folder first, and what changed is
#' reported. `sheet_priority` in the config settles DVs defined on more than
#' one sheet; those still to decide are written to `QA_duplicate_dvs.csv`.
#'
#' Corrections to `verb`, `args` or `condition` are kept only on rows signed
#' off; the backup holds any others.
#'
#' @param config The config from [read_autospec_config()].
#' @param data_names Optional column names of the input data. When given, every
#'   input matching neither a DV nor a data column is noted in the plan.
#'
#' @return The plan, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' config <- read_autospec_config()
#' update_dv_plan(config)
#' }
update_dv_plan <- function(config, data_names = NULL) {
  check_config_object(config)

  if (length(config$spec_files) == 0L) {
    cli::cli_alert_danger("No spec workbooks found in {.file {config$spec_folder}}; check {.field spec_folder} and {.field spec_pattern} in the config.")
    stop("No spec workbooks found.", call. = FALSE)
  }

  data_catalogue <- make_catalogue(config$spec_files, exclude_sheets = config$exclude_sheets)
  data.table::fwrite(data_catalogue, config$catalogue_file)
  cli::cli_alert_success("Catalogue saved to {.file {config$catalogue_file}}.")
  report_unparsed(data_catalogue)

  previous_plan <- if (file.exists(config$plan_file)) read_dv_plan(config$plan_file) else NULL

  plan <- build_dv_plan(
    data_catalogue,
    data_names = data_names,
    previous_plan = previous_plan,
    sheet_priority = config$sheet_priority
  )

  if (!is.null(previous_plan)) {
    backup <- file.path(
      config$output_folder, "plan_backups",
      paste0("dv_plan_", format(Sys.time(), "%Y-%m-%d_%H%M%S"), ".csv")
    )
    dir.create(dirname(backup), recursive = TRUE, showWarnings = FALSE)
    file.copy(config$plan_file, backup)
    cli::cli_alert_info("The plan as it was is saved as {.file {backup}}.")
    report_plan_changes(previous_plan, plan)
  }

  save_plan_file(plan, config$plan_file)
  write_duplicates_file(plan, config$duplicates_file)

  invisible(plan)
}


#' Write the plan, explaining the usual reason it cannot be
#' @keywords internal
#' @noRd
save_plan_file <- function(plan, path) {
  tryCatch(
    write_dv_plan(plan, path),
    error = function(problem) {
      cli::cli_alert_danger(
        "Could not write {.file {path}}: {conditionMessage(problem)}. If it is open in Excel, close it and run this again."
      )
      stop("The plan could not be written.", call. = FALSE)
    }
  )
}


#' Say how many derivations the parser could not read
#' @keywords internal
#' @noRd
report_unparsed <- function(data_catalogue) {
  if (!"parse_status" %in% names(data_catalogue)) {
    return(invisible(NULL))
  }

  unparsed <- data_catalogue$variable[data_catalogue$parse_status %in% "unparsed"]

  if (length(unparsed) > 0L) {
    cli::cli_alert_warning(
      "{length(unparsed)} derivation{?s} could not be read, usually a typo in the spec. They are the {.val unparsed} rows of the catalogue: {.val {utils::head(unparsed, 10)}}{if (length(unparsed) > 10) ', ...' else ''}."
    )
  }

  invisible(NULL)
}


#' Say what changed between the old plan and the new one
#' @keywords internal
#' @noRd
report_plan_changes <- function(previous_plan, plan) {
  key_of <- function(rows) paste(tolower(clean_dv_name(rows$dv)), trimws(rows$sheet_name), sep = "\r")
  old_key <- key_of(previous_plan)
  new_key <- key_of(plan)

  added <- plan$dv[!new_key %in% old_key]
  removed <- previous_plan$dv[!old_key %in% new_key]

  was_reviewed <- old_key[previous_plan$status %in% "reviewed"]
  lost <- plan$dv[new_key %in% was_reviewed & !(plan$status %in% c("reviewed", "duplicate"))]

  cli::cli_h2("Changes since the last plan")

  if (length(added) + length(removed) + length(lost) == 0L) {
    cli::cli_alert_success("No rows added or removed, and every sign-off kept.")
    return(invisible(NULL))
  }

  describe <- function(names) paste0(paste(utils::head(names, 10), collapse = ", "), if (length(names) > 10) ", ..." else "")

  if (length(added) > 0L) {
    cli::cli_alert_info("{length(added)} new row{?s}: {describe(added)}.")
  }

  if (length(removed) > 0L) {
    cli::cli_alert_info("{length(removed)} row{?s} no longer in the specs: {describe(removed)}.")
  }

  if (length(lost) > 0L) {
    cli::cli_alert_warning(
      "{length(lost)} sign-off{?s} dropped because the spec text changed: {describe(lost)}. Their notes say who signed them off; check and sign off again."
    )
  }

  invisible(NULL)
}


#' DVs on more than one row still waiting for someone to choose the row to build from
#'
#' A DV is waiting when more than one of its rows is not marked duplicate,
#' unless those rows are all signed off with the same code.
#' @keywords internal
#' @noRd
duplicates_to_decide <- function(plan) {
  groups <- repeated_dv_rows(plan)

  waiting <- Filter(function(rows) {
    candidates <- rows[!(plan$status[rows] %in% "duplicate")]
    all_same_sign_off <- all(plan$status[candidates] %in% "reviewed") &&
      length(unique(plan_drafts(plan, candidates))) == 1L

    length(candidates) > 1L && !all_same_sign_off
  }, groups)

  rows <- unlist(waiting, use.names = FALSE)

  data.frame(
    dv = as.character(plan$dv[rows]),
    topic = suite_topic(plan$file_name[rows]),
    sheet_name = as.character(plan$sheet_name[rows]),
    excel_row = as.integer(plan$excel_row[rows]),
    status = as.character(plan$status[rows]),
    reviewed_by = as.character(plan$reviewed_by[rows]),
    instructions = as.character(plan$instructions[rows]),
    stringsAsFactors = FALSE
  )
}


#' Write the DVs still to decide, or remove an old list when there are none
#' @keywords internal
#' @noRd
write_duplicates_file <- function(plan, path) {
  waiting <- duplicates_to_decide(plan)

  if (nrow(waiting) == 0L) {
    if (file.exists(path)) {
      file.remove(path)
    }

    cli::cli_alert_success("Every DV defined on more than one row has one row to build from.")
    return(invisible(NULL))
  }

  data.table::fwrite(waiting, path)
  dv_count <- length(unique(tolower(waiting$dv)))

  cli::cli_alert_warning(
    "{dv_count} DV{?s} {?is/are} defined on more than one row with different text; {.file {basename(path)}} shows them side by side."
  )
  cli::cli_bullets(c(
    " " = "To settle a whole sheet at once, list the preferred sheet under {.field sheet_priority} in the config and update the plan again.",
    " " = "Or, in the plan, sign off the right row for each and set the others' status to {.val duplicate}."
  ))

  invisible(path)
}
