#' Where the round has got to, and what to do next
#'
#' Looks at the plan, the suite and, when given the data's column names, the
#' inputs, without changing anything. Prints a line per topic and the one
#' command to run next. Run it whenever unsure where things stand.
#'
#' @param config The config from [read_autospec_config()].
#' @param data_names Optional column names of the data, to check the suite's
#'   inputs are there. `names(haven::read_sav(config$data_file, n_max = 0))`
#'   reads them without loading the data.
#'
#' @return A list, invisibly: `topics`, a data frame with a row per topic, and
#'   `next_step`, the advice printed last.
#' @export
#'
#' @examples
#' \dontrun{
#' autospec_status(read_autospec_config())
#' }
autospec_status <- function(config, data_names = NULL) {
  check_config_object(config)

  cli::cli_h1("Where Round {config$round} has got to")

  topics <- if (length(config$topics) > 0L) config$topics else config$all_topics

  if (length(config$topics) > 0L) {
    cli::cli_alert_info("Showing the {length(topics)} topic{?s} chosen in the config: {.val {topics}}.")
  }

  if (!file.exists(config$plan_file)) {
    return(finish_status(
      data.frame(topic = topics, stringsAsFactors = FALSE),
      "There is no plan yet. Draft it from the specs: run the {.field plan} chunk, or Addins > autospec: Update the plan."
    ))
  }

  plan <- as.data.frame(suppressMessages(read_dv_plan(config$plan_file)), stringsAsFactors = FALSE)
  plan$topic <- suite_topic(plan$file_name)
  plan <- plan[plan$topic %in% topics, , drop = FALSE]

  suite_exists <- file.exists(file.path(config$suite_folder, "settings.R"))
  suite_steps <- if (suite_exists) suite_step_index(config$suite_folder) else NULL
  ready_to_sync <- if (suite_exists) steps_ready_to_sync(plan, suite_steps, data_names, config$name_case) else character()
  waiting <- duplicates_to_decide(plan)

  table <- status_table(plan, topics, suite_steps, ready_to_sync, waiting)
  print(table, row.names = FALSE)

  unnamed <- sum(plan$status %in% "reviewed" & is_blank(plan$reviewed_by))

  if (unnamed > 0L) {
    cli::cli_alert_warning("{unnamed} row{?s} {?is/are} marked reviewed with no {.field reviewed_by}. Check {?it was/they were} really signed off; a trial run tries DVs out without signing anything off.")
  }

  non_derivations <- sum(signed_off_non_derivations(plan))

  if (non_derivations > 0L) {
    cli::cli_alert_warning("{non_derivations} row{?s} marked reviewed {?is/are} not a derivation by {?its/their} notes; set {?it/them} back to not_a_derivation.")
  }

  missing_inputs <- character()

  if (!is.null(data_names) && suite_exists) {
    suite <- suppressMessages(read_dv_suite(config$suite_folder))
    missing_inputs <- report_missing_inputs(suite$steps, data_names, topics)
  }

  to_sign_off <- sum(plan$status %in% c("auto", "needs_review", "hand_written"))
  not_written <- if (suite_exists) sum(!suite_steps$written[suite_steps$topic %in% topics]) else 0L

  next_step <- if (nrow(waiting) > 0L) {
    paste0(
      length(unique(tolower(waiting$dv))), " DV(s) are defined on more than one row with different text. ",
      "Choose which row each is built from: see {.file ", config$duplicates_file, "}, then either list the preferred sheet ",
      "under {.field sheet_priority} in the config or mark the others duplicate in the plan, then update the plan (the {.field plan} chunk)."
    )
  } else if (!suite_exists) {
    "Write the suite: run the {.field sync} chunk, or Addins > autospec: Sync the suite with the plan. Rows not signed off yet become steps to fill in later."
  } else if (length(ready_to_sync) > 0L) {
    paste0(
      length(ready_to_sync), " signed-off DV(s) are still {.code derive = NULL} in the suite. ",
      "Write them in: run the {.field sync} chunk, or Addins > autospec: Sync the suite with the plan."
    )
  } else if (to_sign_off > 0L) {
    paste0(
      to_sign_off, " plan row(s) to sign off in {.file ", config$plan_file, "}; sync the suite after each batch. ",
      "To see output meanwhile, run the {.field trial} chunk, or Addins > autospec: Trial run."
    )
  } else if (not_written > 0L) {
    paste0(not_written, " step(s) still need writing by hand in {.file ", config$suite_folder, "}; check with {.code check_dv_suite()} as you go.")
  } else if (length(missing_inputs) > 0L) {
    "Every step is written, but some inputs are missing from the data (above). Check {.field data_file} in the config."
  } else {
    "Every step is written. Run the {.field check}, {.field build} and {.field export} chunks."
  }

  finish_status(table, next_step)
}


#' @keywords internal
#' @noRd
finish_status <- function(table, next_step) {
  cli::cli_h2("Next")
  cli::cli_alert(next_step)

  invisible(list(topics = table, next_step = next_step))
}


#' Drop the loaded autospec namespace before reinstalling it
#'
#' This is an internal helper for the setup script, where a destructive rebuild
#' is useful while testing a new feature but should remain off by default for
#' normal users.
#'
#' @param destructive When `TRUE`, the loaded package is detached and the
#'   namespace is unloaded so `install.packages()` can rebuild it in a fresh
#'   session without an R restart.
#' @keywords internal
#' @noRd
unload_autospec_for_reinstall <- function(destructive = FALSE) {
  if (!isTRUE(destructive)) {
    return(invisible(FALSE))
  }

  if ("package:autospec" %in% search()) {
    detach("package:autospec", unload = TRUE, character.only = TRUE)
  }

  if ("autospec" %in% loadedNamespaces()) {
    unloadNamespace("autospec")
  }

  invisible(TRUE)
}


#' Build a table of the variables each DV requires and whether they are in the data
#'
#' A one-row-per-input report, useful for normalising the plan around its input
#' dependencies. Every variable is listed once and the table records which DVs
#' use it and whether it is present in the loaded data.
#'
#' @param plan A plan from [read_dv_plan()] or [build_dv_plan()].
#' @param data_names Optional column names of the loaded data. When set, the
#'   `found_in_data` flag is `TRUE` only for names present in the data.
#' @param path Optional CSV file path to write the table to.
#'
#' @return A data frame with one row per distinct input variable: `found_in_data`
#'   says whether the original data has it, `provided_by_dvs` lists any suite
#'   steps that create it, and `available` is true when either source provides
#'   it. `provided_by_uids`, `used_by_dvs` and `used_by_uids` provide the
#'   requirement trace for its producers and consumers.
#' @export
#' @examples
#' 
#' plan <- read_dv_plan("outputs/R9/dv_plan.csv")
#' make_required_input_table(plan, data_names = names(my_data))
#' 
make_required_input_table <- function(plan, data_names = NULL, path = NULL) {
  if (!is.data.frame(plan)) {
    cli::cli_alert_danger("{.arg plan} must be a data frame from the DV plan.")
    stop("`plan` must be a data frame.", call. = FALSE)
  }

  rows <- as.data.frame(plan, stringsAsFactors = FALSE)

  if (!"inputs" %in% names(rows) || !"dv" %in% names(rows)) {
    cli::cli_alert_danger("{.arg plan} must have {.field dv} and {.field inputs} columns.")
    stop("`plan` must have dv and inputs columns.", call. = FALSE)
  }

  if (!is.null(data_names) && !is.character(data_names)) {
    cli::cli_alert_danger("{.arg data_names} must be a character vector of data column names.")
    stop("`data_names` must be a character vector.", call. = FALSE)
  }

  all_inputs <- unique(unlist(lapply(rows$inputs, function(value) {
    expand_variable_names(split_plan_list(value))
  }), use.names = FALSE))
  all_inputs <- all_inputs[!is_blank(all_inputs)]
  all_inputs <- sort(all_inputs, method = "radix")

  if (length(all_inputs) == 0L) {
    table <- data.frame(
      variable = character(),
      found_in_data = logical(),
      provided_by_dvs = character(),
      provided_by_uids = character(),
      available = logical(),
      used_by_dvs = character(),
      used_by_uids = character(),
      stringsAsFactors = FALSE
    )
  } else {
    found_in_data <- if (is.null(data_names)) {
      rep(NA, length(all_inputs))
    } else {
      tolower(all_inputs) %in% tolower(data_names)
    }
    provided_by_dvs <- vapply(all_inputs, function(variable) {
      producers <- rows$dv[!is_blank(rows$dv) & tolower(rows$dv) == tolower(variable)]
      paste(unique(producers), collapse = "; ")
    }, character(1L))
    provided_by_uids <- vapply(all_inputs, function(variable) {
      producers <- which(!is_blank(rows$dv) & tolower(rows$dv) == tolower(variable))
      if (!"uid" %in% names(rows)) return("")
      uids <- as.character(rows$uid[producers])
      paste(unique(uids[!is_blank(uids)]), collapse = "; ")
    }, character(1L))
    used_by <- lapply(all_inputs, function(variable) {
      which(!is_blank(rows$inputs) & vapply(rows$inputs, function(value) {
        variable %in% expand_variable_names(split_plan_list(value))
      }, logical(1L)))
    })

    table <- data.frame(
      variable = all_inputs,
      found_in_data = found_in_data,
      provided_by_dvs = provided_by_dvs,
      provided_by_uids = provided_by_uids,
      available = if (is.null(data_names)) NA else found_in_data | nzchar(provided_by_dvs),
      used_by_dvs = vapply(used_by, function(index) paste(unique(rows$dv[index]), collapse = "; "), character(1L)),
      used_by_uids = vapply(used_by, function(index) {
        if (!"uid" %in% names(rows)) return("")
        uids <- rows$uid[index]
        paste(unique(uids[!is_blank(uids)]), collapse = "; ")
      }, character(1L)),
      stringsAsFactors = FALSE
    )
  }

  if (!is.null(path)) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    data.table::fwrite(table, path)
  }

  table
}


#' One row per topic: review progress in the plan and writing progress in the suite
#' @keywords internal
#' @noRd
status_table <- function(plan, topics, suite_steps, ready_to_sync, waiting) {
  count_in <- function(topic, statuses) sum(plan$topic == topic & plan$status %in% statuses)

  table <- data.frame(
    topic = topics,
    signed_off = vapply(topics, count_in, integer(1L), statuses = "reviewed"),
    to_sign_off = vapply(topics, count_in, integer(1L), statuses = c("auto", "needs_review", "hand_written")),
    duplicates_to_decide = vapply(topics, function(topic) length(unique(tolower(waiting$dv[waiting$topic == topic]))), integer(1L)),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  if (!is.null(suite_steps)) {
    in_topic <- function(topic) suite_steps$topic == topic
    table$steps_written <- vapply(topics, function(topic) sum(in_topic(topic) & suite_steps$written), integer(1L))
    table$steps_not_written <- vapply(topics, function(topic) sum(in_topic(topic) & !suite_steps$written), integer(1L))
    table$ready_to_sync <- vapply(topics, function(topic) {
      sum(in_topic(topic) & tolower(suite_steps$dv) %in% tolower(ready_to_sync))
    }, integer(1L))
  }

  table
}


#' Suite steps still at derive = NULL that a sync would write
#'
#' The steps are drafted exactly as sync_dv_suite() drafts them, so a row
#' signed off with no verb to call is not counted.
#' @keywords internal
#' @noRd
steps_ready_to_sync <- function(plan, suite_steps, data_names = NULL, name_case = "auto") {
  prepared <- tryCatch(
    suppressMessages(prepare_suite_plan(plan, data_names, name_case, strict_duplicates = FALSE)),
    error = function(problem) NULL
  )

  if (is.null(prepared) || nrow(prepared$plan) == 0L) {
    return(character())
  }

  steps <- topic_steps(prepared$plan, prepared, reviewed_only = TRUE, data_names = data_names)
  would_write <- vapply(steps, `[[`, character(1L), "dv")[vapply(steps, `[[`, logical(1L), "written")]
  stubs <- suite_steps$dv[!suite_steps$written]

  stubs[tolower(stubs) %in% tolower(would_write)]
}
