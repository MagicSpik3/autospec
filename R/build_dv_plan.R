#' Draft the DV plan from the catalogue
#'
#' One row per DV: the verb that builds it, its inputs, arguments and
#' condition, a `status` and `notes` saying what to check. Write it with
#' [write_dv_plan()], review and sign off each row (`status = "reviewed"`,
#' `reviewed_by`, `reviewed_on`), then turn it into code with
#' [write_dv_suite()].
#'
#' Pass the last reviewed plan as `previous_plan` to keep every sign-off whose
#' spec text has not changed.
#'
#' A DV defined on more than one sheet is signed off on one row and the others
#' are marked `duplicate`. `sheet_priority` settles this by rule: the row on
#' the sheet earliest in the list is the one to sign off, whatever the text.
#'
#' @param data_catalogue A catalogue from [make_catalogue()].
#' @param data_names Optional column names of the input data. When given,
#'   every input matching neither a DV nor a data column is noted.
#' @param previous_plan Optional earlier plan from [read_dv_plan()].
#' @param sheet_priority Optional sheet names, most preferred first, deciding
#'   which row of a repeated DV is signed off. Matched ignoring surrounding
#'   spaces.
#'
#' @return A data.table with one row per catalogue row.
#' @import data.table
#' @export
#'
#' @examples
#' \dontrun{
#' plan <- build_dv_plan(make_catalogue(list.files("specs", full.names = TRUE)))
#' write_dv_plan(plan, "dv_plan.csv")
#' }
build_dv_plan <- function(data_catalogue, data_names = NULL, previous_plan = NULL,
                          sheet_priority = NULL) {
  if (!is.data.frame(data_catalogue)) {
    cli::cli_alert_danger("{.arg data_catalogue} must be a data frame from make_catalogue().")
    stop("`data_catalogue` must be a data frame.", call. = FALSE)
  }

  required <- c("file_name", "sheet_name", "level", "excel_row", "variable",
                "label", "instructions")
  missing_columns <- setdiff(required, names(data_catalogue))

  if (length(missing_columns) > 0L) {
    cli::cli_alert_danger(
      "{.arg data_catalogue} is missing column{?s}: {.val {missing_columns}}."
    )
    stop("`data_catalogue` is missing required columns.", call. = FALSE)
  }

  if (!is.null(data_names) && !is.character(data_names)) {
    cli::cli_alert_danger("{.arg data_names} must be a character vector.")
    stop("`data_names` must be a character vector.", call. = FALSE)
  }

  if (!is.null(sheet_priority) && !is.character(sheet_priority)) {
    cli::cli_alert_danger("{.arg sheet_priority} must be sheet names in quotes.")
    stop("`sheet_priority` must be a character vector.", call. = FALSE)
  }

  cli::cli_h1("Drafting the DV plan")
  cli::cli_alert_info("Matching {nrow(data_catalogue)} derivation{?s} to verbs in the registry.")

  catalogue <- as.data.frame(data_catalogue, stringsAsFactors = FALSE)
  atomic_catalogue <- expand_atomic_catalogue_rows(catalogue)
  if (!"uid" %in% names(atomic_catalogue) ||
      length(atomic_catalogue$uid) != nrow(atomic_catalogue) ||
      all(is.na(atomic_catalogue$uid))) {
    atomic_catalogue$uid <- sprintf("dv_%06d", seq_len(nrow(atomic_catalogue)))
  }
  dv_names <- clean_dv_name(atomic_catalogue$variable[!is_blank(atomic_catalogue$variable)])
  known_names <- unique(c(expand_variable_names(dv_names), data_names))

  drafts <- lapply(seq_len(nrow(atomic_catalogue)), function(index) {
    draft_plan_row(
      text = atomic_catalogue$instructions[[index]],
      dv = atomic_catalogue$variable[[index]],
      level = atomic_catalogue$level[[index]],
      known_names = known_names,
      check_all_names = !is.null(data_names)
    )
  })

  plan <- data.table::data.table(
    uid = atomic_catalogue$uid,
    dv = clean_dv_name(atomic_catalogue$variable),
    level = atomic_catalogue$level,
    verb = vapply(drafts, `[[`, character(1L), "verb"),
    inputs = vapply(drafts, `[[`, character(1L), "inputs"),
    args = vapply(drafts, `[[`, character(1L), "args"),
    condition = vapply(drafts, `[[`, character(1L), "condition"),
    missing_code = NA_character_,
    status = vapply(drafts, `[[`, character(1L), "status"),
    reviewed_by = NA_character_,
    reviewed_on = NA_character_,
    notes = vapply(drafts, `[[`, character(1L), "notes"),
    file_name = atomic_catalogue$file_name,
    sheet_name = atomic_catalogue$sheet_name,
    excel_row = as.integer(atomic_catalogue$excel_row),
    label = atomic_catalogue$label,
    instructions = atomic_catalogue$instructions
  )

  plan <- flag_duplicate_dvs(plan, sheet_priority)

  if (!is.null(previous_plan)) {
    plan <- carry_over_reviews(plan, previous_plan)
    cli::cli_alert_info("Kept {sum(plan$status %in% 'reviewed')} sign-off{?s} from the previous plan.")
  }

  plan <- prefer_sheets(plan, sheet_priority)
  plan <- settle_duplicate_dvs(plan)

  status_counts <- table(factor(plan$status, levels = plan_statuses()))
  cli::cli_alert_success("Plan drafted: {nrow(plan)} row{?s}.")
  cli::cli_bullets(c(
    " " = "{status_counts[['auto']]} auto: a verb fits clearly; check and sign off",
    " " = "{status_counts[['needs_review']]} needs_review: a verb fits but the notes say what to decide",
    " " = "{status_counts[['hand_written']]} hand_written: no verb fits; will be written by hand",
    " " = "{status_counts[['not_a_derivation']]} not_a_derivation: value labels or derived elsewhere",
    " " = "{status_counts[['duplicate']]} duplicate: the same DV on another row, which is the one to sign off",
    " " = "{status_counts[['reviewed']]} reviewed"
  ))

  plan[]
}


#' Expand one catalogue row into atomic output variables when the output name is a numbered range.
#' @keywords internal
#' @noRd
expand_atomic_catalogue_rows <- function(catalogue) {
  if (!is.data.frame(catalogue)) {
    stop("`catalogue` must be a data frame.", call. = FALSE)
  }

  rows <- lapply(seq_len(nrow(catalogue)), function(index) {
    dv <- catalogue$variable[[index]]
    ranges <- find_number_ranges(dv)
    ranges <- ranges[!duplicated(ranges$match), , drop = FALSE]

    if (nrow(ranges) == 0L || any(ranges$lower > ranges$upper)) {
      return(list(catalogue[index, , drop = FALSE]))
    }

    index_values <- lapply(seq_len(nrow(ranges)), function(position) {
      seq.int(ranges$lower[[position]], ranges$upper[[position]])
    })
    combinations <- rev(expand.grid(rev(index_values), KEEP.OUT.ATTRS = FALSE))

    lapply(seq_len(nrow(combinations)), function(position) {
      numbers <- unlist(combinations[position, ], use.names = FALSE)
      substitute_index <- function(value) {
        if (!is.character(value) || is.na(value)) {
          return(value)
        }

        for (position_in_range in seq_along(numbers)) {
          value <- replace_number_range(value, ranges$match[[position_in_range]], numbers[[position_in_range]])
        }

        value
      }

      out <- catalogue[index, , drop = FALSE]
      out$variable <- substitute_index(dv)
      out$instructions <- substitute_index(catalogue$instructions[[index]])
      out$label <- if (is.null(catalogue$label[[index]]) || is.na(catalogue$label[[index]])) {
        catalogue$label[[index]]
      } else {
        substitute_index(catalogue$label[[index]])
      }
      out
    })
  })

  rows <- unlist(rows, recursive = FALSE)
  atomic <- data.table::as.data.table(
    data.table::rbindlist(rows, fill = TRUE),
    keep.rownames = FALSE
  )

  if (nrow(atomic) > 0L) {
    atomic[, uid := sprintf("dv_%06d", seq_len(nrow(atomic)))]
    atomic <- atomic[, c("uid", setdiff(names(atomic), "uid")), with = FALSE]
  }

  atomic
}


#' Draft one plan row
#' @keywords internal
#' @noRd
draft_plan_row <- function(text, dv, level, known_names, check_all_names) {
  if (is_blank(dv)) {
    return(list(verb = NA_character_, inputs = NA_character_, args = NA_character_,
                condition = NA_character_, status = "needs_review",
                notes = "no variable name against this derivation"))
  }

  if (is_blank(text)) {
    return(list(verb = NA_character_, inputs = NA_character_, args = NA_character_,
                condition = NA_character_, status = "needs_review",
                notes = "no derivation text"))
  }

  match <- match_verb(text, dv, level, known_names)

  name_notes <- character()

  if (length(match$inputs) > 0L) {
    expanded <- expand_variable_names(match$inputs)
    resolution <- resolve_variable_names(expanded, known_names)
    unknown <- resolution$how == "unknown"
    near_miss <- unknown & !is.na(resolution$suggestion)

    if (any(near_miss)) {
      name_notes <- c(name_notes, paste0(
        "unknown variable ", resolution$name[near_miss],
        ", did you mean ", resolution$suggestion[near_miss], "?"
      ))
    }

    if (check_all_names && any(unknown & !near_miss)) {
      name_notes <- c(name_notes, paste0(
        "unknown variable ", resolution$name[unknown & !near_miss]
      ))
    }
  }

  notes <- unique(c(match$reasons, match$notes, name_notes))

  list(
    verb = match$verb,
    inputs = if (length(match$inputs) > 0L) paste(match$inputs, collapse = "; ") else NA_character_,
    args = if (length(match$args) > 0L) format_plan_args(match$args) else NA_character_,
    condition = match$condition,
    status = match$status,
    notes = if (length(notes) > 0L) paste(notes, collapse = "; ") else NA_character_
  )
}


#' Flag DVs defined more than once with different text
#'
#' Those need a person to decide which definition is right, unless
#' sheet_priority decides it. DVs defined more than once with the same text
#' are settled by settle_duplicate_dvs().
#' @keywords internal
#' @noRd
flag_duplicate_dvs <- function(plan, sheet_priority = NULL) {
  for (rows in repeated_dv_rows(plan)) {
    if (same_spec_text(plan$instructions[rows]) || !is.na(priority_row(plan, rows, sheet_priority))) {
      next
    }

    for (index in rows) {
      others <- setdiff(rows, index)
      note <- paste0(
        "also defined on ", describe_plan_rows(plan, others),
        " with different text: sign off the right one and set the others' status to duplicate (decision D8)"
      )

      data.table::set(plan, i = index, j = "notes", value = append_note(plan$notes[[index]], note))

      if (identical(plan$status[[index]], "auto")) {
        data.table::set(plan, i = index, j = "status", value = "needs_review")
      }
    }
  }

  plan
}


#' Settle repeated DVs by sheet_priority: the best-ranked sheet's row is signed off
#'
#' A sign-off on another row moves to it when both would write the same code;
#' otherwise it is left for the reviewer with a note saying where it was.
#' @keywords internal
#' @noRd
prefer_sheets <- function(plan, sheet_priority) {
  if (length(sheet_priority) == 0L) {
    return(plan)
  }

  settled <- 0L

  for (rows in repeated_dv_rows(plan)) {
    home <- priority_row(plan, rows, sheet_priority)

    if (is.na(home)) {
      next
    }

    others <- setdiff(rows, home)
    signed_elsewhere <- others[plan$status[others] %in% "reviewed"]

    if (!(plan$status[[home]] %in% "reviewed")) {
      same_code <- length(signed_elsewhere) > 0L &&
        all(plan_drafts(plan, signed_elsewhere) == plan_drafts(plan, home))

      if (same_code) {
        source_row <- signed_elsewhere[[1L]]
        data.table::set(plan, i = home, j = "status", value = "reviewed")
        data.table::set(plan, i = home, j = "reviewed_by", value = plan$reviewed_by[[source_row]])
        data.table::set(plan, i = home, j = "reviewed_on", value = plan$reviewed_on[[source_row]])
      } else {
        if (plan$status[[home]] %in% "duplicate" || length(signed_elsewhere) > 0L) {
          data.table::set(plan, i = home, j = "status", value = "needs_review")
        }

        if (length(signed_elsewhere) > 0L) {
          data.table::set(plan, i = home, j = "notes", value = append_note(
            plan$notes[[home]],
            paste0("was signed off on ", describe_plan_rows(plan, signed_elsewhere),
                   " with different code; sheet_priority makes this the row to sign off"),
            first = TRUE
          ))
        }
      }
    }

    for (index in others) {
      data.table::set(plan, i = index, j = "status", value = "duplicate")
      data.table::set(plan, i = index, j = "reviewed_by", value = NA_character_)
      data.table::set(plan, i = index, j = "reviewed_on", value = NA_character_)
      data.table::set(plan, i = index, j = "notes", value = append_note(
        plan$notes[[index]],
        paste0("built from ", describe_plan_rows(plan, home), ", preferred by sheet_priority")
      ))
    }

    settled <- settled + 1L
  }

  if (settled > 0L) {
    cli::cli_alert_info("{settled} DV{?s} defined on more than one sheet {?was/were} settled by {.field sheet_priority}.")
  }

  plan
}


#' The row of a repeated DV on the best-ranked sheet, or NA when no rule applies
#'
#' No rule applies when none of the rows' sheets is listed, or when the
#' best-ranked sheet holds more than one of them.
#' @keywords internal
#' @noRd
priority_row <- function(plan, rows, sheet_priority) {
  if (length(sheet_priority) == 0L) {
    return(NA_integer_)
  }

  rank <- match(trimws(plan$sheet_name[rows]), trimws(sheet_priority))

  if (all(is.na(rank)) || sum(rank == min(rank, na.rm = TRUE), na.rm = TRUE) > 1L) {
    return(NA_integer_)
  }

  rows[[which.min(rank)]]
}


#' Keep one row to sign off for each DV defined more than once with the same text
#'
#' The row to sign off is the one already signed off, else one not marked
#' duplicate, else the first. The others are marked duplicate. DVs signed off
#' on several rows in different ways are left for the reviewer.
#' @keywords internal
#' @noRd
settle_duplicate_dvs <- function(plan) {
  for (rows in repeated_dv_rows(plan)) {
    if (!same_spec_text(plan$instructions[rows])) {
      next
    }

    reviewed <- rows[plan$status[rows] %in% "reviewed"]

    if (length(unique(plan_drafts(plan, reviewed))) > 1L) {
      next
    }

    not_duplicate <- rows[!(plan$status[rows] %in% "duplicate")]
    home <- c(reviewed, not_duplicate, rows)[[1L]]
    others <- setdiff(rows, home)

    if (plan$status[[home]] %in% "duplicate") {
      data.table::set(plan, i = home, j = "status", value = "needs_review")
    }

    data.table::set(plan, i = home, j = "notes", value = append_note(
      plan$notes[[home]],
      paste0("also defined on ", describe_plan_rows(plan, others),
             " with the same text; this is the row to sign off (decision D8)")
    ))

    for (index in others) {
      data.table::set(plan, i = index, j = "status", value = "duplicate")
      data.table::set(plan, i = index, j = "reviewed_by", value = NA_character_)
      data.table::set(plan, i = index, j = "reviewed_on", value = NA_character_)
      data.table::set(plan, i = index, j = "notes", value = append_note(
        plan$notes[[index]],
        paste0("same as ", describe_plan_rows(plan, home), ", which is the row to sign off (decision D8)")
      ))
    }
  }

  plan
}


#' Row numbers of each DV that appears on more than one plan row
#' @keywords internal
#' @noRd
repeated_dv_rows <- function(plan) {
  key <- tolower(plan$dv)
  candidate <- !is_blank(plan$dv) & !(plan$status %in% "not_a_derivation")
  repeated <- unique(key[candidate][duplicated(key[candidate])])

  lapply(repeated, function(dv_key) which(key == dv_key & candidate))
}


#' @keywords internal
#' @noRd
same_spec_text <- function(instructions) {
  length(unique(normalise_spec_text(instructions))) == 1L
}


#' @keywords internal
#' @noRd
plan_drafts <- function(plan, rows) {
  paste(plan$verb[rows], plan$args[rows], plan$condition[rows], sep = "\r")
}


#' @keywords internal
#' @noRd
describe_plan_rows <- function(plan, rows) {
  paste(unique(paste0(plan$sheet_name[rows], " row ", plan$excel_row[rows])), collapse = " and ")
}


#' Carry signed-off reviews over from an earlier plan
#' @keywords internal
#' @noRd
carry_over_reviews <- function(plan, previous_plan) {
  previous <- as.data.frame(previous_plan, stringsAsFactors = FALSE)

  needed <- c("dv", "sheet_name", "status", "verb", "args", "condition",
              "reviewed_by", "reviewed_on", "instructions")
  missing_columns <- setdiff(needed, names(previous))

  if (length(missing_columns) > 0L) {
    cli::cli_alert_danger(
      "{.arg previous_plan} is missing column{?s}: {.val {missing_columns}}."
    )
    stop("`previous_plan` is missing required columns.", call. = FALSE)
  }

  plan <- carry_over_missing_codes(plan, previous)

  # A duplicate chosen by the reviewer is kept too, so their choice of row stands.
  previous <- previous[previous$status %in% c("reviewed", "duplicate"), , drop = FALSE]

  if (nrow(previous) == 0L) {
    return(plan)
  }

  new_key <- paste(tolower(plan$dv), plan$sheet_name, sep = "\r")
  old_key <- paste(tolower(clean_dv_name(previous$dv)), previous$sheet_name, sep = "\r")
  position <- match(new_key, old_key)

  for (index in which(!is.na(position))) {
    old <- previous[position[[index]], , drop = FALSE]

    unchanged <- identical(
      normalise_spec_text(old$instructions),
      normalise_spec_text(plan$instructions[[index]])
    )

    if (unchanged) {
      for (column in c("verb", "args", "condition", "status", "reviewed_by", "reviewed_on")) {
        data.table::set(plan, i = index, j = column, value = as.character(old[[column]]))
      }
    } else {
      note <- paste0(
        "spec text changed since it was reviewed by ",
        if (is_blank(old$reviewed_by)) "an unnamed reviewer" else old$reviewed_by,
        if (is_blank(old$reviewed_on)) "" else paste0(" on ", old$reviewed_on)
      )
      data.table::set(plan, i = index, j = "notes",
                      value = append_note(plan$notes[[index]], note, first = TRUE))
    }
  }

  plan
}


#' Carry missing_code over from an earlier plan, on every row, signed off or not
#'
#' A choice about the DV rather than its formula, so it stands whatever the
#' spec text now says.
#' @keywords internal
#' @noRd
carry_over_missing_codes <- function(plan, previous) {
  if (!"missing_code" %in% names(previous)) {
    return(plan)
  }

  previous <- previous[!is_blank(previous$missing_code), , drop = FALSE]
  new_key <- paste(tolower(plan$dv), plan$sheet_name, sep = "\r")
  old_key <- paste(tolower(clean_dv_name(previous$dv)), previous$sheet_name, sep = "\r")
  position <- match(new_key, old_key)

  for (index in which(!is.na(position))) {
    data.table::set(plan, i = index, j = "missing_code", value = as.character(previous$missing_code[[position[[index]]]]))
  }

  plan
}


#' Normalise spec text for comparison
#' @keywords internal
#' @noRd
normalise_spec_text <- function(text) {
  text <- gsub("\r\n?", "\n", text)
  trimws(gsub("[ \t]+\n", "\n", text))
}


#' Add a note to a notes cell
#' @keywords internal
#' @noRd
append_note <- function(notes, note, first = FALSE) {
  if (is_blank(notes)) {
    return(note)
  }

  if (first) paste(note, notes, sep = "; ") else paste(notes, note, sep = "; ")
}


#' Write plan arguments as R code
#' @keywords internal
#' @noRd
format_plan_args <- function(args) {
  if (length(args) == 0L) {
    return("")
  }

  parts <- vapply(
    names(args),
    FUN = function(name) paste(name, "=", format_plan_value(args[[name]])),
    FUN.VALUE = character(1L),
    USE.NAMES = FALSE
  )

  paste(parts, collapse = ", ")
}


#' Write one argument value as R code
#' @keywords internal
#' @noRd
format_plan_value <- function(value) {
  if (is.null(value) || length(value) == 0L) {
    return("NULL")
  }

  text <- if (is.character(value)) {
    encodeString(value, quote = "\"")
  } else if (is.numeric(value)) {
    format_number(value)
  } else {
    ifelse(is.na(value), "NA", as.character(value))
  }

  if (length(text) == 1L) text else paste0("c(", paste(text, collapse = ", "), ")")
}


#' @title write_dv_plan
#'
#' @description
#'
#' Writes a DV plan to a CSV for review. Open it in Excel, work down the rows,
#' and for each one either sign it off — set `status` to `reviewed` and fill in
#' `reviewed_by` and `reviewed_on` — or correct `verb`, `args` or `condition`
#' first. Multi-line spec text is kept intact in the `instructions` column.
#'
#' @param plan A plan from [build_dv_plan()].
#' @param path Where to write the CSV.
#'
#' @return `path`, invisibly.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' write_dv_plan(plan, "dv_plan.csv")
#' }
write_dv_plan <- function(plan, path) {
  check_plan_columns(plan)

  if (!is.character(path) || length(path) != 1L || is.na(path)) {
    cli::cli_alert_danger("{.arg path} must be a single file path.")
    stop("`path` must be a single file path.", call. = FALSE)
  }

  data.table::fwrite(plan, path, na = "")
  cli::cli_alert_success("Plan written to {.file {path}}. Open it in Excel to review and sign off each row.")

  invisible(path)
}


#' @title read_dv_plan
#'
#' @description
#'
#' Reads a reviewed DV plan back from CSV, keeping every column as text so that
#' Excel's habit of reformatting values cannot change an argument, and checks
#' that it still has the plan columns and only recognised statuses.
#'
#' @param path Path to a plan CSV written by [write_dv_plan()].
#'
#' @return A data.table.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' plan <- read_dv_plan("dv_plan.csv")
#' }
read_dv_plan <- function(path) {
  if (!is.character(path) || length(path) != 1L || !file.exists(path)) {
    cli::cli_alert_danger("Cannot find the plan file {.file {path}}.")
    stop("Cannot find the plan file.", call. = FALSE)
  }

  # read.csv, not fread: fread leaves the doubled quotes in "value_col = ""x""".
  plan <- data.table::as.data.table(utils::read.csv(
    path, colClasses = "character", na.strings = "", encoding = "UTF-8",
    check.names = FALSE, stringsAsFactors = FALSE
  ))

  check_plan_columns(plan)

  if (!"uid" %in% names(plan)) {
    plan[, uid := paste0("legacy_", sprintf("%06d", seq_len(.N)))]
  }

  if (!"missing_code" %in% names(plan)) {
    plan[, missing_code := NA_character_]
  }

  plan[, excel_row := as.integer(excel_row)]
  cli::cli_alert_success("Read {nrow(plan)} plan row{?s} from {.file {path}}.")
  report_review_progress(plan)

  plan[]
}


#' Say how far review has got
#' @keywords internal
#' @noRd
report_review_progress <- function(plan) {
  count_of <- function(statuses) sum(plan$status %in% statuses)
  to_sign_off <- count_of(c("auto", "needs_review", "hand_written"))

  cli::cli_bullets(c(
    " " = "{count_of('reviewed')} signed off",
    " " = "{to_sign_off} still to sign off",
    " " = "{count_of('duplicate')} duplicate and {count_of('not_a_derivation')} not_a_derivation, which need no sign-off"
  ))

  unnamed <- sum(plan$status %in% "reviewed" & is_blank(plan$reviewed_by))

  if (unnamed > 0L) {
    cli::cli_alert_warning(
      "{unnamed} row{?s} {?is/are} marked reviewed with no {.field reviewed_by}. If {?it was/they were} marked only to try the DVs out, use {.code trial_run()} instead, which needs no sign-off, and set {?it/them} back."
    )
  }

  signed_non_derivations <- plan$dv[signed_off_non_derivations(plan)]

  if (length(signed_non_derivations) > 0L) {
    cli::cli_alert_warning(
      "{length(signed_non_derivations)} row{?s} marked reviewed {?has/have} notes saying {?it is/they are} not a derivation (value labels, or built elsewhere): {.val {utils::head(signed_non_derivations, 10)}}{if (length(signed_non_derivations) > 10) ', ...' else ''}. {cli::qty(length(signed_non_derivations))}Set {?its/their} status back to not_a_derivation, or the suite gets a step to write for {?it/each}."
    )
  }

  signed_twice <- repeated_dv_rows(plan[plan$status %in% "reviewed"])

  if (length(signed_twice) > 0L) {
    reviewed <- plan[plan$status %in% "reviewed"]
    names_twice <- vapply(signed_twice, function(rows) reviewed$dv[[rows[[1L]]]], character(1L))
    cli::cli_alert_warning(
      "{length(names_twice)} DV{?s} {?is/are} signed off on more than one row: {.val {utils::head(names_twice, 10)}}{if (length(names_twice) > 10) ', ...' else ''}. Keep one row signed off and set the rest to duplicate."
    )
  }

  invisible(NULL)
}


#' Rows signed off with no verb whose notes say they are not a derivation
#'
#' Usually a sign-off by mistake, such as marking every row reviewed at once.
#' @keywords internal
#' @noRd
signed_off_non_derivations <- function(plan) {
  plan$status %in% "reviewed" & is_blank(plan$verb) &
    grepl("value labels, not a derivation|derived upstream or in another spec", plan$notes)
}


#' The columns a plan must have
#' @keywords internal
#' @noRd
plan_columns <- function() {
  c("uid", "dv", "level", "verb", "inputs", "args", "condition", "missing_code", "status",
    "reviewed_by", "reviewed_on", "notes", "file_name", "sheet_name",
    "excel_row", "label", "instructions")
}


#' Plan columns added after the first plans were written, so not required
#' @keywords internal
#' @noRd
optional_plan_columns <- function() {
  "missing_code"
}


#' The statuses a plan row may have
#' @keywords internal
#' @noRd
plan_statuses <- function() {
  c("auto", "needs_review", "hand_written", "not_a_derivation", "duplicate", "reviewed")
}


#' Check a plan's columns and statuses
#' @keywords internal
#' @noRd
check_plan_columns <- function(plan) {
  if (!is.data.frame(plan)) {
    cli::cli_alert_danger("The plan must be a data frame.")
    stop("The plan must be a data frame.", call. = FALSE)
  }

  missing_columns <- setdiff(plan_columns(), c(names(plan), optional_plan_columns()))

  if (length(missing_columns) > 0L) {
    cli::cli_alert_danger("The plan is missing column{?s}: {.val {missing_columns}}.")
    stop("The plan is missing required columns.", call. = FALSE)
  }

  unknown_status <- setdiff(unique(plan$status[!is.na(plan$status)]), plan_statuses())

  if (length(unknown_status) > 0L) {
    cli::cli_alert_danger(
      "Unrecognised status in the plan: {.val {unknown_status}}. Use one of {.val {plan_statuses()}}."
    )
    stop("The plan has unrecognised statuses.", call. = FALSE)
  }

  invisible(NULL)
}
