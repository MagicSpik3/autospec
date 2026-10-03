#' Write the DV suite: one R file of steps per topic
#'
#' Turns a DV plan into R code you can read, edit and run. Each spec becomes a
#' topic file (`financial_wealth.R`, `income.R`, ...) holding one [dv_step()]
#' per DV: its label, the columns it reads, and a `derive` function that
#' builds it by calling a verb. DVs no verb covers, and rows not yet signed
#' off, are written as steps with `derive = NULL` and the spec text above them,
#' ready to be written by hand.
#'
#' Existing files are never overwritten unless `overwrite = TRUE`, so edits
#' made after feedback are safe. To pick up rows signed off since, use
#' [sync_dv_suite()], which fills in only the steps not written yet.
#'
#' @param plan A plan from [build_dv_plan()] or [read_dv_plan()].
#' @param suite_dir Folder to write the suite into.
#' @param data_names Column names of the input data. Every name the data has,
#'   in any case, is written in the data's spelling.
#' @param reviewed_only When `TRUE`, only rows signed off in the plan get a
#'   working `derive`; the drafted call for any other row is written as a
#'   comment. When `FALSE`, every drafted call is written as working code.
#' @param name_case How DVs not already in the data are spelt: `"auto"` follows
#'   the data (lower case, upper case, or the spec's spelling if the data mixes
#'   cases); `"lower"`, `"upper"` or `"spec"` choose one.
#' @param overwrite Whether to replace files that already exist.
#' @param topics Optional topic names to write, such as `"property_wealth"`;
#'   every topic when `NULL`.
#' @param strict_duplicates Whether a DV signed off on more than one row, in
#'   different ways, stops the write. When `FALSE` the first signed-off row is
#'   used and the others are listed, which suits a trial run.
#'
#' @return The paths of the files written, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' write_dv_suite(read_dv_plan("dv_plan.csv"), "dv_suite")
#' }
write_dv_suite <- function(plan, suite_dir = "dv_suite", data_names = NULL,
                           reviewed_only = TRUE, name_case = "auto",
                           overwrite = FALSE, topics = NULL,
                           strict_duplicates = TRUE) {
  written_paths <- write_suite_folder(
    plan, suite_dir, data_names = data_names, reviewed_only = reviewed_only,
    name_case = name_case, overwrite = overwrite, topics = topics,
    strict_duplicates = strict_duplicates
  )

  cli::cli_h2("Next steps")
  cli::cli_ol(c(
    "Open each topic file and write the steps marked {.code derive = NULL}.",
    "Set the pension discount rate in {.file settings.R}.",
    "Run {.code check_dv_suite(\"{suite_dir}\")} to catch mistakes before touching the data.",
    "Fill in the example tests in {.file tests/} and run {.code test_dv_suite(\"{suite_dir}\")}.",
    "Build the DVs with {.code run_dv_suite(df, \"{suite_dir}\")}.",
    "After signing off more rows in the plan, run {.code sync_dv_suite()} to write them in."
  ))

  invisible(written_paths)
}


#' Write the suite files; write_dv_suite() without the closing advice
#' @keywords internal
#' @noRd
write_suite_folder <- function(plan, suite_dir, data_names = NULL, reviewed_only = TRUE,
                               name_case = "auto", overwrite = FALSE, topics = NULL,
                               strict_duplicates = TRUE) {
  check_suite_dir(suite_dir)
  cli::cli_h1("Writing the DV suite to {.file {suite_dir}}")

  prepared <- prepare_suite_plan(plan, data_names, name_case, topics, strict_duplicates)
  plan <- prepared$plan

  dir.create(file.path(suite_dir, "tests"), recursive = TRUE, showWarnings = FALSE)

  written_paths <- character()
  all_steps <- list()

  written_paths <- c(written_paths, write_suite_file(
    settings_file_lines(household_id = guess_household_id(data_names)), file.path(suite_dir, "settings.R"), overwrite
  ))

  written_paths <- c(written_paths, write_suite_file(
    test_helper_lines(), file.path(suite_dir, "tests", "helper-suite.R"), overwrite
  ))

  for (topic in unique(plan$topic)) {
    topic_rows <- plan[plan$topic == topic, , drop = FALSE]
    steps <- topic_steps(topic_rows, prepared, reviewed_only, data_names)

    all_steps <- c(all_steps, steps)
    working <- sum(vapply(steps, `[[`, logical(1L), "written"))

    topic_lines <- c(
      topic_header_lines(topic, topic_rows, suite_dir),
      unlist(lapply(steps, `[[`, "lines"), use.names = FALSE)
    )

    path <- write_suite_file(topic_lines, file.path(suite_dir, paste0(topic, ".R")), overwrite)

    if (length(path) > 0L) {
      cli::cli_alert_success(
        "{.file {basename(path)}}: {length(steps)} step{?s}, {working} ready to run, {length(steps) - working} not written yet."
      )
    }

    stubs <- Filter(function(step) !step$written, steps)

    if (length(stubs) > 0L) {
      written_paths <- c(written_paths, path, write_suite_file(
        test_stub_lines(topic, stubs),
        file.path(suite_dir, "tests", paste0("test-", topic, ".R")),
        overwrite
      ))
    } else {
      written_paths <- c(written_paths, path)
    }
  }

  report_dvs_in_data(all_steps, data_names)

  invisible(written_paths)
}


#' @keywords internal
#' @noRd
check_suite_dir <- function(suite_dir) {
  if (!is.character(suite_dir) || length(suite_dir) != 1L || is.na(suite_dir)) {
    cli::cli_alert_danger("{.arg suite_dir} must be a single folder path.")
    stop("`suite_dir` must be a single folder path.", call. = FALSE)
  }

  invisible(NULL)
}


#' Check a plan and get it ready to write as steps
#'
#' Returns the plan with a `topic` column and one row per DV, cut to the chosen
#' topics, with the verb registry and the function that spells names.
#' @keywords internal
#' @noRd
prepare_suite_plan <- function(plan, data_names, name_case, topics = NULL,
                               strict_duplicates = TRUE) {
  check_plan_columns(plan)

  if (!is.null(data_names) && !is.character(data_names)) {
    cli::cli_alert_danger("{.arg data_names} must be a character vector of column names.")
    stop("`data_names` must be a character vector.", call. = FALSE)
  }

  check_name_case(name_case)

  if (!is.null(data_names)) {
    stop_if_case_duplicates(data_names)
  }

  plan <- as.data.frame(plan, stringsAsFactors = FALSE)
  plan$dv <- clean_dv_name(plan$dv)
  plan$topic <- suite_topic(plan$file_name)

  check_plan_topics(topics, unique(plan$topic))

  left_out <- plan$status %in% c("not_a_derivation", "duplicate") | is_blank(plan$dv)
  chosen_case <- resolve_name_case(name_case, data_names)
  spell <- name_speller(data_names, chosen_case, expand_variable_names(plan$dv[!left_out]))

  if (length(topics) > 0L) {
    plan <- plan[plan$topic %in% topics, , drop = FALSE]
    left_out <- plan$status %in% c("not_a_derivation", "duplicate") | is_blank(plan$dv)
    cli::cli_alert_info("Topic{?s} chosen: {.val {topics}}.")
  }

  cli::cli_alert_info(
    "{sum(!left_out)} plan row{?s} to write; {sum(left_out)} left out (marked not_a_derivation or duplicate, or with no DV name)."
  )

  report_name_case(name_case, chosen_case, data_names)

  list(
    plan = choose_one_row_per_dv(plan, strict = strict_duplicates),
    spell = spell,
    registry = verb_arguments()
  )
}


#' Stop when chosen topics are not in the plan
#' @keywords internal
#' @noRd
check_plan_topics <- function(topics, available) {
  if (is.null(topics)) {
    return(invisible(NULL))
  }

  if (!is.character(topics)) {
    cli::cli_alert_danger("{.arg topics} must be topic names in quotes, such as {.val property_wealth}.")
    stop("`topics` must be a character vector.", call. = FALSE)
  }

  unknown <- setdiff(topics, available)

  if (length(unknown) > 0L) {
    cli::cli_alert_danger("No topic called {.val {unknown}} in the plan. The topics are {.val {available}}.")
    stop("Unknown topic requested.", call. = FALSE)
  }

  invisible(NULL)
}


#' Build every step of one topic's plan rows
#' @keywords internal
#' @noRd
topic_steps <- function(topic_rows, prepared, reviewed_only, data_names) {
  to_write <- topic_rows[!(topic_rows$status %in% c("not_a_derivation", "duplicate") | is_blank(topic_rows$dv)), , drop = FALSE]

  unlist(
    lapply(seq_len(nrow(to_write)), function(index) {
      suite_steps_for_row(to_write[index, , drop = FALSE], prepared$registry, prepared$spell, reviewed_only, data_names)
    }),
    recursive = FALSE
  )
}


#' The data's household identifier column
#'
#' The name usually carries the round, as `hhserialr9`, so an exact match is
#' tried first, then any column whose name starts with it. Without the data's
#' names there is nothing to go on, so the plain name is written for someone
#' to correct in `settings.R`.
#' @keywords internal
#' @noRd
guess_household_id <- function(data_names, stem = "hhserial") {
  if (is.null(data_names)) {
    return(stem)
  }

  exact <- data_names[tolower(data_names) == tolower(stem)]

  if (length(exact) > 0L) {
    return(exact[[1L]])
  }

  starting <- data_names[startsWith(tolower(data_names), tolower(stem))]

  if (length(starting) > 0L) {
    return(starting[[1L]])
  }

  stem
}


#' Say which DVs the data already has, especially those with nothing written
#' @keywords internal
#' @noRd
report_dvs_in_data <- function(steps, data_names) {
  if (is.null(data_names) || length(steps) == 0L) {
    return(invisible(NULL))
  }

  in_data <- vapply(steps, `[[`, logical(1L), "in_data")
  written <- vapply(steps, `[[`, logical(1L), "written")
  unwritten_in_data <- vapply(steps[in_data & !written], `[[`, character(1L), "dv")

  cli::cli_h2("DVs already in the data")

  if (!any(in_data)) {
    cli::cli_alert_info("None of the DVs are in the data yet.")
    return(invisible(NULL))
  }

  cli::cli_alert_info("{sum(in_data)} of {length(steps)} DV{?s} {?is/are} already in the data; running the suite rebuilds the ones that are written.")

  if (length(unwritten_in_data) > 0L) {
    cli::cli_alert_warning(
      "{length(unwritten_in_data)} of them {?has/have} nothing written yet: {.val {utils::head(unwritten_in_data, 10)}}{if (length(unwritten_in_data) > 10) ', ...' else ''}. {cli::qty(length(unwritten_in_data))}If something earlier builds {?it/them} (imputation, or code written before), mark {?it/them} not_a_derivation in the plan; the step comments say which."
    )
  }

  invisible(NULL)
}


#' Topic name for a spec file: Financial_Wealth_DV_Spec_R9.xlsx -> financial_wealth
#' @keywords internal
#' @noRd
suite_topic <- function(file_name) {
  topic <- tolower(sub("_DV_Spec.*$", "", basename(file_name), ignore.case = TRUE))
  topic <- gsub("[^a-z0-9]+", "_", topic)
  topic <- gsub("^_+|_+$", "", topic)

  ifelse(is.na(file_name) | !nzchar(topic), "unknown_topic", topic)
}


#' Keep one plan row per DV: the signed-off one, or the first
#'
#' A DV signed off on several rows in different ways stops, unless `strict` is
#' FALSE, when the first signed-off row is used.
#' @keywords internal
#' @noRd
choose_one_row_per_dv <- function(plan, strict = TRUE) {
  key <- tolower(plan$dv)
  candidate <- !(plan$status %in% c("not_a_derivation", "duplicate")) & !is_blank(plan$dv)
  keep <- rep(TRUE, nrow(plan))
  candidate_keys <- key[candidate]
  chosen_rows <- integer()
  conflicts <- character()

  for (dv_key in unique(candidate_keys[duplicated(candidate_keys)])) {
    rows <- which(key == dv_key & candidate)
    reviewed <- rows[plan$status[rows] %in% "reviewed"]

    # Signed off more than once is fine when every sign-off would write the same code.
    if (length(unique(plan_drafts(plan, reviewed))) > 1L) {
      conflicts <- c(conflicts, paste0(plan$dv[reviewed[[1L]]], ": ", describe_plan_rows(plan, reviewed)))

      if (strict) {
        next
      }
    }

    chosen <- c(reviewed, rows)[[1L]]
    keep[setdiff(rows, chosen)] <- FALSE
    chosen_rows <- c(chosen_rows, chosen)
  }

  if (length(conflicts) > 0L && !strict) {
    cli::cli_alert_warning(
      "{length(conflicts)} DV{?s} {?is/are} signed off on more than one plan row in different ways; the first row is used for now. Settle {?it/them} before writing the real suite:"
    )
    cli::cli_bullets(stats::setNames(conflicts, rep("*", length(conflicts))))
  } else if (length(conflicts) > 0L) {
    cli::cli_alert_danger(
      "{length(conflicts)} DV{?s} {?is/are} signed off on more than one plan row, with a different verb, args or condition. For each, keep the right row signed off and set the others' status to duplicate:"
    )
    cli::cli_bullets(stats::setNames(conflicts, rep("*", length(conflicts))))
    stop("DVs are signed off more than once in the plan, in different ways.", call. = FALSE)
  }

  if (length(chosen_rows) > 0L) {
    cli::cli_alert_info(
      "{length(chosen_rows)} DV{?s} {?is/are} on more than one plan row; each is written once, from its signed-off row or else its first. Mark the other rows duplicate in the plan to make this clear."
    )
  }

  plan[keep, , drop = FALSE]
}


#' Build the step code for one plan row, one step per index of a numbered range
#' @keywords internal
#' @noRd
suite_steps_for_row <- function(row, registry, spell, reviewed_only, data_names = NULL) {
  verb <- if (is_blank(row$verb)) NA_character_ else trimws(row$verb)
  is_verb <- !is.na(verb) && verb %in% registered_verbs()
  verb_spec <- registry[registry$verb %in% verb, , drop = FALSE]
  problems <- character()

  args <- tryCatch(parse_plan_args(row$args), error = function(problem) {
    problems <<- c(problems, paste0("args could not be read: ", conditionMessage(problem)))
    list()
  })

  if (is_verb) {
    accepted <- verb_spec$argument[verb_spec$where_it_is_set == "plan args"]
    unexpected <- setdiff(names(args), accepted)

    if (length(unexpected) > 0L) {
      problems <- c(problems, paste0(
        "args has ", paste(unexpected, collapse = ", "), ", which ", verb,
        " does not take per DV (settings go in settings.R)"
      ))
    }

    missing_args <- setdiff(
      verb_spec$argument[verb_spec$where_it_is_set == "plan args" & verb_spec$must_be_given == "yes"],
      names(args)
    )

    if (length(missing_args) > 0L) {
      problems <- c(problems, paste0("args has no value for ", paste(missing_args, collapse = ", ")))
    }

    if (any(verb_spec$where_it_is_set == "plan condition") && is_blank(row$condition)) {
      problems <- c(problems, "no condition drafted")
    }
  }

  # A range written twice, as in X(1-3)_Y(1-3), runs in parallel; different
  # ranges, as in M(1-12)_interest(1-3), give one step per combination.
  ranges <- find_number_ranges(row$dv)
  ranges <- ranges[!duplicated(ranges$match), , drop = FALSE]
  expandable <- nrow(ranges) > 0L && all(ranges$lower <= ranges$upper)

  combinations <- if (expandable) {
    index_values <- lapply(seq_len(nrow(ranges)), function(position) {
      seq.int(ranges$lower[[position]], ranges$upper[[position]])
    })
    rev(expand.grid(rev(index_values), KEEP.OUT.ATTRS = FALSE))
  } else {
    data.frame(unexpanded = NA_integer_)
  }

  lapply(seq_len(nrow(combinations)), function(combination) {
    numbers <- if (expandable) unlist(combinations[combination, ], use.names = FALSE) else integer()

    substitute_index <- function(value) {
      if (!is.character(value)) {
        return(value)
      }

      for (position in seq_along(numbers)) {
        value <- replace_number_range(value, ranges$match[[position]], numbers[[position]])
      }

      value
    }

    column_args <- intersect(
      names(args),
      verb_spec$argument[verb_spec$what_to_give %in% c("one column name", "several column names")]
    )

    dv <- spell(substitute_index(row$dv))
    step_args <- lapply(args, substitute_index)
    condition_text <- if (is_blank(row$condition)) NA_character_ else substitute_index(row$condition)
    step_problems <- problems

    for (arg in column_args) {
      if (is.character(step_args[[arg]]) && !anyNA(step_args[[arg]])) {
        step_args[[arg]] <- spell(expand_variable_names(step_args[[arg]]))
      }
    }

    condition_code <- NULL

    if (!is.na(condition_text)) {
      condition_code <- tryCatch(
        columns_to_df(str2lang(condition_text), spell = spell),
        error = function(problem) {
          step_problems <<- c(step_problems, paste0("condition could not be read: ", condition_text))
          NULL
        }
      )
    }

    inputs <- spell(expand_variable_names(substitute_index(split_plan_list(row$inputs))))
    column_values <- as.character(unlist(step_args[column_args], use.names = FALSE))
    condition_columns <- if (is.null(condition_code)) character() else code_column_names(condition_code)
    inputs <- setdiff(unique(c(inputs, column_values, condition_columns)), dv)

    call_lines <- if (is_verb && length(step_problems) == 0L) {
      verb_call_lines(verb, verb_spec, step_args, condition_code, dv)
    } else {
      character()
    }

    signed_off <- row$status %in% "reviewed"
    written <- length(call_lines) > 0L && (signed_off || !reviewed_only)

    reason <- if (written) {
      NA_character_
    } else if (length(call_lines) > 0L) {
      paste0("not signed off in the plan (", row$status, "). Drafted call:")
    } else if (!is_verb && !is.na(verb)) {
      paste0("the plan names ", verb, ", which is not a verb in the registry")
    } else if (length(step_problems) > 0L) {
      paste(step_problems, collapse = "; ")
    } else {
      "no verb fits this derivation"
    }

    list(
      dv = dv,
      uid = if ("uid" %in% names(row) && !is.null(row$uid) && !is.na(row$uid)) as.character(row$uid) else NA_character_,
      label = substitute_index(row$label),
      inputs = inputs,
      written = written,
      missing_code = plan_missing_code(row),
      in_data = tolower(dv) %in% tolower(data_names),
      lines = step_lines(dv, row, substitute_index(row$label), inputs, call_lines, written, reason,
                         in_data = tolower(dv) %in% tolower(data_names))
    )
  })
}


#' Lines for one dv_step() in a topic file
#' @keywords internal
#' @noRd
step_lines <- function(dv, row, label, inputs, call_lines, written, reason, in_data = FALSE) {
  spec_lines <- strsplit(normalise_spec_text(row$instructions), "\n", fixed = TRUE)[[1L]]

  heading <- paste0("# ", dv, " ")
  heading <- paste0(heading, strrep("-", max(4L, 78L - nchar(heading))))

  comment_lines <- c(
    "",
    heading,
    paste0("# ", row$file_name, ", sheet ", row$sheet_name, ", row ", row$excel_row),
    paste0("#   ", spec_lines)
  )

  if (!written) {
    comment_lines <- c(comment_lines, paste0("# Not written yet: ", reason))

    if (in_data) {
      comment_lines <- c(
        comment_lines,
        "# Already in the data: if something earlier builds it (imputation, or code written before),",
        "# mark it not_a_derivation in the plan. Until written, runs leave the data's column as it is."
      )
    }

    if (length(call_lines) > 0L) {
      comment_lines <- c(comment_lines, paste0("#   ", call_lines))
    }
  }

  if (!is_blank(row$notes) && !(row$status %in% "reviewed")) {
    comment_lines <- c(comment_lines, paste0("# Plan notes: ", row$notes))
  }

  missing_code <- plan_missing_code(row)
  uid <- if ("uid" %in% names(row) && !is.null(row$uid) && !is.na(row$uid)) {
    encodeString(as.character(row$uid), quote = "\"")
  } else {
    NULL
  }

  has_more_args <- !is.na(missing_code) || !is.null(uid)
  ending <- if (has_more_args) "," else ""

  derive_lines <- if (written) {
    c("  derive = function(df) {", paste0("    ", call_lines), paste0("  }", ending))
  } else {
    paste0("  derive = NULL", ending, " # replace NULL with function(df) { ... } that returns df with the DV added")
  }

  uid_line <- if (!is.null(uid)) {
    paste0("  uid = ", uid, if (!is.na(missing_code)) "," else "")
  } else {
    NULL
  }

  missing_line <- if (!is.na(missing_code)) {
    paste0("  missing_code = ", format_number(missing_code),
           " # -8 and -9 inputs count as missing; missing in the DV is written as ", format_number(missing_code))
  } else {
    NULL
  }

  c(
    comment_lines,
    paste0("steps$", deparse_name(dv), " <- dv_step("),
    paste0("  label = ", if (is_blank(label)) "NA" else encodeString(label, quote = "\""), ","),
    argument_lines("inputs", inputs, indent = "  ", last = FALSE),
    derive_lines,
    uid_line,
    missing_line,
    ")"
  )
}


#' The plan row's missing_code as a number, NA when blank; stops when it is not a number
#' @keywords internal
#' @noRd
plan_missing_code <- function(row) {
  if (!"missing_code" %in% names(row) || is_blank(row$missing_code)) {
    return(NA_real_)
  }

  code <- suppressWarnings(as.numeric(row$missing_code))

  if (is.na(code)) {
    cli::cli_alert_danger("{row$dv}: missing_code in the plan is {.val {row$missing_code}}; it must be a number such as -9, or blank.")
    stop("A missing_code in the plan is not a number.", call. = FALSE)
  }

  code
}


#' Lines for a call to a registered verb, one argument per line
#' @keywords internal
#' @noRd
verb_call_lines <- function(verb, verb_spec, args, condition_code, dv) {
  call_body <- character()

  for (argument in names(verb_formals(verb))) {
    where <- verb_spec$where_it_is_set[verb_spec$argument == argument]
    setting <- verb_spec$settings_name[verb_spec$argument == argument]

    call_body <- c(call_body, if (argument == "df") {
      "df,"
    } else if (argument == "new_col") {
      paste0("new_col = ", encodeString(dv, quote = "\""), ",")
    } else if (identical(where, "plan condition")) {
      paste0("condition = ", paste(deparse(condition_code, width.cutoff = 500L), collapse = " "), ",")
    } else if (identical(where, "settings.R")) {
      paste0(argument, " = settings$", setting, ",")
    } else if (argument %in% names(args)) {
      argument_lines(argument, args[[argument]], indent = "", last = FALSE)
    } else if (nzchar(formal_default(verb, argument))) {
      paste0(argument, " = ", formal_default(verb, argument), ",")
    })
  }

  call_body[length(call_body)] <- sub(",$", "", call_body[length(call_body)])

  c(paste0(verb, "("), paste0("  ", call_body), ")")
}


#' Lines for `name = value,`, spread over several lines for long vectors
#' @keywords internal
#' @noRd
argument_lines <- function(name, value, indent, last) {
  ending <- if (last) "" else ","

  if (is.character(value) && length(value) > 3L) {
    items <- encodeString(value, quote = "\"")
    return(c(
      paste0(indent, name, " = c("),
      paste0(indent, "  ", items, c(rep(",", length(items) - 1L), "")),
      paste0(indent, ")", ending)
    ))
  }

  text <- if (is.character(value) && length(value) == 0L) "character()" else format_plan_value(value)

  paste0(indent, name, " = ", text, ending)
}


#' Write a list element name, backticked when it is not syntactic
#' @keywords internal
#' @noRd
deparse_name <- function(name) {
  if (make.names(name) == name) name else paste0("`", name, "`")
}


#' Say how names will be spelt
#' @keywords internal
#' @noRd
report_name_case <- function(name_case, chosen_case, data_names) {
  style <- c(lower = "in lower case", upper = "in upper case", spec = "as the spec spells them")[[chosen_case]]

  if (name_case != "auto") {
    cli::cli_alert_info("New DVs are named {style} ({.code name_case = \"{name_case}\"}).")
  } else if (is.null(data_names)) {
    cli::cli_alert_warning(
      "No {.arg data_names} given, so new DVs are named {style}. Pass {.code data_names = names(your_data)} to follow the data's naming."
    )
  } else if (chosen_case == "spec") {
    cli::cli_alert_info("Column names in the data mix upper and lower case, so new DVs are named {style}.")
  } else {
    cli::cli_alert_info("Column names in the data are {sub('^in ', '', style)}, so new DVs are named {style}.")
  }

  if (!is.null(data_names)) {
    cli::cli_alert_info("Inputs, and DVs already in the data, are spelt exactly as the data spells them.")
  }

  invisible(NULL)
}


#' Use the known spelling of each name where it differs only in case
#' @keywords internal
#' @noRd
spell_as_known <- function(names, known_names) {
  if (length(names) == 0L) {
    return(character())
  }

  resolution <- resolve_variable_names(names, known_names)
  resolution$resolved
}


#' Rewrite a condition so column names read from the data: `x > 0` -> `df$x > 0`
#' @keywords internal
#' @noRd
columns_to_df <- function(code, known_names = character(),
                          spell = function(names) spell_as_known(names, known_names)) {
  if (is.symbol(code)) {
    name <- as.character(code)

    if (!nzchar(name) || name %in% c("T", "F", "pi")) {
      return(code)
    }

    return(call("$", as.name("df"), as.name(spell(name))))
  }

  if (is.call(code)) {
    parts <- as.list(code)

    if (length(parts) > 1L) {
      parts[-1L] <- lapply(parts[-1L], columns_to_df, spell = spell)
    }

    return(as.call(parts))
  }

  code
}


#' Column names read as `df$name` or `df[["name"]]` in some code
#' @keywords internal
#' @noRd
code_column_names <- function(code) {
  if (!is.call(code)) {
    return(character())
  }

  call_head <- code[[1L]]
  here <- character()

  if (is.symbol(call_head) && as.character(call_head) %in% c("$", "[[") && length(code) == 3L &&
      identical(code[[2L]], as.name("df"))) {
    field <- code[[3L]]
    here <- if (is.symbol(field)) as.character(field) else if (is.character(field)) field else character()
  }

  c(here, unlist(lapply(as.list(code)[-1L], code_column_names), use.names = FALSE))
}


#' Read a plan's args cell: only values, so the CSV cannot run code
#' @keywords internal
#' @noRd
parse_plan_args <- function(text) {
  if (is_blank(text)) {
    return(list())
  }

  parsed_args <- tryCatch(
    str2lang(paste0("list(", text, ")")),
    error = function(problem) stop("cannot read `", text, "` as R arguments", call. = FALSE)
  )

  check_literal_expression(parsed_args)

  values <- eval(parsed_args, envir = baseenv())

  if (length(values) > 0L && (is.null(names(values)) || any(!nzchar(names(values))))) {
    stop("every argument must be named, as in `value_col = \"x\"`", call. = FALSE)
  }

  values
}


#' @keywords internal
#' @noRd
check_literal_expression <- function(code) {
  if (is.call(code)) {
    function_name <- deparse(code[[1L]])

    if (!function_name %in% c("list", "c", "-", "+")) {
      stop("args may contain only values, not `", function_name, "()`", call. = FALSE)
    }

    for (part in as.list(code)[-1L]) {
      check_literal_expression(part)
    }
  } else if (is.name(code)) {
    if (!as.character(code) %in% c("NA_real_", "NA_integer_", "NA_character_", "Inf")) {
      stop("args may contain only values; `", as.character(code),
           "` looks like a variable. Put column names in quotes", call. = FALSE)
    }
  }

  invisible(NULL)
}


#' Split a "a; b; c" plan cell
#' @keywords internal
#' @noRd
split_plan_list <- function(text) {
  if (is_blank(text)) {
    return(character())
  }

  parts <- trimws(strsplit(text, ";", fixed = TRUE)[[1L]])
  parts[nzchar(parts)]
}


#' Write lines to a file unless it exists; returns the path when written
#' @keywords internal
#' @noRd
write_suite_file <- function(lines, path, overwrite) {
  if (file.exists(path) && !overwrite) {
    cli::cli_alert_warning(
      "{.file {path}} already exists and was left as it is. Use {.code overwrite = TRUE} to replace it."
    )
    return(character())
  }

  connection <- file(path, open = "w", encoding = "UTF-8")
  on.exit(close(connection))
  writeLines(lines, connection)

  path
}


#' @keywords internal
#' @noRd
topic_header_lines <- function(topic, topic_rows, suite_dir) {
  elsewhere <- topic_rows[topic_rows$status %in% "not_a_derivation", , drop = FALSE]
  duplicates <- topic_rows[topic_rows$status %in% "duplicate", , drop = FALSE]

  c(
    paste0("# ", topic, " DVs"),
    paste0("# Written by autospec::write_dv_suite() on ", format(Sys.Date()), "."),
    "# Each step builds one DV. Edit freely: this file is not overwritten.",
    paste0("# Run just this topic with run_dv_suite(df, \"", suite_dir, "\", topics = \"", topic, "\")."),
    if (nrow(elsewhere) > 0L) {
      c(
        "#",
        "# In the spec but not built here (marked not_a_derivation in the plan):",
        paste0("#   ", elsewhere$dv, ": ",
               substr(gsub("\\s+", " ", elsewhere$instructions), 1L, 70L))
      )
    },
    if (nrow(duplicates) > 0L) {
      c(
        "#",
        "# Defined again here but built from another row of the spec (marked duplicate in the plan):",
        paste0("#   ", duplicates$dv, " (", duplicates$sheet_name, " row ", duplicates$excel_row, ")")
      )
    },
    "",
    "steps <- list()"
  )
}


#' The settings.R template: every verb argument set in settings.R
#' @keywords internal
#' @noRd
settings_file_lines <- function(household_id = "hhserial") {
  c(
    "# Settings used by every step. Change a convention here, once, not per DV.",
    "",
    "settings <- list(",
    "  # Household identifier: groups people into households for the household totals and flags.",
    "  # Person-level DVs are worked out row by row and do not use it.",
    paste0("  by = ", encodeString(household_id, quote = "\""), ","),
    "",
    "  # Count a missing value as zero when adding up (decision D3)",
    "  na_as_zero = TRUE,",
    "",
    "  # Sentinel codes dv_clear_sentinels() replaces, and the value put in their place (decision D1)",
    "  codes = c(-8, -9),",
    "  replacement = 0,",
    "",
    "  # What a DV holds where its input is -8/-9 (decision D1): \"zero\" puts 0, \"missing\" puts NA,",
    "  # \"stop\" refuses to run. Copies, band midpoints, flags, kept values and totals use this.",
    "  # The published Round 8 DVs hold 0 there. A step can override it, e.g. sentinels = \"keep\"",
    "  # in a dv_copy() whose published DV keeps -9.",
    "  sentinels = \"zero\",",
    "",
    "  # The same for differences, ratios and present values. The published Round 8 DVs leave",
    "  # these missing where an input is -8/-9.",
    "  sentinels_in_calculations = \"missing\",",
    "",
    "  # Pension discount rate from the pension rates lookup. Pension DVs stop until this is set.",
    "  rate = NULL,",
    "",
    "  # Age pension amounts are expected at",
    "  target_age = 66",
    ")"
  )
}


#' @keywords internal
#' @noRd
test_helper_lines <- function() {
  c(
    "# Loaded before the example tests: makes the suite available as `suite`.",
    "library(autospec)",
    "options(autospec.quiet = TRUE)",
    "suite <- suppressMessages(read_dv_suite(\"..\"))"
  )
}


#' Example-test stubs for the steps that need writing by hand
#' @keywords internal
#' @noRd
test_stub_lines <- function(topic, stubs, header = TRUE) {
  header_lines <- c(
    paste0("# Example tests for the ", topic, " steps written by hand."),
    "# For each: fill `given` with rows covering every branch of the spec, put the",
    "# values the spec gives in `expected`, then delete the skip() line.",
    "testthat::local_edition(3)"
  )

  blocks <- lapply(stubs, function(step) {
    inputs <- step$inputs

    given_lines <- if (length(inputs) == 0L) {
      "  given <- data.frame(row = 1:2)"
    } else {
      c(
        "  given <- data.frame(",
        paste0("    ", vapply(inputs, deparse_name, character(1L)), " = c(NA, NA)",
               c(rep(",", length(inputs) - 1L), "")),
        "  )"
      )
    }

    c(
      "",
      paste0("test_that(\"", step$dv, " follows the spec\", {"),
      paste0("  skip(\"", step$dv, " is not written yet\")"),
      "",
      given_lines,
      "  expected <- c(NA, NA)",
      "",
      paste0("  result <- run_dv_step(given, suite, \"", step$dv, "\")"),
      "",
      paste0("  expect_equal(result[[\"", step$dv, "\"]], expected, ignore_attr = TRUE)"),
      "})"
    )
  })

  c(if (header) header_lines, unlist(blocks, use.names = FALSE))
}
