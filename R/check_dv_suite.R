#' Check the DV suite for mistakes before running it
#'
#' Reads the suite and checks every step without touching any data:
#'
#' * the code calls only functions that exist, and calls verbs with arguments
#'   they take;
#' * the code mentions the DV it is meant to build;
#' * every column the code reads is listed in `inputs`, so steps run in the
#'   right order;
#' * no steps depend on each other in a circle;
#' * every DV has a label;
#' * no name is spelt in a different case from the data or the step that
#'   builds it;
#' * with `data_names`, every input is in the data or built by a step.
#'
#' @param suite The suite folder, or a suite from [read_dv_suite()].
#' @param data_names Optional column names of the input data.
#'
#' @return A data frame of problems (`topic`, `dv`, `severity`, `problem`),
#'   invisibly. `severity` is `"error"` for a step that will not run
#'   correctly and `"warning"` for something to look at.
#' @export
#'
#' @examples
#' \dontrun{
#' check_dv_suite("dv_suite", data_names = names(was_data))
#' }
check_dv_suite <- function(suite = "dv_suite", data_names = NULL) {
  if (!is.null(data_names) && !is.character(data_names)) {
    cli::cli_alert_danger("{.arg data_names} must be a character vector of column names.")
    stop("`data_names` must be a character vector.", call. = FALSE)
  }

  cli::cli_h1("Checking the DV suite")

  if (!is.null(data_names)) {
    stop_if_case_duplicates(data_names)
  }

  suite <- as_dv_suite(suite)
  steps <- suite$steps
  known_columns <- c(names(steps), data_names)
  household_id <- settings_column_names(suite$settings)

  # Case differences from the data are matched at run time; within the suite they are mistakes.
  suite_columns <- unique(c(names(steps), unlist(lapply(steps, `[[`, "inputs"), use.names = FALSE), household_id))

  problems <- list()

  add_problem <- function(step, severity, problem) {
    problems[[length(problems) + 1L]] <<- data.frame(
      topic = step$topic, dv = step$dv, severity = severity, problem = problem,
      stringsAsFactors = FALSE
    )
  }

  # Every household step uses it, so a wrong one fails them all at run time
  household_verbs <- c("dv_sum_to_household", "dv_any_to_household")
  uses_households <- any(vapply(
    steps,
    function(step) is.function(step$derive) && any(household_verbs %in% code_function_names(step$derive)),
    logical(1L)
  ))

  if (!is.null(data_names) && uses_households && length(household_id) > 0L &&
      !tolower(household_id) %in% tolower(data_names)) {
    suggestion <- guess_household_id(data_names)

    add_problem(
      list(topic = NA_character_, dv = "settings.R"), "error",
      paste0(
        "by = \"", household_id, "\" is not a column in the data",
        if (!identical(suggestion, household_id)) paste0("; the data has ", suggestion),
        ". Every household total and flag would fail"
      )
    )
  }

  for (spelling in case_duplicates(suite_columns)) {
    add_problem(
      list(topic = NA_character_, dv = "suite"), "error",
      paste0("spells one variable more than one way across step names and inputs: ", spelling, ". Use one spelling")
    )
  }

  for (step in steps) {
    if (is.na(step$label) || !nzchar(trimws(step$label))) {
      add_problem(step, "warning", "has no label")
    }

    input_clashes <- case_clashes(step$inputs, names(steps))

    if (nrow(input_clashes) > 0L) {
      add_problem(step, "error", paste0(
        "inputs spelt differently from the step that builds them: ",
        paste0(input_clashes$name, " (spelt ", input_clashes$known, ")", collapse = ", ")
      ))
    }

    if (!is.null(data_names)) {
      unknown <- step$inputs[!tolower(step$inputs) %in% tolower(known_columns)]

      if (length(unknown) > 0L) {
        add_problem(step, "error", paste0(
          "input not in the data or built by a step: ",
          describe_unknown_names(unknown, known_columns)
        ))
      }
    }

    if (is.null(step$derive)) {
      add_problem(step, "warning", if (tolower(step$dv) %in% tolower(data_names)) {
        "not written yet, but already in the data"
      } else {
        "not written yet"
      })
      next
    }

    for (problem in check_step_code(step, known_columns, suite_columns)) {
      add_problem(step, problem[["severity"]], problem[["problem"]])
    }
  }

  cycle <- tryCatch(
    {
      suppressMessages(order_suite_steps(steps))
      NULL
    },
    error = function(problem) problem
  )

  if (!is.null(cycle)) {
    problems[[length(problems) + 1L]] <- data.frame(
      topic = NA_character_, dv = NA_character_, severity = "error",
      problem = "steps depend on each other in a circle; check their inputs",
      stringsAsFactors = FALSE
    )
  }

  result <- if (length(problems) > 0L) {
    do.call(rbind, problems)
  } else {
    data.frame(topic = character(), dv = character(), severity = character(),
               problem = character(), stringsAsFactors = FALSE)
  }

  report_suite_check(result, length(steps), is.null(data_names))

  invisible(result)
}


#' Run the suite's example tests
#'
#' Runs [check_dv_suite()] and then the example tests in the suite's `tests`
#' folder, which check hand-written steps against values worked out from the
#' spec.
#'
#' @param suite_dir The suite folder.
#'
#' @return The testthat results, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' test_dv_suite("dv_suite")
#' }
test_dv_suite <- function(suite_dir = "dv_suite") {
  run_suite_tests(suite_dir, check_dv_suite(suite_dir))
}


#' Run the example tests once the suite has been checked
#' @keywords internal
#' @noRd
run_suite_tests <- function(suite_dir, problems) {
  test_folder <- file.path(suite_dir, "tests")

  if (length(list.files(test_folder, pattern = "^test.*[.][rR]$")) == 0L) {
    cli::cli_alert_info("No example tests in {.file {test_folder}}: every step uses a verb, or none are written yet.")
    return(invisible(NULL))
  }

  if (!requireNamespace("testthat", quietly = TRUE)) {
    cli::cli_alert_danger("The testthat package is needed to run the example tests. Run setup_autospec.R to install it.")
    stop("testthat is not installed.", call. = FALSE)
  }

  cli::cli_h1("Running the example tests")

  if (any(problems$severity == "error")) {
    cli::cli_alert_warning("The checks found errors; fix those first, as they may make tests fail.")
  }

  # The tests folder is not a package, so testthat would use its old edition,
  # which ignores ignore_attr and fails every test on the DV label.
  previous_edition <- Sys.getenv("TESTTHAT_EDITION", unset = NA)
  Sys.setenv(TESTTHAT_EDITION = "3")
  on.exit(
    if (is.na(previous_edition)) Sys.unsetenv("TESTTHAT_EDITION") else Sys.setenv(TESTTHAT_EDITION = previous_edition),
    add = TRUE
  )

  results <- testthat::test_dir(test_folder, stop_on_failure = FALSE)

  invisible(results)
}


#' Problems in one written step's code
#' @keywords internal
#' @noRd
check_step_code <- function(step, known_columns, suite_columns = known_columns) {
  problems <- list()
  add <- function(severity, problem) {
    problems[[length(problems) + 1L]] <<- c(severity = severity, problem = problem)
  }

  code <- body(step$derive)
  code_env <- environment(step$derive)

  local_names <- code_assigned_names(code)
  called <- setdiff(unique(code_function_names(step$derive)), local_names)
  missing_functions <- called[!vapply(called, exists, logical(1L), envir = code_env, mode = "function")]

  if (length(missing_functions) > 0L) {
    add("error", paste0("calls a function that does not exist: ", paste(missing_functions, collapse = ", ")))
  }

  for (verb_call in code_verb_calls(code, registered_verbs())) {
    verb <- as.character(verb_call[[1L]])
    matched <- tryCatch(
      match.call(get(verb, envir = asNamespace("autospec")), verb_call),
      error = function(problem) problem
    )

    if (inherits(matched, "error")) {
      add("error", paste0(verb, "() is given an argument it does not take: ", conditionMessage(matched)))
      next
    }

    required <- Filter(function(argument) !nzchar(formal_default(verb, argument)), names(verb_formals(verb)))
    not_given <- setdiff(required, names(as.list(matched))[-1L])

    if (length(not_given) > 0L) {
      add("error", paste0(verb, "() is missing ", paste(not_given, collapse = ", ")))
    }
  }

  code_clashes <- case_clashes(
    unique(c(code_column_names(code), code_strings(code))),
    suite_columns
  )

  if (nrow(code_clashes) > 0L) {
    add("error", paste0(
      "the code spells a column differently from the step's inputs or the suite: ",
      paste0(code_clashes$name, " (spelt ", code_clashes$known, ")", collapse = ", ")
    ))
  }

  if (!grepl(step$dv, paste(deparse(code), collapse = " "), fixed = TRUE)) {
    add("warning", paste0("the code never mentions ", step$dv, "; does it build it?"))
  }

  read_columns <- intersect(
    unique(c(code_column_names(code), code_strings(code))),
    known_columns
  )
  unlisted <- setdiff(read_columns, c(step$inputs, step$dv))

  if (length(unlisted) > 0L) {
    add("warning", paste0(
      "reads ", paste(unlisted, collapse = ", "),
      " but does not list it in inputs, so it may run before that is built"
    ))
  }

  problems
}


#' Names of the functions a function's body calls
#' @keywords internal
#' @noRd
code_function_names <- function(code) {
  if (is.function(code)) {
    code <- body(code)
  }

  if (!is.call(code)) {
    return(character())
  }

  call_head <- code[[1L]]

  if (is.call(call_head) && is.symbol(call_head[[1L]]) &&
      as.character(call_head[[1L]]) %in% c("::", ":::")) {
    here <- character()
  } else if (is.symbol(call_head)) {
    here <- as.character(call_head)
  } else {
    here <- code_function_names(call_head)
  }

  arguments <- as.list(code)[-1L]

  if (identical(here, "function")) {
    arguments <- list(code[[3L]])
  }

  c(here, unlist(lapply(arguments, code_function_names), use.names = FALSE))
}


#' Names assigned inside some code, so local helpers are not reported missing
#' @keywords internal
#' @noRd
code_assigned_names <- function(code) {
  if (!is.call(code)) {
    return(character())
  }

  here <- character()

  if (is.symbol(code[[1L]]) && as.character(code[[1L]]) %in% c("<-", "=", "<<-") &&
      is.symbol(code[[2L]])) {
    here <- as.character(code[[2L]])
  }

  c(here, unlist(lapply(as.list(code)[-1L], code_assigned_names), use.names = FALSE))
}


#' Calls to registered verbs inside some code
#' @keywords internal
#' @noRd
code_verb_calls <- function(code, verbs) {
  if (!is.call(code)) {
    return(list())
  }

  here <- if (is.symbol(code[[1L]]) && as.character(code[[1L]]) %in% verbs) list(code) else list()

  c(here, unlist(lapply(as.list(code)[-1L], code_verb_calls, verbs = verbs), recursive = FALSE))
}


#' Text constants inside some code
#' @keywords internal
#' @noRd
code_strings <- function(code) {
  if (is.character(code)) {
    return(code)
  }

  if (!is.call(code)) {
    return(character())
  }

  unlist(lapply(as.list(code)[-1L], code_strings), use.names = FALSE)
}


#' @keywords internal
#' @noRd
describe_unknown_names <- function(unknown, known_columns) {
  resolution <- resolve_variable_names(unknown, known_columns)
  hints <- ifelse(
    resolution$how == "case",
    paste0(" (spelt ", resolution$resolved, ")"),
    ifelse(is.na(resolution$suggestion), "", paste0(" (did you mean ", resolution$suggestion, "?)"))
  )

  paste0(unknown, hints, collapse = ", ")
}


#' @keywords internal
#' @noRd
report_suite_check <- function(result, step_count, no_data_names) {
  errors <- result[result$severity == "error", , drop = FALSE]
  warnings <- result[result$severity == "warning", , drop = FALSE]
  not_written <- startsWith(warnings$problem, "not written yet")
  in_data <- sum(warnings$problem == "not written yet, but already in the data")

  for (index in seq_len(nrow(errors))) {
    cli::cli_alert_danger("{errors$dv[index]} ({errors$topic[index]}): {errors$problem[index]}")
  }

  for (index in which(!not_written)) {
    cli::cli_alert_warning("{warnings$dv[index]} ({warnings$topic[index]}): {warnings$problem[index]}")
  }

  cli::cli_h2("Summary")
  cli::cli_alert_info("{step_count} step{?s} checked.")

  if (nrow(errors) == 0L) {
    cli::cli_alert_success("No errors.")
  } else {
    cli::cli_alert_danger("{nrow(errors)} error{?s} to fix before running.")
  }

  cli::cli_alert_warning("{sum(!not_written)} warning{?s} to look at; {sum(not_written)} step{?s} not written yet.")

  if (in_data > 0L) {
    cli::cli_alert_info(
      "{in_data} of the steps not written yet build a DV already in the data. If something earlier builds {?it/them}, mark {?it/them} not_a_derivation in the plan."
    )
  }

  if (no_data_names) {
    cli::cli_alert_info(
      "Inputs were not checked against the data. Pass {.code data_names = names(your_data)} to check them."
    )
  }

  invisible(NULL)
}
