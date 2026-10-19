#' What each suite step's code and unit test show, read from the suite files
#'
#' Nothing is run. A step is written when it has a function body. Its unit test
#' is the test named `Requirement UID <uid>: ...`, or `<dv> follows the spec`
#' when the name has no UID, as write_dv_suite() names them. The test is
#' populated once its skip() line has been removed.
#'
#' @param suite_dir The suite folder.
#' @return A data frame with a row per step: `uid`, `dv`, `DV Written`,
#'   `Unit Test Stub` and `Unit Test Populated`, each `yes` or `no`.
#' @keywords internal
#' @noRd
suite_tracking <- function(suite_dir) {
  steps <- suppressMessages(read_dv_suite(suite_dir))$steps
  tests <- read_suite_tests(suite_dir)
  owner <- test_owners(tests$name, steps)

  has_test <- vapply(seq_along(steps), function(index) any(owner %in% index), logical(1L))
  populated <- vapply(seq_along(steps), function(index) any(owner %in% index & !tests$skipped), logical(1L))
  written <- vapply(steps, function(step) is.function(step$derive), logical(1L), USE.NAMES = FALSE)

  data.frame(
    uid = step_uids(steps),
    dv = names(steps),
    `DV Written` = yes_no(written),
    `Unit Test Stub` = yes_no(has_test),
    `Unit Test Populated` = yes_no(populated),
    check.names = FALSE,
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}


#' Run the suite's unit tests and record each step's result
#'
#' A step's test passed when every test for it ran without a failure or an
#' error. A step whose tests were all skipped, or that has none, is blank.
#'
#' @param suite_dir The suite folder.
#' @return A data frame with a row per step: `uid`, `dv` and `Unit Test Passed`,
#'   which is `yes`, `no` or `NA`.
#' @keywords internal
#' @noRd
unit_test_results <- function(suite_dir) {
  steps <- suppressMessages(read_dv_suite(suite_dir))$steps
  results <- run_suite_tests(suite_dir, problems = data.frame(severity = character()))

  outcomes <- if (is.null(results)) {
    data.frame(test = character(), failed = integer(), skipped = logical(), error = logical())
  } else {
    as.data.frame(results)
  }

  outcome <- ifelse(outcomes$failed > 0L | outcomes$error, "no", ifelse(outcomes$skipped, NA_character_, "yes"))
  owner <- test_owners(outcomes$test, steps)

  passed <- vapply(seq_along(steps), function(index) {
    mine <- outcome[owner %in% index]
    if ("no" %in% mine) "no" else if ("yes" %in% mine) "yes" else NA_character_
  }, character(1L))

  data.frame(
    uid = step_uids(steps),
    dv = names(steps),
    `Unit Test Passed` = passed,
    check.names = FALSE,
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}


#' The test_that() blocks in the suite's test files: their names, and whether they still skip
#' @keywords internal
#' @noRd
read_suite_tests <- function(suite_dir) {
  paths <- list.files(file.path(suite_dir, "tests"), pattern = "^test.*[.][rR]$", full.names = TRUE)

  found <- lapply(paths, function(path) {
    expressions <- tryCatch(
      parse(path, keep.source = FALSE),
      error = function(problem) {
        cli::cli_alert_danger("{.file {path}} could not be read: {conditionMessage(problem)}")
        stop("The test file ", basename(path), " has a syntax error; fix it first.", call. = FALSE)
      }
    )
    calls <- Filter(function(expression) is.call(expression) && call_name(expression) %in% "test_that", as.list(expressions))

    data.frame(
      name = vapply(calls, function(call) if (is.character(call[[2L]])) call[[2L]] else NA_character_, character(1L)),
      skipped = vapply(calls, function(call) calls_function(call[[3L]], "skip"), logical(1L)),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, c(list(data.frame(name = character(), skipped = logical(), stringsAsFactors = FALSE)), found))
}


#' The step each test belongs to, by the UID in its name or else by the DV it names
#' @keywords internal
#' @noRd
test_owners <- function(test_names, steps) {
  uid_pattern <- "^Requirement UID ([^:]+): .*$"
  dv_pattern <- "^(\\S+) follows the spec$"

  uid <- ifelse(grepl(uid_pattern, test_names), sub(uid_pattern, "\\1", test_names), NA_character_)
  dv <- ifelse(is.na(uid) & grepl(dv_pattern, test_names), sub(dv_pattern, "\\1", test_names), NA_character_)

  by_uid <- match(uid, step_uids(steps), incomparables = NA)
  by_dv <- match(tolower(dv), tolower(names(steps)), incomparables = NA)

  ifelse(is.na(by_uid), by_dv, by_uid)
}


#' @keywords internal
#' @noRd
step_uids <- function(steps) {
  vapply(steps, function(step) {
    uid <- step$uid
    if (is.character(uid) && length(uid) == 1L && !is.na(uid) && nzchar(uid)) uid else NA_character_
  }, character(1L), USE.NAMES = FALSE)
}


#' The name of the function a call calls, including pkg::name calls
#' @keywords internal
#' @noRd
call_name <- function(expression) {
  head <- expression[[1L]]

  if (is.name(head)) {
    return(as.character(head))
  }

  if (is.call(head) && length(head) == 3L && is.name(head[[1L]]) && as.character(head[[1L]]) %in% c("::", ":::")) {
    return(as.character(head[[3L]]))
  }

  NA_character_
}


#' Whether code calls the named function anywhere inside it
#' @keywords internal
#' @noRd
calls_function <- function(expression, name) {
  if (!is.call(expression)) {
    return(FALSE)
  }

  if (identical(call_name(expression), name)) {
    return(TRUE)
  }

  any(vapply(as.list(expression)[-1L], function(part) calls_function(part, name), logical(1L)))
}


#' @keywords internal
#' @noRd
yes_no <- function(value) ifelse(value, "yes", "no")
