check_suite <- function(step_lines, data_names = NULL) {
  suite_dir <- tempfile("dv_suite")
  dir.create(suite_dir)
  writeLines("settings <- list(by = \"hhserial\")", file.path(suite_dir, "settings.R"))
  writeLines(c("steps <- list()", step_lines), file.path(suite_dir, "topic.R"))

  suppressMessages(check_dv_suite(suite_dir, data_names = data_names))
}


test_that("a clean step has no problems", {
  problems <- check_suite(
    "steps$b <- dv_step(label = 'B', inputs = 'a', derive = function(df) dv_copy(df, source_col = 'a', new_col = 'b'))",
    data_names = "a"
  )

  expect_identical(nrow(problems), 0L)
})


test_that("a function that does not exist is an error", {
  problems <- check_suite(
    "steps$b <- dv_step(label = 'B', inputs = 'a', derive = function(df) dv_copyy(df, 'a', 'b'))"
  )

  expect_true(any(problems$severity == "error" & grepl("dv_copyy", problems$problem)))
})


test_that("a verb argument it does not take is an error", {
  problems <- check_suite(
    "steps$b <- dv_step(label = 'B', inputs = 'a', derive = function(df) dv_copy(df, 'a', new_col = 'b', colour = 'red'))"
  )

  expect_true(any(problems$severity == "error" & grepl("does not take", problems$problem)))
})


test_that("a verb call missing a required argument is an error", {
  problems <- check_suite(
    "steps$b <- dv_step(label = 'B', inputs = 'a', derive = function(df) dv_copy(df, new_col = 'b'))"
  )

  expect_true(any(grepl("missing source_col", problems$problem)))
})


test_that("reading a DV not listed in inputs is a warning", {
  problems <- check_suite(c(
    "steps$b <- dv_step(label = 'B', inputs = 'a', derive = function(df) dv_copy(df, 'a', 'b'))",
    "steps$c <- dv_step(label = 'C', inputs = 'a', derive = function(df) dv_row_total(df, c('a', 'b'), 'c'))"
  ))

  expect_true(any(problems$dv == "c" & grepl("does not list it in inputs", problems$problem)))
})


test_that("code that never mentions its DV is a warning", {
  problems <- check_suite(
    "steps$b <- dv_step(label = 'B', inputs = 'a', derive = function(df) dv_copy(df, 'a', 'z'))"
  )

  expect_true(any(grepl("never mentions b", problems$problem)))
})


test_that("missing labels and unwritten steps are warnings", {
  problems <- check_suite("steps$b <- dv_step(label = NA, inputs = 'a', derive = NULL)")

  expect_setequal(problems$problem, c("has no label", "not written yet"))
  expect_true(all(problems$severity == "warning"))
})


test_that("inputs are checked against the data when its names are given, ignoring case", {
  problems <- check_suite(
    "steps$b <- dv_step(label = 'B', inputs = c('commir9', 'nothere'), derive = function(df) { df$b <- df$commir9 + df$nothere; df })",
    data_names = "CommiR9"
  )

  expect_false(any(grepl("commir9", problems$problem, ignore.case = TRUE)))
  expect_true(any(grepl("input not in the data.*nothere", problems$problem)))
})


test_that("case differences from the data are fine, but not within the suite", {
  problems <- check_suite(c(
    "steps$b <- dv_step(label = 'B', inputs = 'A', derive = function(df) { df$b <- df$A; df })",
    "steps$c <- dv_step(label = 'C', inputs = 'B', derive = function(df) dv_copy(df, 'B', 'c'))",
    "steps$total <- dv_step(label = 'T', inputs = 'a', derive = function(df) dv_copy(df, 'a', 'total'))",
    "steps$d <- dv_step(label = 'D', inputs = 'x', derive = function(df) dv_copy(df, 'X', 'd'))"
  ), data_names = c("a", "TOTAL", "x"))

  errors <- problems[problems$severity == "error", ]

  expect_true(any(errors$dv == "suite" & grepl("A / a", errors$problem)))
  expect_true(any(errors$dv == "c" & grepl("inputs spelt differently.*B \\(spelt b\\)", errors$problem)))
  expect_true(any(errors$dv == "d" & grepl("X \\(spelt x\\)", errors$problem)))
  expect_false(any(errors$dv == "total"))
})


test_that("local helpers and namespaced calls are not reported missing", {
  problems <- check_suite(c(
    "steps$b <- dv_step(label = 'B', inputs = 'a', derive = function(df) {",
    "  double_it <- function(x) x * 2",
    "  df$b <- data.table::fifelse(df$a > 0, double_it(df$a), 0)",
    "  df",
    "})"
  ))

  expect_false(any(grepl("does not exist", problems$problem)))
})


test_that("a filled-in example test passes, and a wrong one fails", {
  suite_dir <- tempfile("dv_suite")
  dir.create(file.path(suite_dir, "tests"), recursive = TRUE)
  writeLines("settings <- list(by = \"hhserial\")", file.path(suite_dir, "settings.R"))
  writeLines(c(
    "steps <- list()",
    "steps$b <- dv_step(label = 'B', inputs = 'a', derive = function(df) {",
    "  df$b <- ifelse(df$a > 0, df$a * 2, NA)",
    "  df",
    "})"
  ), file.path(suite_dir, "topic.R"))
  writeLines(
    c("library(wealthdv)", "suite <- suppressMessages(read_dv_suite(\"..\"))"),
    file.path(suite_dir, "tests", "helper-suite.R")
  )
  writeLines(c(
    "test_that('right', {",
    "  result <- run_dv_step(data.frame(a = c(2, -9)), suite, 'b')",
    "  expect_equal(result[['b']], c(4, NA), ignore_attr = TRUE)",
    "})",
    "test_that('wrong', {",
    "  result <- run_dv_step(data.frame(a = 2), suite, 'b')",
    "  expect_equal(result[['b']], 5, ignore_attr = TRUE)",
    "})"
  ), file.path(suite_dir, "tests", "test-topic.R"))

  utils::capture.output(results <- suppressMessages(test_dv_suite(suite_dir)))
  outcome <- as.data.frame(results)

  expect_identical(outcome$failed[outcome$test == "right"], 0L)
  expect_identical(outcome$failed[outcome$test == "wrong"], 1L)
})


test_that("a tests folder with no test files is not an error", {
  suite_dir <- tempfile("dv_suite")
  dir.create(file.path(suite_dir, "tests"), recursive = TRUE)
  writeLines("settings <- list(by = \"hhserial\")", file.path(suite_dir, "settings.R"))
  writeLines(c("steps <- list()",
               "steps$b <- dv_step(label = 'B', inputs = 'a', derive = function(df) dv_copy(df, source_col = 'a', new_col = 'b'))"),
             file.path(suite_dir, "topic.R"))
  writeLines("suite <- NULL", file.path(suite_dir, "tests", "helper-suite.R"))

  expect_null(suppressMessages(test_dv_suite(suite_dir)))
})


test_that("function names are found in code", {
  code_fn <- function(df) {
    helper(df)
    data.table::fifelse(TRUE, 1, 0)
  }

  found <- code_function_names(code_fn)

  expect_true("helper" %in% found)
  expect_false("fifelse" %in% found)
})


test_that("a step not written yet whose DV is in the data says so", {
  problems <- check_suite(
    c("steps$old <- dv_step(label = 'Old', inputs = 'a', derive = NULL)",
      "steps$new <- dv_step(label = 'New', inputs = 'a', derive = NULL)"),
    data_names = c("a", "old")
  )

  expect_identical(problems$problem[problems$dv == "old"], "not written yet, but already in the data")
  expect_identical(problems$problem[problems$dv == "new"], "not written yet")
})


test_that("a household identifier that is not in the data is an error", {
  suite_dir <- tempfile("dv_suite")
  dir.create(suite_dir)
  writeLines("settings <- list(by = \"hhserial\")", file.path(suite_dir, "settings.R"))
  writeLines(c("steps <- list()",
               "steps$t <- dv_step(label = 'T', inputs = 'v', derive = function(df) dv_sum_to_household(df, value_col = 'v', new_col = 't', by = settings$by))"),
             file.path(suite_dir, "topic.R"))

  problems <- suppressMessages(check_dv_suite(suite_dir, data_names = c("v", "hhserialr9")))

  expect_true(any(problems$dv == "settings.R" & problems$severity == "error" &
                    grepl("the data has hhserialr9", problems$problem)))
})
