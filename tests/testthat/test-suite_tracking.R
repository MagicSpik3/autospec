row_of <- function(table, dv) table[table$dv == dv, , drop = FALSE]


quiet_results <- function(suite_dir) {
  utils::capture.output(results <- suppressMessages(unit_test_results(suite_dir)))
  results
}


# Stage 1: what the suite files show, without running anything

test_that("every suite step has one row, with its UID", {
  tracking <- suite_tracking(tracking_suite())

  expect_setequal(tracking$dv, c("done", "broken", "draft", "todo", "no_uid", "untested", "uses_broken"))
  expect_identical(row_of(tracking, "done")$uid, "U1")
  expect_true(is.na(row_of(tracking, "no_uid")$uid))
})


test_that("a step with a function body is written, a derive = NULL step is not", {
  tracking <- suite_tracking(tracking_suite())

  expect_identical(row_of(tracking, "done")$`DV Written`, "yes")
  expect_identical(row_of(tracking, "todo")$`DV Written`, "no")
})


test_that("a test named with the step's UID is its stub, and a step with no test has none", {
  tracking <- suite_tracking(tracking_suite())

  expect_identical(row_of(tracking, "done")$`Unit Test Stub`, "yes")
  expect_identical(row_of(tracking, "untested")$`Unit Test Stub`, "no")
})


test_that("a test with no UID in its name is matched by the step name", {
  tracking <- suite_tracking(tracking_suite())

  expect_identical(row_of(tracking, "no_uid")$`Unit Test Stub`, "yes")
  expect_identical(row_of(tracking, "no_uid")$`Unit Test Populated`, "yes")
})


test_that("a test still holding its skip() line is a stub, not populated", {
  tracking <- suite_tracking(tracking_suite())

  expect_identical(row_of(tracking, "draft")$`Unit Test Populated`, "no")
  expect_identical(row_of(tracking, "done")$`Unit Test Populated`, "yes")
})


test_that("a stub is recorded even when its step is not written yet", {
  tracking <- suite_tracking(tracking_suite())

  expect_identical(row_of(tracking, "todo")$`Unit Test Stub`, "yes")
  expect_identical(row_of(tracking, "todo")$`Unit Test Populated`, "no")
})


test_that("a suite with no tests folder has no stubs, and is not an error", {
  suite_dir <- tracking_suite()
  unlink(file.path(suite_dir, "tests"), recursive = TRUE)

  tracking <- suite_tracking(suite_dir)

  expect_true(all(tracking$`Unit Test Stub` == "no"))
  expect_true(all(tracking$`Unit Test Populated` == "no"))
})


test_that("a test file that cannot be read stops, naming the file", {
  suite_dir <- tracking_suite()
  writeLines("test_that('x', {", file.path(suite_dir, "tests", "test-bad.R"))

  expect_error(suppressMessages(suite_tracking(suite_dir)), "test-bad.R")
})


# Stage 2: running the tests and recording each DV's result

test_that("passing, failing and skipped tests are recorded per DV", {
  results <- quiet_results(tracking_suite())

  expect_identical(row_of(results, "done")$`Unit Test Passed`, "yes")
  expect_identical(row_of(results, "no_uid")$`Unit Test Passed`, "yes")
  expect_identical(row_of(results, "broken")$`Unit Test Passed`, "no")
  expect_true(is.na(row_of(results, "draft")$`Unit Test Passed`))
  expect_true(is.na(row_of(results, "todo")$`Unit Test Passed`))
  expect_true(is.na(row_of(results, "untested")$`Unit Test Passed`))
})


test_that("a test that throws an error counts as not passed", {
  suite_dir <- tracking_suite(include_failing = FALSE)
  writeLines(c(
    "test_that('Requirement UID U6: untested follows the spec', {",
    "  stop('boom')",
    "})"
  ), file.path(suite_dir, "tests", "test-extra.R"))

  expect_identical(row_of(quiet_results(suite_dir), "untested")$`Unit Test Passed`, "no")
})


test_that("a suite with no tests has no results, and is not an error", {
  suite_dir <- tracking_suite()
  unlink(file.path(suite_dir, "tests"), recursive = TRUE)

  results <- quiet_results(suite_dir)

  expect_true(all(is.na(results$`Unit Test Passed`)))
})
