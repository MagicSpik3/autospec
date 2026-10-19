# A suite with a step at each point of the DV tracking chain, and its example tests:
#   done        written, test populated, passes
#   broken      written, test populated, fails (passes when include_failing = FALSE)
#   draft       written, test still a stub (skip() left in)
#   todo        derive = NULL, test still a stub
#   no_uid      written, test populated and named by DV, not UID, passes
#   untested    written, no test at all
#   uses_broken written, test passes, but reads broken
tracking_suite <- function(include_failing = TRUE) {
  suite_dir <- tempfile("dv_suite")
  dir.create(file.path(suite_dir, "tests"), recursive = TRUE)
  writeLines(c(
    "settings <- list(by = \"hhserial\", na_as_zero = TRUE, codes = c(-8, -9), replacement = 0,",
    "                 sentinels = \"zero\", sentinels_in_calculations = \"missing\", rate = 0.05, target_age = 66)"
  ), file.path(suite_dir, "settings.R"))
  writeLines(c(
    "steps <- list()",
    "steps$done <- dv_step(label = 'Done', inputs = 'a', uid = 'U1', derive = function(df) {",
    "  df$done <- df$a * 2",
    "  df",
    "})",
    "steps$broken <- dv_step(label = 'Broken', inputs = 'a', uid = 'U2', derive = function(df) {",
    "  df$broken <- df$a + 1",
    "  df",
    "})",
    "steps$draft <- dv_step(label = 'Draft', inputs = 'a', uid = 'U3', derive = function(df) {",
    "  df$draft <- df$a",
    "  df",
    "})",
    "steps$todo <- dv_step(label = 'To do', inputs = 'a', uid = 'U4', derive = NULL)",
    "steps$no_uid <- dv_step(label = 'No UID', inputs = 'a', derive = function(df) {",
    "  df$no_uid <- df$a * 3",
    "  df",
    "})",
    "steps$untested <- dv_step(label = 'Untested', inputs = 'a', uid = 'U6', derive = function(df) {",
    "  df$untested <- df$a - 1",
    "  df",
    "})",
    "steps$uses_broken <- dv_step(label = 'Uses broken', inputs = 'broken', uid = 'U7', derive = function(df) {",
    "  df$uses_broken <- df$broken * 10",
    "  df",
    "})"
  ), file.path(suite_dir, "topic.R"))
  writeLines(
    c("library(autospec)", "suite <- suppressMessages(read_dv_suite(\"..\"))"),
    file.path(suite_dir, "tests", "helper-suite.R")
  )
  writeLines(c(
    "test_that('Requirement UID U1: done follows the spec', {",
    "  result <- run_dv_step(data.frame(a = 2), suite, 'done')",
    "  expect_equal(result[['done']], 4, ignore_attr = TRUE)",
    "})",
    "test_that('Requirement UID U2: broken follows the spec', {",
    "  result <- run_dv_step(data.frame(a = 2), suite, 'broken')",
    paste0("  expect_equal(result[['broken']], ", if (include_failing) 99 else 3, ", ignore_attr = TRUE)"),
    "})",
    "test_that('Requirement UID U3: draft follows the spec', {",
    "  skip('draft is not written yet')",
    "  result <- run_dv_step(data.frame(a = NA), suite, 'draft')",
    "})",
    "test_that('Requirement UID U4: todo follows the spec', {",
    "  skip('todo is not written yet')",
    "  result <- run_dv_step(data.frame(a = NA), suite, 'todo')",
    "})",
    "test_that('no_uid follows the spec', {",
    "  result <- run_dv_step(data.frame(a = 2), suite, 'no_uid')",
    "  expect_equal(result[['no_uid']], 6, ignore_attr = TRUE)",
    "})",
    "test_that('Requirement UID U7: uses_broken follows the spec', {",
    "  result <- run_dv_step(data.frame(broken = 3), suite, 'uses_broken')",
    "  expect_equal(result[['uses_broken']], 30, ignore_attr = TRUE)",
    "})"
  ), file.path(suite_dir, "tests", "test-topic.R"))

  suite_dir
}


# A plan for tracking_suite() with the tracking values the suite would give
finished_plan <- function() {
  dvs <- c("done", "broken", "draft", "todo", "no_uid", "untested", "uses_broken")
  plan <- do.call(rbind, lapply(dvs, fake_plan_row))
  plan$uid <- c("U1", "U2", "U3", "U4", NA, "U6", "U7")
  plan$`DV Written` <- c("yes", "yes", "yes", "no", "yes", "yes", "yes")
  plan$`Unit Test Stub` <- c("yes", "yes", "yes", "yes", "yes", "no", "yes")
  plan$`Unit Test Populated` <- c("yes", "yes", "no", "no", "yes", "no", "yes")
  plan$`Unit Test Passed` <- c("yes", "no", NA, NA, "yes", NA, "yes")
  plan
}
