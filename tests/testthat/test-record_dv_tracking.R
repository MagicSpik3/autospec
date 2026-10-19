# What suite_tracking() and unit_test_results() together would report
suite_state <- function() {
  data.frame(
    uid = c("U1", NA, "U4"),
    dv = c("done", "No_Uid", "todo"),
    `DV Written` = c("yes", "yes", "no"),
    `Unit Test Stub` = c("yes", "yes", "yes"),
    `Unit Test Populated` = c("yes", "yes", "no"),
    `Unit Test Passed` = c("yes", "yes", NA),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}


plan_for <- function(dvs, uids) {
  plan <- do.call(rbind, lapply(dvs, fake_plan_row))
  plan$uid <- uids
  plan
}


# Stage 3: the plan records what the suite shows

test_that("tracking values are recorded on the plan row with the same UID", {
  recorded <- record_tracking(plan_for("done", "U1"), suite_state())

  expect_identical(recorded$`DV Written`, "yes")
  expect_identical(recorded$`Unit Test Passed`, "yes")
})


test_that("a plan row with no UID is matched by DV name, in any case", {
  recorded <- record_tracking(plan_for("no_uid", NA_character_), suite_state())

  expect_identical(recorded$`Unit Test Passed`, "yes")
})


test_that("values typed in the plan are replaced by what the suite shows", {
  plan <- plan_for("done", "U1")
  plan$`Unit Test Passed` <- "no"

  expect_identical(record_tracking(plan, suite_state())$`Unit Test Passed`, "yes")
})


test_that("a plan row with no suite step is left blank", {
  recorded <- record_tracking(plan_for("not_synced", "U9"), suite_state())

  expect_true(all(is.na(unlist(recorded[, tracking_columns()]))))
})


test_that("a stub written before its DV is recorded without a tracking problem", {
  # Needs the rule change in ENGINEERING.md section 7: a stub no longer needs DV Written.
  recorded <- record_tracking(plan_for("todo", "U4"), suite_state())

  expect_identical(nrow(tracking_problems(recorded)), 0L)
})


test_that("update_dv_tracking writes what the suite shows into the plan, backing up the old one", {
  config <- fake_config()
  config$suite_folder <- tracking_suite()
  suppressMessages(write_dv_plan(plan_for(c("done", "broken", "todo"), c("U1", "U2", "U4")), config$plan_file))

  utils::capture.output(suppressMessages(update_dv_tracking(config)))
  written <- suppressMessages(read_dv_plan(config$plan_file))

  expect_identical(written$`DV Written`, c("yes", "yes", "no"))
  expect_identical(written$`Unit Test Passed`, c("yes", "no", NA))
  expect_length(list.files(file.path(config$output_folder, "plan_backups")), 1L)
})


test_that("update_dv_tracking with no plan stops and says to draft one", {
  config <- fake_config()
  config$suite_folder <- tracking_suite()

  expect_error(suppressMessages(update_dv_tracking(config)), "plan")
})
