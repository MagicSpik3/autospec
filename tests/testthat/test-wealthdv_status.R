test_that("with no plan, the next step is to draft one", {
  status <- suppressMessages(wealthdv_status(fake_config()))

  expect_match(status$next_step, "no plan yet")
})


test_that("with no suite, the next step is to write it", {
  config <- fake_config()
  suppressMessages(write_dv_plan(fake_plan_row("copy_a", "dv_copy", args = "source_col = \"a\""), config$plan_file))

  status <- suppressMessages(wealthdv_status(config))

  expect_match(status$next_step, "Write the suite")
})


test_that("signed-off stubs send you to sync, and the table counts them", {
  config <- fake_config()
  plan <- rbind(
    fake_plan_row("copy_a", "dv_copy", args = "source_col = \"a\"", status = "auto", reviewed_by = NA),
    fake_plan_row("by_hand", status = "hand_written", reviewed_by = NA)
  )
  suppressMessages(write_dv_suite(plan, config$suite_folder, data_names = "a"))
  plan$status <- "reviewed"
  suppressMessages(write_dv_plan(plan, config$plan_file))

  status <- suppressMessages(wealthdv_status(config))

  expect_match(status$next_step, "sync")
  expect_identical(status$topics$signed_off, 2L)
  expect_identical(status$topics$steps_not_written, 2L)
  # by_hand has no verb, so a sync cannot write it
  expect_identical(status$topics$ready_to_sync, 1L)
})


test_that("DVs still to decide come first", {
  config <- fake_config()
  plan <- rbind(
    fake_plan_row("b", "dv_copy", args = "source_col = \"a\"", status = "needs_review", sheet_name = "S1",
                  instructions = "b = a"),
    fake_plan_row("b", "dv_copy", args = "source_col = \"c\"", status = "needs_review", sheet_name = "S2",
                  instructions = "b = c")
  )
  suppressMessages(write_dv_plan(plan, config$plan_file))

  status <- suppressMessages(wealthdv_status(config))

  expect_match(status$next_step, "more than one row")
  expect_identical(status$topics$duplicates_to_decide, 1L)
})


test_that("rows signed off with no name, or that are not derivations, are pointed out", {
  config <- fake_config()
  plan <- rbind(
    fake_plan_row("copy_a", "dv_copy", args = "source_col = \"a\"", reviewed_by = NA),
    fake_plan_row("labels", reviewed_by = NA, notes = "value labels, not a derivation")
  )
  suppressMessages(write_dv_plan(plan, config$plan_file))

  messages <- messages_from(wealthdv_status(config))

  expect_true(any(grepl("no .*reviewed_by", messages)))
  expect_true(any(grepl("not a derivation", messages)))
})


test_that("only the chosen topics are shown", {
  config <- fake_config(topics = "income", all_topics = c("income", "property_wealth"))
  plan <- rbind(
    fake_plan_row("b", "dv_copy", args = "source_col = \"a\"", file_name = "Property_Wealth_DV_Spec_R9.xlsx"),
    fake_plan_row("d", "dv_copy", args = "source_col = \"a\"", file_name = "Income_DV_Spec_R9.xlsx")
  )
  suppressMessages(write_dv_plan(plan, config$plan_file))

  status <- suppressMessages(wealthdv_status(config))

  expect_identical(status$topics$topic, "income")
})
