test_that("a trial runs drafted calls with nothing signed off, and leaves the plan alone", {
  config <- fake_config()
  plan <- rbind(
    fake_plan_row("copy_a", "dv_copy", args = "source_col = \"a\"", status = "auto", reviewed_by = NA),
    fake_plan_row("flag", "dv_flag_if", condition = "a > 1", status = "needs_review", reviewed_by = NA)
  )
  df <- data.frame(hhserial = 1:2, a = c(1, 2))

  result <- suppressMessages(trial_run(df, config, plan = plan))

  expect_identical(result$copy_a, c(1, 2), ignore_attr = TRUE)
  expect_identical(result$flag, c(0, 1), ignore_attr = TRUE)
  expect_true(file.exists(config$trial_report_file))
  expect_false(file.exists(config$plan_file))
  expect_false(dir.exists(config$suite_folder))
})


test_that("a trial does not stop for a DV signed off twice in different ways", {
  config <- fake_config()
  plan <- rbind(
    fake_plan_row("b", "dv_copy", args = "source_col = \"a\"", sheet_name = "S1"),
    fake_plan_row("b", "dv_copy", args = "source_col = \"c\"", sheet_name = "S2")
  )
  df <- data.frame(hhserial = 1:2, a = c(1, 2), c = c(3, 4))

  result <- suppressMessages(trial_run(df, config, plan = plan))

  expect_identical(result$b, c(1, 2), ignore_attr = TRUE)
})


test_that("a trial runs only the chosen topics", {
  config <- fake_config(all_topics = c("income", "property_wealth"))
  plan <- rbind(
    fake_plan_row("b", "dv_copy", args = "source_col = \"a\"", file_name = "Property_Wealth_DV_Spec_R9.xlsx"),
    fake_plan_row("d", "dv_copy", args = "source_col = \"a\"", file_name = "Income_DV_Spec_R9.xlsx")
  )
  df <- data.frame(hhserial = 1:2, a = c(1, 2))

  result <- suppressMessages(trial_run(df, config, topics = "income", plan = plan))

  expect_true("d" %in% names(result))
  expect_false("b" %in% names(result))
})


test_that("a trial replaces the last one and uses the real suite's settings", {
  config <- fake_config()
  dir.create(config$suite_folder, recursive = TRUE)
  writeLines("settings <- list(by = \"hhserial\", sentinels = \"missing\")", file.path(config$suite_folder, "settings.R"))
  dir.create(config$trial_suite_folder, recursive = TRUE)
  writeLines("stale", file.path(config$trial_suite_folder, "old_topic.R"))

  plan <- fake_plan_row("copy_a", "dv_copy", args = "source_col = \"a\"", status = "auto", reviewed_by = NA)
  result <- suppressMessages(trial_run(data.frame(hhserial = 1:2, a = c(1, -9)), config, plan = plan))

  expect_false(file.exists(file.path(config$trial_suite_folder, "old_topic.R")))
  expect_identical(result$copy_a, c(1, NA), ignore_attr = TRUE)
})


test_that("inputs missing from the data are reported before the run, with the _i hint", {
  steps <- list(
    x = list(inputs = c("aaa_i", "bbb_i", "built"), topic = "t"),
    built = list(inputs = "ccc", topic = "t")
  )

  messages <- messages_from(missing <- report_missing_inputs(steps, c("aaa", "BBB", "ccc")))

  expect_identical(missing, c("aaa_i", "bbb_i"))
  expect_true(any(grepl("_i", messages, fixed = TRUE) & grepl("imputed", messages)))
})


test_that("the data must be a data frame", {
  expect_error(suppressMessages(trial_run("data", fake_config())), "must be a data frame")
})
