with_tracking <- function(plan, written = NA, stub = NA, populated = NA, passed = NA) {
  plan$`DV Written` <- written
  plan$`Unit Test Stub` <- stub
  plan$`Unit Test Populated` <- populated
  plan$`Unit Test Passed` <- passed
  plan
}


make_tracking_catalogue <- function(variable, instructions, sheet_name = "Sheet1") {
  data.table::data.table(
    file_name = "spec.xlsx",
    sheet_name = sheet_name,
    level = "person",
    excel_row = seq_along(variable) + 1L,
    variable = variable,
    label = NA_character_,
    instructions = instructions
  )
}


test_that("the tracking columns are named as the team uses them", {
  expect_identical(
    tracking_columns(),
    c("DV Written", "Unit Test Stub", "Unit Test Populated", "Unit Test Passed")
  )
})


test_that("a new plan has the tracking columns, all blank", {
  plan <- build_dv_plan(make_tracking_catalogue("DVa", "DVa = x"))

  expect_true(all(tracking_columns() %in% names(plan)))
  expect_true(all(is.na(plan[, tracking_columns(), with = FALSE])))
})


test_that("blank tracking values, or a complete chain of yes, have no problems", {
  blank <- with_tracking(fake_plan_row("a"))
  complete <- with_tracking(fake_plan_row("a"), "yes", "yes", "yes", "yes")

  expect_identical(nrow(tracking_problems(blank)), 0L)
  expect_identical(nrow(tracking_problems(complete)), 0L)
})


test_that("a value other than yes or no is a problem, and yes in any case is accepted", {
  wrong <- with_tracking(fake_plan_row("a"), written = "maybe")
  shouting <- with_tracking(fake_plan_row("a"), written = "YES", stub = "Yes")

  problems <- tracking_problems(wrong)

  expect_identical(problems$column, "DV Written")
  expect_match(problems$problem, "yes or no")
  expect_identical(nrow(tracking_problems(shouting)), 0L)
})


test_that("yes on a step needs yes on the steps it depends on", {
  no_written <- with_tracking(fake_plan_row("a"), "no", "yes", "yes", "yes")
  no_stub <- with_tracking(fake_plan_row("a"), written = "yes", stub = "no", populated = "yes")
  no_populated <- with_tracking(fake_plan_row("a"), "yes", "yes", "no", "yes")

  expect_identical(tracking_problems(no_written)$column, "Unit Test Passed")
  expect_identical(tracking_problems(no_stub)$column, "Unit Test Populated")
  expect_identical(tracking_problems(no_populated)$column, "Unit Test Passed")
})


test_that("a test stub may exist before its DV is written", {
  plan <- with_tracking(fake_plan_row("a"), written = "no", stub = "yes", populated = "yes")

  expect_identical(nrow(tracking_problems(plan)), 0L)
})


test_that("a problem names the DV and sheet it is on", {
  plan <- with_tracking(fake_plan_row("var_x", sheet_name = "Pension"), written = "yes", stub = "no", populated = "yes")

  problems <- tracking_problems(plan)

  expect_identical(problems$dv, "var_x")
  expect_identical(problems$sheet_name, "Pension")
})


test_that("a plan without the tracking columns has no problems", {
  expect_identical(nrow(tracking_problems(fake_plan_row("a"))), 0L)
})


test_that("tracking values carry over for the same DV on the same sheet, even if the spec text changed", {
  previous <- with_tracking(fake_plan_row("a", sheet_name = "S1", instructions = "a = x"), "yes", "yes")
  plan <- with_tracking(fake_plan_row("a", sheet_name = "S1", instructions = "a = y"))

  result <- carry_over_tracking(plan, previous)

  expect_identical(result$`DV Written`, "yes")
  expect_identical(result$`Unit Test Stub`, "yes")
  expect_true(is.na(result$`Unit Test Populated`))
})


test_that("a DV not in the earlier plan starts blank", {
  previous <- with_tracking(fake_plan_row("a", sheet_name = "S1"), "yes")
  plan <- with_tracking(fake_plan_row("a", sheet_name = "S2"))

  expect_true(is.na(carry_over_tracking(plan, previous)$`DV Written`))
})


test_that("an earlier plan without the tracking columns carries nothing and does not fail", {
  previous <- fake_plan_row("a", sheet_name = "S1")
  plan <- with_tracking(fake_plan_row("a", sheet_name = "S1"))

  expect_identical(carry_over_tracking(plan, previous), plan)
})
