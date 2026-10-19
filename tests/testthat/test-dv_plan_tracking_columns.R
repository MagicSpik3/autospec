make_tracking_catalogue <- function(variable, instructions) {
  data.table::data.table(
    file_name = "spec.xlsx",
    sheet_name = "Sheet1",
    level = "person",
    excel_row = seq_along(variable) + 1L,
    variable = variable,
    label = NA_character_,
    instructions = instructions
  )
}


test_that("the written dv_plan.csv has the DV tracking columns", {
  catalogue <- make_tracking_catalogue("DVa", "DVa = x")
  plan <- build_dv_plan(catalogue)
  path <- tempfile(fileext = ".csv")

  suppressMessages(write_dv_plan(plan, path))
  written <- utils::read.csv(path, check.names = FALSE, nrows = 0L)

  expect_contains(
    names(written),
    c("DV Written", "Unit Test Stub", "Unit Test Populated", "Unit Test Passed")
  )
})
