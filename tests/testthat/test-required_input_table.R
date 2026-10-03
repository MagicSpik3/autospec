test_that("the required input table lists each input once and flags what is in the data", {
  plan <- data.frame(
    dv = c("DV_A", "DV_B", "DV_C"),
    inputs = c("x; y; z", "x; q", "missing; y"),
    stringsAsFactors = FALSE
  )

  table <- make_required_input_table(plan, data_names = c("x", "y", "q"))

  expect_identical(table$variable, c("missing", "q", "x", "y", "z"))
  expect_identical(table$found_in_data, c(FALSE, TRUE, TRUE, TRUE, FALSE))
  expect_identical(table$used_by_dvs[[which(table$variable == "x")]], "DV_A; DV_B")
  expect_identical(table$used_by_dvs[[which(table$variable == "missing")]], "DV_C")
})
