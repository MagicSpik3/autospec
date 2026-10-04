test_that("the required input table lists each input once and flags what is in the data", {
  plan <- data.frame(
    dv = c("DV_A", "DV_B", "DV_C"),
    inputs = c("x; y; z", "x; q", "missing; y"),
    stringsAsFactors = FALSE
  )

  table <- make_required_input_table(plan, data_names = c("x", "y", "q"))

  expect_identical(table$variable, c("missing", "q", "x", "y", "z"))
  expect_identical(table$found_in_data, c(FALSE, TRUE, TRUE, TRUE, FALSE))
  expect_identical(table$available, c(FALSE, TRUE, TRUE, TRUE, FALSE))
  expect_true(all(table$provided_by_dvs == ""))
  expect_true(all(table$provided_by_uids == ""))
  expect_identical(table$used_by_dvs[[which(table$variable == "x")]], "DV_A; DV_B")
  expect_identical(table$used_by_dvs[[which(table$variable == "missing")]], "DV_C")
})


test_that("the input audit distinguishes raw columns from plan-produced inputs", {
  plan <- data.frame(
    uid = c("req_1", "req_2"),
    dv = c("intermediate", "final"),
    inputs = c("raw", "intermediate"),
    stringsAsFactors = FALSE
  )

  table <- make_required_input_table(plan, data_names = "raw")
  intermediate <- table[table$variable == "intermediate", ]

  expect_false(intermediate$found_in_data)
  expect_identical(intermediate$provided_by_dvs, "intermediate")
  expect_identical(intermediate$provided_by_uids, "req_1")
  expect_true(intermediate$available)
  expect_identical(intermediate$used_by_uids, "req_2")
})
