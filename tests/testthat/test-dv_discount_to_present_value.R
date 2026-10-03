test_that("an amount is discounted over the years to the target age", {
  df <- data.frame(pension = 1000, age = 64)

  result <- dv_discount_to_present_value(df, "pension", "age", "pv", rate = 0.1)

  expect_equal(result$pv, 1000 / 1.1^2)
})


test_that("rows at or past the target age are missing with a warning", {
  df <- data.frame(pension = c(1000, 1000), age = c(66, 70))

  expect_warning(
    result <- dv_discount_to_present_value(df, "pension", "age", "pv", rate = 0.1),
    "at or past"
  )
  expect_identical(result$pv, c(NA_real_, NA_real_))
})


test_that("the rate must be given", {
  df <- data.frame(pension = 1000, age = 60)

  expect_error(dv_discount_to_present_value(df, "pension", "age", "pv"), "rate")
})


test_that("a sentinel age stops the calculation", {
  df <- data.frame(pension = 1000, age = -9)

  expect_error(
    dv_discount_to_present_value(df, "pension", "age", "pv", rate = 0.1),
    "sentinel"
  )
})
