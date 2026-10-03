test_that("the flag follows the condition and a missing condition stays missing", {
  df <- data.frame(CommiR9 = c(4, 1, NA))

  result <- dv_flag_if(df, df$CommiR9 > 3, "flag")

  expect_identical(result$flag, c(1, 0, NA))
})


test_that("otherwise sets the value where the condition is false", {
  df <- data.frame(x = c(1, 0))

  expect_identical(dv_flag_if(df, df$x == 1, "flag", otherwise = NA)$flag, c(1, NA))
})


test_that("the condition must be logical and the right length", {
  df <- data.frame(x = c(1, 0))

  expect_error(dv_flag_if(df, c(1, 0), "flag"), "logical")
  expect_error(dv_flag_if(df, TRUE, "flag"), "rows")
})
