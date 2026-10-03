test_that("a match in any column sets the flag", {
  df <- data.frame(a = c(1, 2, NA, NA), b = c(1, NA, 3, NA))

  expect_identical(
    dv_flag_any_of(df, c("a", "b"), 2, "flag")$flag,
    c(0, 1, 0, NA)
  )
})


test_that("several match values are allowed", {
  df <- data.frame(a = c(1, 4, 6))

  expect_identical(dv_flag_any_of(df, "a", c(1, 6), "flag")$flag, c(1, 0, 1))
})


test_that("match_values must be numbers", {
  expect_error(dv_flag_any_of(data.frame(a = 1), "a", "2", "flag"))
})
