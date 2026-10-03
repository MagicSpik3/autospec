test_that("division happens only where the denominator is above zero", {
  df <- data.frame(n = c(10, 5, NA, 4), d = c(2, 0, 1, -1))

  expect_identical(dv_ratio(df, "n", "d", "r")$r, c(5, 0, NA, 0))
})


test_that("otherwise can be changed", {
  df <- data.frame(n = 1, d = 0)

  expect_identical(dv_ratio(df, "n", "d", "r", otherwise = NA)$r, NA_real_)
})


test_that("a sentinel stops the ratio", {
  expect_error(dv_ratio(data.frame(n = -9, d = 1), "n", "d", "r"), "sentinel")
})
