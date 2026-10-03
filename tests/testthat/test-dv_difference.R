test_that("one column is subtracted from another", {
  df <- data.frame(assets = c(500, 200), debts = c(100, 300))

  expect_identical(dv_difference(df, "assets", "debts", "net")$net, c(400, -100))
})


test_that("a missing value gives a missing difference", {
  df <- data.frame(a = c(5, NA), b = c(NA, 1))

  expect_identical(dv_difference(df, "a", "b", "d")$d, c(NA_real_, NA_real_))
})


test_that("a sentinel stops the calculation", {
  df <- data.frame(year = 2024, start = -9)

  expect_error(dv_difference(df, "year", "start", "years"), "sentinel")
})
