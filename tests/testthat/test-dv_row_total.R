test_that("columns are added row by row", {
  df <- data.frame(a = c(1, 2), b = c(10, 20), c = c(100, 200))

  expect_identical(dv_row_total(df, c("a", "b", "c"), "t")$t, c(111, 222))
})


test_that("missing values count as zero unless told otherwise", {
  df <- data.frame(a = c(1, NA, NA), b = c(NA, 2, NA))

  expect_identical(dv_row_total(df, c("a", "b"), "t")$t, c(1, 2, 0))
  expect_identical(
    dv_row_total(df, c("a", "b"), "t", na_as_zero = FALSE)$t,
    c(NA_real_, NA_real_, NA_real_)
  )
})


test_that("a sentinel stops the total and names the column", {
  df <- data.frame(a = c(1, -8), b = c(2, 3))

  expect_error(dv_row_total(df, c("a", "b"), "t"), "a \\(1 rows\\)")
})


test_that("non-numeric and missing columns are refused", {
  df <- data.frame(a = c("1", "2"), b = c(1, 2))

  expect_error(dv_row_total(df, c("a", "b"), "t"), "numeric")
  expect_error(dv_row_total(df, c("b", "z"), "t"), "z")
})
