test_that("the source column is copied unchanged, sentinels included", {
  df <- data.frame(FCISAvR9_i = c(100, -9, NA))

  result <- dv_copy(df, "FCISAvR9_i", "DVCISAvR9")

  expect_identical(result$DVCISAvR9, c(100, -9, NA))
})


test_that("the sentinels setting turns -8/-9 into 0 or NA, or refuses", {
  df <- data.frame(a = c(100, -9, -8, NA))

  expect_identical(dv_copy(df, "a", "b", sentinels = "zero")$b, c(100, 0, 0, NA))
  expect_identical(dv_copy(df, "a", "b", sentinels = "missing")$b, c(100, NA, NA, NA))
  expect_error(dv_copy(df, "a", "b", sentinels = "stop"), "sentinel codes")
  expect_error(dv_copy(df, "a", "b", sentinels = "drop"), "keep")
})


test_that("a missing source column stops with its name", {
  df <- data.frame(a = 1)

  expect_error(dv_copy(df, "b", "c"), "b")
})


test_that("new_col must be a single name", {
  df <- data.frame(a = 1)

  expect_error(dv_copy(df, "a", c("x", "y")))
})


test_that("a data.table stays a data.table", {
  df <- data.table::data.table(a = 1:2)

  expect_true(data.table::is.data.table(dv_copy(df, "a", "b")))
})
