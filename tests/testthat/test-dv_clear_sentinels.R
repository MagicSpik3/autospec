test_that("-8 and -9 become zero, and real negatives and missing values survive", {
  df <- data.frame(v = c(5, -8, -9, -500, NA))

  expect_identical(
    dv_clear_sentinels(df, "v", "clean")$clean,
    c(5, 0, 0, -500, NA)
  )
})


test_that("the codes and replacement can be changed", {
  df <- data.frame(v = c(-8, -9))

  expect_identical(dv_clear_sentinels(df, "v", "clean", codes = -9)$clean, c(-8, 0))
  expect_identical(
    dv_clear_sentinels(df, "v", "clean", replacement = NA)$clean,
    c(NA_real_, NA_real_)
  )
})


test_that("a column can be cleaned in place", {
  df <- data.frame(v = -9)

  expect_identical(dv_clear_sentinels(df, "v", "v")$v, 0)
})
