test_that("values above zero are flagged, zero and missing are not", {
  df <- data.frame(value = c(5, 0, NA))

  expect_identical(dv_flag_positive(df, "value", "has")$has, c(1, 0, NA))
})


test_that("a threshold other than zero can be used", {
  df <- data.frame(ratio = c(0.25, 0.20, 0.10))

  expect_identical(
    dv_flag_positive(df, "ratio", "high", threshold = 0.20)$high,
    c(1, 0, 0)
  )
})


test_that("a sentinel stops rather than counting as no", {
  expect_error(
    dv_flag_positive(data.frame(value = -9), "value", "has"),
    "sentinel"
  )
})


test_that("threshold must be a single number", {
  expect_error(dv_flag_positive(data.frame(value = 1), "value", "has", threshold = "0"))
})
