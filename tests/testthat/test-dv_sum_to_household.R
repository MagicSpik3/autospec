test_that("values are summed within households and written to every person", {
  df <- data.frame(hhserial = c(1, 1, 2), savings = c(10, 5, 7))

  expect_identical(
    dv_sum_to_household(df, "savings", "savings_hh")$savings_hh,
    c(15, 15, 7)
  )
})


test_that("missing values count as zero unless told otherwise", {
  df <- data.frame(hhserial = c(1, 1, 2), v = c(10, 5, NA))

  expect_identical(dv_sum_to_household(df, "v", "t")$t, c(15, 15, 0))
  expect_identical(
    dv_sum_to_household(df, "v", "t", na_as_zero = FALSE)$t,
    c(15, 15, NA)
  )
})


test_that("households need not be sorted", {
  df <- data.frame(hhserial = c("b", "a", "b"), v = c(1, 2, 3))

  expect_identical(dv_sum_to_household(df, "v", "t")$t, c(4, 2, 4))
})


test_that("a different household identifier can be named", {
  df <- data.frame(hh = c(1, 1), v = c(1, 2))

  expect_identical(dv_sum_to_household(df, "v", "t", by = "hh")$t, c(3, 3))
})


test_that("a sentinel or a missing household identifier stops", {
  expect_error(
    dv_sum_to_household(data.frame(hhserial = 1, v = -9), "v", "t"),
    "sentinel"
  )
  expect_error(
    dv_sum_to_household(data.frame(hhserial = NA, v = 1), "v", "t"),
    "missing"
  )
})


test_that("an empty data frame gains an empty column", {
  df <- data.frame(hhserial = numeric(), v = numeric())

  result <- dv_sum_to_household(df, "v", "t")

  expect_identical(result$t, numeric())
})
