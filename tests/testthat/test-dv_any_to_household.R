test_that("a household is flagged when anyone in it is flagged", {
  df <- data.frame(hhserial = c(1, 1, 2, 2), owns = c(0, 1, 0, 0))

  expect_identical(
    dv_any_to_household(df, "owns", "owns_hh")$owns_hh,
    c(1L, 1L, 0L, 0L)
  )
})


test_that("missing people are ignored, and an all-missing household is missing", {
  df <- data.frame(hhserial = c(1, 1, 2, 2, 3), owns = c(NA, 1, NA, 0, NA))

  expect_identical(
    dv_any_to_household(df, "owns", "owns_hh")$owns_hh,
    c(1L, 1L, 0L, 0L, NA_integer_)
  )
})


test_that("anything other than 0, 1 or missing stops", {
  expect_error(
    dv_any_to_household(data.frame(hhserial = 1, f = -8), "f", "g"),
    "only 0, 1 or missing"
  )
  expect_error(
    dv_any_to_household(data.frame(hhserial = 1, f = 2), "f", "g"),
    "only 0, 1 or missing"
  )
})
