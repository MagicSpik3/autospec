test_that("band codes become midpoints and sentinels are kept", {
  df <- data.frame(UGdVbSR9_i = c(1, 3, 11, -9))

  result <- dv_band_midpoint(df, "UGdVbSR9_i", "HousGdsTR9", table = "ugdvbs")

  expect_identical(result$HousGdsTR9, c(0, 7500, 300000, -9))
})


test_that("with sentinels = zero, -8/-9 bands become 0", {
  df <- data.frame(UGdVbSR9_i = c(3, -9, -8))

  result <- dv_band_midpoint(df, "UGdVbSR9_i", "HousGdsTR9", table = "ugdvbs", sentinels = "zero")

  expect_identical(result$HousGdsTR9, c(7500, 0, 0))
})


test_that("a code not in the table is missing with a warning", {
  df <- data.frame(band = 20)

  expect_warning(
    result <- dv_band_midpoint(df, "band", "value", table = "gcontvb"),
    "not in the gcontvb table"
  )
  expect_identical(result$value, NA_real_)
})


test_that("the table must be one the package holds", {
  df <- data.frame(band = 1)

  expect_error(dv_band_midpoint(df, "band", "value", table = "nope"), "gcontvb")
})


test_that("the band tables are available by name", {
  expect_identical(
    names(band_midpoint_tables()),
    c("gcontvb", "ugdvbs", "ugdvbl", "ugdvbos")
  )
})
