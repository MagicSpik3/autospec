test_that("target must be annual or monthly", {
  df <- data.frame(amount = 1, period = 5)

  expect_error(dv_annualise(df, "amount", "period", "x", target = "weekly"), "target")
})


test_that("amounts are annualised with was.utils", {
  skip_if_not_installed("was.utils")

  df <- data.frame(amount = c(100, 1000, -9), period = c(1, 5, 5))

  result <- dv_annualise(df, "amount", "period", "annual")

  expect_identical(
    result$annual,
    was.utils::annualise(values = df$amount, period = df$period)
  )
})


test_that("a monthly target is one twelfth, leaving sentinels alone", {
  skip_if_not_installed("was.utils")

  df <- data.frame(amount = c(100, -9), period = c(5, 5))

  result <- dv_annualise(df, "amount", "period", "monthly", target = "monthly")

  expect_identical(result$monthly, c(100, -9))
})


test_that("the spec's own period table is used when given", {
  df <- data.frame(amount = c(100, 200, 50, 0, -9, 30), period = c(1, 2, 3, -9, -9, 7))

  result <- suppressWarnings(dv_annualise(df, "amount", "period", "annual",
                                          period_codes = c(1, 2, 3), multipliers = c(52, 12, 1)))

  expect_identical(result$annual, c(5200, 2400, 50, 0, -9, NA))
  expect_warning(dv_annualise(df, "amount", "period", "annual",
                              period_codes = c(1, 2, 3), multipliers = c(52, 12, 1)), "not in the spec's table")
  expect_error(dv_annualise(df, "amount", "period", "x", period_codes = c(1, 2)), "both")
})


test_that("the sentinels setting decides what -8/-9 become", {
  df <- data.frame(amount = c(100, -9, 100), period = c(1, 1, -8))
  build <- function(sentinels) {
    dv_annualise(df, "amount", "period", "annual", period_codes = 1, multipliers = 52, sentinels = sentinels)$annual
  }

  expect_identical(build("keep"), c(5200, -9, -8))
  expect_identical(build("zero"), c(5200, 0, 0))
  expect_identical(build("missing"), c(5200, NA, NA))
  expect_error(build("stop"), "sentinel codes")
})


test_that("an unrecognised period code gives a warning", {
  skip_if_not_installed("was.utils")

  df <- data.frame(amount = 100, period = 6)

  expect_warning(dv_annualise(df, "amount", "period", "annual"), "period code")
})
