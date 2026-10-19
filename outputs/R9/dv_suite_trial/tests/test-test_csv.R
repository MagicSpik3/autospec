# Example tests for the test_csv steps written by hand.
# For each: fill `given` with rows covering every branch of the spec, put the
# values the spec gives in `expected`, then delete the skip() line.
testthat::local_edition(3)

test_that("Requirement UID dv_000001: var_c1 follows the spec", {
  skip("var_c1 is not written yet")
  expect_identical(suite$steps[["var_c1"]]$uid, "dv_000001")

  given <- data.frame(
    var_a = c(NA, NA),
    var_b1 = c(NA, NA),
    var_d1 = c(NA, NA),
    YearR9 = c(NA, NA)
  )
  expected <- c(NA, NA)

  result <- run_dv_step(given, suite, "var_c1")

  expect_equal(result[["var_c1"]], expected, ignore_attr = TRUE)
})

test_that("Requirement UID dv_000002: var_c2 follows the spec", {
  skip("var_c2 is not written yet")
  expect_identical(suite$steps[["var_c2"]]$uid, "dv_000002")

  given <- data.frame(
    var_a = c(NA, NA),
    var_b2 = c(NA, NA),
    var_d2 = c(NA, NA),
    YearR9 = c(NA, NA)
  )
  expected <- c(NA, NA)

  result <- run_dv_step(given, suite, "var_c2")

  expect_equal(result[["var_c2"]], expected, ignore_attr = TRUE)
})

test_that("Requirement UID dv_000003: var_e1 follows the spec", {
  skip("var_e1 is not written yet")
  expect_identical(suite$steps[["var_e1"]]$uid, "dv_000003")

  given <- data.frame(
    var_a = c(NA, NA),
    var_b1 = c(NA, NA),
    var_d1 = c(NA, NA),
    MonthR9 = c(NA, NA)
  )
  expected <- c(NA, NA)

  result <- run_dv_step(given, suite, "var_e1")

  expect_equal(result[["var_e1"]], expected, ignore_attr = TRUE)
})

test_that("Requirement UID dv_000004: var_e2 follows the spec", {
  skip("var_e2 is not written yet")
  expect_identical(suite$steps[["var_e2"]]$uid, "dv_000004")

  given <- data.frame(
    var_a = c(NA, NA),
    var_b2 = c(NA, NA),
    var_d2 = c(NA, NA),
    MonthR9 = c(NA, NA)
  )
  expected <- c(NA, NA)

  result <- run_dv_step(given, suite, "var_e2")

  expect_equal(result[["var_e2"]], expected, ignore_attr = TRUE)
})

test_that("Requirement UID dv_000005: var_h1 follows the spec", {
  skip("var_h1 is not written yet")
  expect_identical(suite$steps[["var_h1"]]$uid, "dv_000005")

  given <- data.frame(
    var_c1 = c(NA, NA),
    var_e1 = c(NA, NA)
  )
  expected <- c(NA, NA)

  result <- run_dv_step(given, suite, "var_h1")

  expect_equal(result[["var_h1"]], expected, ignore_attr = TRUE)
})

test_that("Requirement UID dv_000006: var_h2 follows the spec", {
  skip("var_h2 is not written yet")
  expect_identical(suite$steps[["var_h2"]]$uid, "dv_000006")

  given <- data.frame(
    var_c2 = c(NA, NA),
    var_e2 = c(NA, NA)
  )
  expected <- c(NA, NA)

  result <- run_dv_step(given, suite, "var_h2")

  expect_equal(result[["var_h2"]], expected, ignore_attr = TRUE)
})

test_that("Requirement UID dv_000007: var_f1 follows the spec", {
  skip("var_f1 is not written yet")
  expect_identical(suite$steps[["var_f1"]]$uid, "dv_000007")

  given <- data.frame(
    var_g1 = c(NA, NA),
    var_h1 = c(NA, NA),
    var_i1 = c(NA, NA)
  )
  expected <- c(NA, NA)

  result <- run_dv_step(given, suite, "var_f1")

  expect_equal(result[["var_f1"]], expected, ignore_attr = TRUE)
})

test_that("Requirement UID dv_000008: var_f2 follows the spec", {
  skip("var_f2 is not written yet")
  expect_identical(suite$steps[["var_f2"]]$uid, "dv_000008")

  given <- data.frame(
    var_g2 = c(NA, NA),
    var_h2 = c(NA, NA),
    var_i2 = c(NA, NA)
  )
  expected <- c(NA, NA)

  result <- run_dv_step(given, suite, "var_f2")

  expect_equal(result[["var_f2"]], expected, ignore_attr = TRUE)
})

test_that("Requirement UID dv_000010: var_m follows the spec", {
  skip("var_m is not written yet")
  expect_identical(suite$steps[["var_m"]]$uid, "dv_000010")

  given <- data.frame(row = 1:2)
  expected <- c(NA, NA)

  result <- run_dv_step(given, suite, "var_m")

  expect_equal(result[["var_m"]], expected, ignore_attr = TRUE)
})

test_that("Requirement UID dv_000014: var_t follows the spec", {
  skip("var_t is not written yet")
  expect_identical(suite$steps[["var_t"]]$uid, "dv_000014")

  given <- data.frame(row = 1:2)
  expected <- c(NA, NA)

  result <- run_dv_step(given, suite, "var_t")

  expect_equal(result[["var_t"]], expected, ignore_attr = TRUE)
})
