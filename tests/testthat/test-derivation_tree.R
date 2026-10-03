first_condition <- function(text) {
  parse_derivation(text)$statements[[1L]]$branches[[1L]]$condition
}


test_that("names are collected from conditions and values, not function names", {
  parsed <- parse_derivation("IF x > 0 THEN y = SUM(a, b)")

  expect_identical(tree_names(parsed$statements), c("x", "a", "b"))
  expect_identical(tree_targets(parsed$statements), "y")
})


test_that("an addition of names flattens, anything else does not", {
  plain <- parse_derivation("t = a + b + SUM(c, d)")$statements[[1L]]$value
  mixed <- parse_derivation("t = a + b * 2")$statements[[1L]]$value

  expect_identical(flatten_addition(plain), c("a", "b", "c", "d"))
  expect_null(flatten_addition(mixed))
})


test_that("set membership translates to %in%", {
  expect_identical(
    condition_to_r(first_condition("IF CommiR9 in (4,5) THEN f = 1")),
    "CommiR9 %in% c(4, 5)"
  )
})


test_that("AND and comparisons translate to R operators", {
  expect_identical(
    condition_to_r(first_condition("IF x = 1 AND y > 0 THEN f = 1")),
    "(x == 1 & y > 0)"
  )
})


test_that("ANY with a value first translates to a chain of equalities", {
  expect_identical(
    condition_to_r(first_condition("IF ANY(2, a, b) THEN f = 1")),
    "(a == 2 | b == 2)"
  )
})


test_that("a function with no R equivalent cannot be translated", {
  expect_error(
    condition_to_r(first_condition("IF Partner(x) > 0 THEN f = 1")),
    class = "untranslatable"
  )
})


test_that("a name that is not syntactic R is backticked", {
  expect_identical(
    condition_to_r(first_condition("IF DLType(1-3)R9_i = 2 THEN f = 1")),
    "`DLType(1-3)R9_i` == 2"
  )
})


test_that("the multiplier applied to an amount is evaluated", {
  value <- parse_derivation("d = (a * 52) / 12")$statements[[1L]]$value

  expect_equal(evaluate_multiplier(value, "a"), 52 / 12)
  expect_null(evaluate_multiplier(value, "other"))
})


test_that("numbers format without scientific notation, one at a time", {
  expect_identical(format_number(c(1, 2.5, 150000, NA)), c("1", "2.5", "150000", "NA"))
})
