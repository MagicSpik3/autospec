test_that("a continuation shares the group of the item above it", {
  groups <- continuation_group(
    c("IF x = 1 THEN y = 1", "OTHERWISE y = 0", "z = 1")
  )

  expect_identical(groups, c(1L, 1L, 2L))
})


test_that("a leading continuation starts its own group", {
  groups <- continuation_group(c("OTHERWISE y = 0", "z = 1"))

  expect_identical(groups, c(1L, 2L))
})


test_that("an empty input gives an empty result", {
  expect_identical(continuation_group(character()), integer())
})


test_that("supplied continuation flags override the pattern", {
  # The spreadsheet knows better than the text does: a row carrying its own
  # variable name starts a new derivation even when it opens with ELSE.
  groups <- continuation_group(
    c("IF x = 1 THEN y = 1", "ELSE y = 0"),
    is_continuation = c(FALSE, FALSE)
  )

  expect_identical(groups, c(1L, 2L))
})


test_that("group numbers stay in step with the input", {
  groups <- continuation_group(
    c("a = 1", "OTHERWISE a = 0", "b = 1", "c = 1")
  )

  expect_length(groups, 4L)
  expect_true(!is.unsorted(groups))
})
