test_that("a single block is located from its header", {
  blocks <- find_derivation_blocks(
    c("Variable Name", "Label", "Derivation", "Notes")
  )

  expect_equal(nrow(blocks), 1L)
  expect_identical(blocks$block_column, "C")
  expect_identical(blocks$derivation_col, 3L)
  expect_identical(blocks$variable_col, 1L)
  expect_identical(blocks$label_col, 2L)
})


test_that("side-by-side blocks are found and kept separate", {
  # The layout used by most Round 9 sheets: an input block with no derivation,
  # then a person block at column G and a household block at column L.
  header <- c(
    "Variable Name", "Label", "Value Labels", "Notes",
    "Variable Name", "Label", "Derivation", "Value Labels", "Notes",
    "Variable Name", "Label", "Derivation", "Value Labels", "Notes"
  )

  blocks <- find_derivation_blocks(header)

  expect_identical(blocks$block_column, c("G", "L"))
  expect_identical(blocks$variable_col, c(5L, 10L))
  expect_identical(blocks$label_col, c(6L, 11L))
})


test_that("a block does not borrow columns from the block to its left", {
  # The household variable column is the nearest one to the left of column L,
  # not the person variable column further away.
  header <- c(
    "Variable", "Label", "Derivation", "Notes",
    "Variable", "Label", "Derivation", "Notes"
  )

  blocks <- find_derivation_blocks(header)

  expect_identical(blocks$derivation_col, c(3L, 7L))
  expect_identical(blocks$variable_col, c(1L, 5L))
})


test_that("Variable and Variable Name are both recognised", {
  expect_equal(
    nrow(find_derivation_blocks(c("Variable", "Derivation"))),
    1L
  )

  expect_equal(
    nrow(find_derivation_blocks(c("Variable Name", "Derivation"))),
    1L
  )
})


test_that("a derivation with no variable column to its left is dropped", {
  blocks <- find_derivation_blocks(c("Notes", "Derivation"))

  expect_equal(nrow(blocks), 0L)
})


test_that("a block with no Label column reports a missing label position", {
  blocks <- find_derivation_blocks(
    c("Variable Name", "Derivation", "Notes")
  )

  expect_equal(nrow(blocks), 1L)
  expect_identical(blocks$label_col, NA_integer_)
})


test_that("a header with no derivation gives an empty result", {
  blocks <- find_derivation_blocks(c("Variable Name", "Label", "Notes"))

  expect_equal(nrow(blocks), 0L)
  expect_identical(
    names(blocks),
    c("block_column", "derivation_col", "variable_col", "label_col",
      "uid_col", "value_labels_col", "notes_col")
  )
})


test_that("missing header labels are tolerated", {
  blocks <- find_derivation_blocks(
    c(NA_character_, "Variable Name", "Derivation")
  )

  expect_equal(nrow(blocks), 1L)
  expect_identical(blocks$variable_col, 2L)
})


test_that("header must be a character vector", {
  expect_error(find_derivation_blocks(1:3))
})


test_that("value labels and notes columns are found within each block", {
  header <- c(
    "Variable Name", "Label", "Derivation", "Value Labels", "Notes",
    "Variable Name", "Label", "Derivation", "Notes"
  )

  blocks <- find_derivation_blocks(header)

  expect_identical(blocks$value_labels_col, c(4L, NA_integer_))
  expect_identical(blocks$notes_col, c(5L, 9L))
})


test_that("requirement UID columns are found within each derivation block", {
  header <- c(
    "Variable Name", "Label", "Derivation", "Notes", "Requirement UID",
    "Variable Name", "Label", "Derivation", "Notes", "Requirement UID"
  )

  blocks <- find_derivation_blocks(header)

  expect_identical(blocks$uid_col, c(5L, 10L))
})


test_that("the block label decides the level where it names one", {
  expect_identical(
    block_level(c("Person", "Household Level", "Input Variables"), 1:3, 3L),
    c("person", "household", "input")
  )
})


test_that("an unlabelled block's level comes from its position", {
  expect_identical(
    block_level(c(NA, "Investment Income"), 1:2, 2L),
    c("person", "household")
  )
  expect_identical(block_level(NA_character_, 1L, 1L), "unknown")
})
