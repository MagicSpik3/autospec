test_that("Excel column references are generated in order", {
  expect_identical(excel_letters(3L), c("A", "B", "C"))

  expect_identical(
    excel_letters(28L)[c(26L, 27L, 28L)],
    c("Z", "AA", "AB")
  )
})


test_that("no columns gives no references", {
  expect_identical(excel_letters(0L), character())
})


test_that("blank cells cover missing and whitespace-only values", {
  expect_identical(
    is_blank(c("x", "", " ", NA_character_, "  a  ")),
    c(FALSE, TRUE, TRUE, TRUE, FALSE)
  )
})


test_that("header labels are compared without case or spacing", {
  expect_identical(
    normalise_header(c("Variable  Name", "Label ", " DERIVATION")),
    c("variable name", "label", "derivation")
  )
})


test_that("a missing header label becomes an empty string", {
  expect_identical(normalise_header(NA_character_), "")
})
