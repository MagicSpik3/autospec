test_that("a numbered range expands into its variables", {
  expect_identical(
    expand_variable_names("DCSC(1-3)R9"),
    c("DCSC1R9", "DCSC2R9", "DCSC3R9")
  )
})


test_that("two different ranges expand in every combination", {
  expect_identical(
    expand_variable_names("M(1-2)_x(1-3)"),
    c("M1_x1", "M1_x2", "M1_x3", "M2_x1", "M2_x2", "M2_x3")
  )
})


test_that("names without ranges pass through and missing names are dropped", {
  expect_identical(
    expand_variable_names(c("TOTCSCR9_SUM", NA, "TOTCSCR9_SUM")),
    "TOTCSCR9_SUM"
  )
})


test_that("names resolve exactly, by case, or with a suggestion", {
  resolution <- resolve_variable_names(
    c("DVHseValR9", "dvhsevalr9", "DVFLfSVR9", "Unrelated"),
    known_names = c("DVHseValR9", "DVFLfFSVR9")
  )

  expect_identical(resolution$how, c("exact", "case", "unknown", "unknown"))
  expect_identical(resolution$resolved[[2L]], "DVHseValR9")
  expect_identical(resolution$suggestion[[3L]], "DVFLfFSVR9")
  expect_true(is.na(resolution$suggestion[[4L]]))
})


test_that("resolving against no known names reports everything unknown", {
  resolution <- resolve_variable_names("a", character())

  expect_identical(resolution$how, "unknown")
})


test_that("DV names lose a trailing full stop and spaces before a numbered range", {
  expect_identical(clean_dv_name(c("DVDBInc (1-6)R9", " ActEpJbAmt2R9. ", "A B")), c("DVDBInc(1-6)R9", "ActEpJbAmt2R9", "A B"))
})


test_that("DV names compare ignoring case and a trailing full stop", {
  expect_true(same_variable("ActEpJbAmt2R9.", "actepjbamt2r9"))
  expect_false(same_variable("a", NA_character_))
})
