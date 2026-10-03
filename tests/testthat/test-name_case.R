test_that("the data's naming style is detected, tolerating a few odd names", {
  expect_identical(detect_name_case(c("hhserial", "dvager9", "x_1")), "lower")
  expect_identical(detect_name_case(c("HHSERIAL", "DVAGER9")), "upper")
  expect_identical(detect_name_case(c("HHSerial", "dvager9")), "spec")
  expect_identical(detect_name_case(c(paste0("v", 1:99), "ID")), "lower")
  expect_identical(detect_name_case(c("1", "_2")), "spec")
})


test_that("auto follows the data, and without data keeps the spec's spelling", {
  expect_identical(resolve_name_case("auto", c("a", "b")), "lower")
  expect_identical(resolve_name_case("auto", NULL), "spec")
  expect_identical(resolve_name_case("upper", c("a", "b")), "upper")
  expect_error(suppressMessages(check_name_case("camel")), "not recognised")
})


test_that("names the data has take its spelling; others follow the style", {
  spell <- name_speller(c("FCIsaVR9_i", "hhserial"), "lower", c("DVCopyR9"))

  expect_identical(spell(c("fcisavr9_i", "DVCopyR9", "HHSERIAL")), c("FCIsaVR9_i", "dvcopyr9", "hhserial"))
  expect_identical(spell(character()), character())
})


test_that("with the spec style, every mention of a DV takes its plan spelling", {
  spell <- name_speller(character(), "spec", c("DVBldValR9"))

  expect_identical(spell(c("DVblDValR9", "Other")), c("DVBldValR9", "Other"))
})


test_that("case clashes and duplicates are found", {
  clashes <- case_clashes(c("dvager9", "a", "dvager9"), c("DVAgeR9", "a"))

  expect_identical(clashes$name, "dvager9")
  expect_identical(clashes$known, "DVAgeR9")
  expect_identical(case_duplicates(c("a", "A", "b", "B", "b", "c")), c("a / A", "b / B"))
})
