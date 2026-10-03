test_that("keywords, names, numbers and operators are recognised", {
  tokens <- tokenise_derivation("IF x >= 10 THEN y = 1")

  expect_identical(
    tokens$kind,
    c("kw", "name", "op", "number", "kw", "name", "op", "number", "eof")
  )
  expect_identical(tokens$text[1:4], c("IF", "x", ">=", "10"))
})


test_that("keywords are recognised in any case", {
  tokens <- tokenise_derivation("if x then y = 1")

  expect_identical(tokens$text[[1L]], "IF")
  expect_identical(tokens$text[[3L]], "THEN")
})


test_that("a name carrying a numbered range is one token", {
  tokens <- tokenise_derivation("DCSC(1-5)R9 = 0")

  expect_identical(tokens$kind[[1L]], "name")
  expect_identical(tokens$text[[1L]], "DCSC(1-5)R9")
})


test_that("a number with a thousands comma is one number", {
  tokens <- tokenise_derivation("x <= 12,570")

  expect_identical(tokens$text[[3L]], "12,570")
  expect_true(tokens$comma[[3L]])
})


test_that("SAS comparison words become operators", {
  tokens <- tokenise_derivation("x ne 2")

  expect_identical(tokens$kind[[2L]], "op")
  expect_identical(tokens$text[[2L]], "!=")
})


test_that("a keyword run into a known name is split and noted", {
  tokens <- tokenise_derivation(
    "ifHvDBurd_NumR9 > 0",
    known_names = "HvDBurd_NumR9"
  )

  expect_identical(tokens$kind[1:2], c("kw", "name"))
  expect_identical(tokens$text[1:2], c("IF", "HvDBurd_NumR9"))
  expect_length(attr(tokens, "notes"), 1L)
})


test_that("a word that only looks like a glued keyword is left alone", {
  expect_identical(tokenise_derivation("ORDERS > 0")$text[[1L]], "ORDERS")
  expect_identical(tokenise_derivation("OrdSharesR9 > 0")$text[[1L]], "OrdSharesR9")
})


test_that("a line break before a token is recorded", {
  tokens <- tokenise_derivation("SUM\n(a)")

  expect_true(tokens$newline[[2L]])
})


test_that("an unrecognised character is a parse error", {
  expect_error(
    tokenise_derivation(paste0("x = ", intToUtf8(163), "5")),
    class = "derivation_parse_error"
  )
})


test_that("empty text gives only the end-of-text token", {
  tokens <- tokenise_derivation("")

  expect_identical(tokens$kind, "eof")
})


test_that("text must be a single string", {
  expect_error(tokenise_derivation(NA_character_))
  expect_error(tokenise_derivation(c("a", "b")))
})
