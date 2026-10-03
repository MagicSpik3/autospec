test_that("an assignment parses into a target and a value", {
  parsed <- parse_derivation("y = a + b")

  expect_true(parsed$ok)
  expect_length(parsed$statements, 1L)
  expect_identical(parsed$statements[[1L]]$type, "assign")
  expect_identical(parsed$statements[[1L]]$target, "y")
  expect_identical(parsed$statements[[1L]]$value$op, "+")
})


test_that("IF THEN ELSE gives one branch and an else body", {
  parsed <- parse_derivation("IF x > 0 THEN y = 1 ELSE y = 0")
  statement <- parsed$statements[[1L]]

  expect_true(parsed$ok)
  expect_identical(statement$type, "if")
  expect_length(statement$branches, 1L)
  expect_length(statement$otherwise, 1L)
})


test_that("ELIF blocks with repeated ranges parse as branches", {
  parsed <- parse_derivation(
    "IF MOyr(1-2)R9 == 0 AND MOmn(1-2)R9 == 0 THEN\n  MOlft(1-2)R9 = 1\nELIF MOyr(1-2)R9 == 0 AND MOmn(1-2)R9 == -1 THEN\n  MOlft(1-2)R9 = 1\nELSE MOlft(1-2)R9 = MOyr(1-2)R9 * 12 + MOmn(1-2)R9"
  )

  expect_true(parsed$ok)
  expect_length(parsed$statements[[1L]]$branches, 2L)
  expect_length(parsed$statements[[1L]]$otherwise, 1L)
})


test_that("SPSS DO IF blocks with ELSE IF parse", {
  text <- paste(
    "DO IF (x = 1).",
    "  COMPUTE y = 2.",
    "ELSE IF (x = 2).",
    "  COMPUTE y = 3.",
    "END IF.",
    sep = "\n"
  )

  parsed <- parse_derivation(text)

  expect_true(parsed$ok)
  expect_length(parsed$statements, 1L)
  expect_length(parsed$statements[[1L]]$branches, 2L)
  expect_null(parsed$statements[[1L]]$otherwise)
})


test_that("separate IF statements are not nested", {
  parsed <- parse_derivation(
    "IF p = 1 THEN d = a * 52\nIF p = 2 THEN d = a * 26"
  )

  expect_true(parsed$ok)
  expect_length(parsed$statements, 2L)
})


test_that("x = 1,3,5 is read as set membership and noted", {
  parsed <- parse_derivation("IF t = 1,3,5 THEN y = 1")
  condition <- parsed$statements[[1L]]$branches[[1L]]$condition

  expect_identical(condition$type, "in")
  expect_length(condition$set, 3L)
  expect_true("'x = a,b,c' read as x in (a, b, c)" %in% parsed$notes)
})


test_that("x = (2 OR 5 OR 8) is read as set membership", {
  parsed <- parse_derivation("IF t = (2 OR 5 OR 8) THEN y = 1")
  condition <- parsed$statements[[1L]]$branches[[1L]]$condition

  expect_identical(condition$type, "in")
  expect_length(condition$set, 3L)
})


test_that("OTHERWISE in brackets becomes the else branch", {
  parsed <- parse_derivation("IF v > 0 then f = 1 (otherwise f = 0)")

  expect_true(parsed$ok)
  expect_length(parsed$statements[[1L]]$otherwise, 1L)
  expect_true("OTHERWISE written in brackets" %in% parsed$notes)
})


test_that("an assignment with nothing on its left is kept with an implied target", {
  parsed <- parse_derivation("If p = 1 then = amt * 52")
  assignment <- parsed$statements[[1L]]$branches[[1L]]$body[[1L]]

  expect_true(parsed$ok)
  expect_true(is.na(assignment$target))
  expect_true("assignment with no variable on its left" %in% parsed$notes)
})


test_that("an implied THEN is noted", {
  parsed <- parse_derivation("IF FTypeAcc_binary1r9_i = 1\n    CurrAccR9 = 1.")

  expect_true(parsed$ok)
  expect_true("IF without THEN" %in% parsed$notes)
})


test_that("a keyword run into a known name still parses", {
  parsed <- parse_derivation(
    "ifHvDBurd_NumR9>0 then f=1; else f=0;",
    known_names = "HvDBurd_NumR9"
  )

  expect_true(parsed$ok)
})


test_that("aggregate from person level becomes SUM", {
  parsed <- parse_derivation("aggregate from person level (DVCISAvR9)")
  value <- parsed$statements[[1L]]$value

  expect_identical(value$type, "call")
  expect_identical(value$fn, "SUM")
})


test_that("an unmatched closing bracket is removed and noted", {
  parsed <- parse_derivation("IF a < 60 THEN b = 60 ELSE b = a)")

  expect_true(parsed$ok)
  expect_true("unmatched closing bracket removed" %in% parsed$notes)
})


test_that("an unparseable derivation reports where it failed", {
  parsed <- parse_derivation("x = (a, b)")

  expect_false(parsed$ok)
  expect_match(parsed$error, "comma inside brackets")
  expect_length(parsed$statements, 0L)
})


test_that("prose is not mistaken for code", {
  parsed <- parse_derivation("IF n > 0 THEN ++1 for each iteration")

  expect_false(parsed$ok)
  expect_match(parsed$error, "prose")
})


test_that("text must be a single string", {
  expect_error(parse_derivation(NA_character_))
})


test_that("non-breaking spaces pasted from Word are read as spaces", {
  parsed <- parse_derivation(paste0("IF x = 1 THEN", intToUtf8(0xA0), "y = 1"))

  expect_true(parsed$ok)
})
