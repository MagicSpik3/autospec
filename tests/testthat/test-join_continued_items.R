test_that("OTHERWISE is joined to the preceding item", {
  input <- c(
    "IF FCOvDShR9_i = 2 THEN \r\n    CACrNumR9 = FCNumShR9_i",
    "OTHERWISE \r\n    CACrNumR9 = 0"
  )

  expected <- paste0(
    input[[1]],
    "\n",
    input[[2]]
  )

  result <- join_continued_items(input)

  expect_identical(result, expected)
  expect_length(result, 1L)
})


test_that("unrelated items remain separate", {
  input <- c(
    "CACrNumR9 = 0",
    "CACrValueR9 = 100"
  )

  result <- join_continued_items(input)

  expect_identical(result, input)
})


test_that("continuation keywords are case-insensitive", {
  input <- c(
    "IF x = 1 THEN\n    y = 2",
    "otherwise\n    y = 0"
  )

  result <- join_continued_items(input)

  expect_identical(
    result,
    paste0(input[[1]], "\n", input[[2]])
  )
})


test_that("leading whitespace before a continuation is allowed", {
  input <- c(
    "IF x = 1 THEN\n    y = 2",
    "    OTHERWISE\n        y = 0"
  )

  result <- join_continued_items(input)

  expect_identical(
    result,
    paste0(input[[1]], "\n", input[[2]])
  )
})


test_that("ELSE and ELSE IF are treated as continuations", {
  input <- c(
    "IF x = 1 THEN\n    y = 1",
    "ELSE IF x = 2 THEN\n    y = 2",
    "ELSE\n    y = 0"
  )

  result <- join_continued_items(input)

  expect_identical(
    result,
    paste(input, collapse = "\n")
  )

  expect_length(result, 1L)
})


test_that("multiple logical statements are reconstructed independently", {
  input <- c(
    "IF x = 1 THEN\n    y = 1",
    "OTHERWISE\n    y = 0",
    "z = 100",
    "IF a = 1 THEN\n    b = 1",
    "OTHERWISE\n    b = 0"
  )

  result <- join_continued_items(input)

  expect_identical(
    result,
    c(
      paste(input[1:2], collapse = "\n"),
      input[[3]],
      paste(input[4:5], collapse = "\n")
    )
  )
})


test_that("a continuation without a preceding item remains unchanged", {
  input <- c(
    "OTHERWISE\n    y = 0",
    "z = 100"
  )

  result <- join_continued_items(input)

  expect_identical(result, input)
})


test_that("an empty input returns an empty character vector", {
  expect_identical(
    join_continued_items(character()),
    character()
  )
})


test_that("missing items are removed before continuations are joined", {
  input <- c(
    "IF x = 1 THEN\n    y = 1",
    NA_character_,
    "OTHERWISE\n    y = 0"
  )

  result <- join_continued_items(input)

  expected <- paste(
    input[[1]],
    input[[3]],
    sep = "\n"
  )

  expect_identical(result, expected)
  expect_length(result, 1L)
})


test_that("x must be a character vector", {
  expect_snapshot_error(
    join_continued_items(1:3)
  )
})


test_that("an entirely missing input returns an empty character vector", {
  result <- join_continued_items(
    c(NA_character_, NA_character_)
  )

  expect_identical(result, character())
})


test_that("more than nine statements stay in their original order", {
  # Grouping by a number and splitting on it will reorder the result unless
  # the levels are set explicitly, because "10" sorts before "2".
  input <- paste0("x", 1:12, " = ", 1:12)

  expect_identical(join_continued_items(input), input)
})


test_that("order is preserved when continuations are joined throughout", {
  input <- c(
    rbind(
      paste0("IF a", 1:11, " = 1 THEN b", 1:11, " = 1"),
      paste0("OTHERWISE b", 1:11, " = 0")
    )
  )

  result <- join_continued_items(input)

  expect_length(result, 11L)
  expect_identical(
    result[[10L]],
    paste(input[[19L]], input[[20L]], sep = "\n")
  )
})


test_that("missing items are kept as missing when they are not dropped", {
  input <- c("a = 1", NA_character_, "b = 2")

  result <- join_continued_items(input, drop_na = FALSE)

  expect_identical(result, input)
})


test_that("a continuation does not attach to a retained missing item", {
  input <- c("a = 1", NA_character_, "OTHERWISE a = 0")

  result <- join_continued_items(input, drop_na = FALSE)

  expect_identical(result, input)
})
