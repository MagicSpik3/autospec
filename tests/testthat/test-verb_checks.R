test_that("verbs say what they built", {
  df <- data.frame(CommiR9 = c(4, 1, 5))

  expect_message(dv_copy(df, "CommiR9", "copy"), "copy: copied from CommiR9")
  expect_message(
    dv_flag_if(df, df$CommiR9 %in% c(4, 5), "flag"),
    "1 where df\\$CommiR9 %in% c\\(4, 5\\) \\(2 rows\\), 0 elsewhere"
  )
})


test_that("the wealthdv.quiet option silences verb reports", {
  old_options <- options(wealthdv.quiet = TRUE)
  on.exit(options(old_options))

  expect_silent(dv_copy(data.frame(a = 1), "a", "b"))
})


test_that("a verb error is printed outside the suite and left to it inside", {
  expect_message(try(dv_copy(data.frame(a = 1), "z", "b"), silent = TRUE), "not found")

  old_options <- options(wealthdv.in_suite = TRUE)
  on.exit(options(old_options))

  expect_silent(try(dv_copy(data.frame(a = 1), "z", "b"), silent = TRUE))
})


test_that("long conditions are described in words", {
  expect_identical(describe_condition(quote(df$a > 0)), "df$a > 0")
  expect_identical(describe_condition(c(TRUE, FALSE)), "the condition")
})


test_that("counts are formatted with thousands separators", {
  expect_identical(format_count(21000L), "21,000")
})


test_that("the sentinels setting stops, or treats -8/-9 as missing or zero", {
  df <- data.frame(hhserial = c(1, 1), a = c(5, -9), b = c(1, 1))

  expect_error(suppressMessages(dv_row_total(df, c("a", "b"), "t")), "sentinel")
  expect_identical(suppressMessages(dv_row_total(df, c("a", "b"), "t", sentinels = "missing"))$t, c(6, 1))
  expect_identical(
    suppressMessages(dv_row_total(df, c("a", "b"), "t", na_as_zero = FALSE, sentinels = "missing"))$t,
    c(6, NA)
  )
  expect_identical(suppressMessages(dv_difference(df, "a", "b", "d", sentinels = "zero"))$d, c(4, -1))
  expect_message(dv_sum_to_household(df, "a", "s", sentinels = "missing"), "1 -8/-9 values treated as missing")
  expect_error(suppressMessages(dv_ratio(df, "a", "b", "r", sentinels = "ignore")), "must be")
})


test_that("sentinels only count in the rows dv_keep_if copies", {
  df <- data.frame(type = c(2, 3), amount = c(10, -9))

  result <- suppressMessages(dv_keep_if(df, df$type == 2, "amount", "kept", sentinels = "missing"))

  expect_identical(result$kept, c(10, 0))
  expect_silent(sentinel_note(sentinel_safe_values(df, "amount", "v", "missing", rows = df$type == 2), "missing"))
  expect_identical(
    sentinel_note(sentinel_safe_values(df, "amount", "v", "missing", rows = df$type == 2), "missing"),
    ""
  )
})

