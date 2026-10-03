test_that("numbers are written whole, and NA as $SYSMIS", {
  expect_equal(spss_number(c(300000, 0.25, -9, NA)), c("300000", "0.25", "-9", "$SYSMIS"))
  expect_equal(spss_text("Say \"hi\""), "\"Say \"\"hi\"\"\"")
})


test_that("conditions become SPSS with the brackets that matter", {
  expect_equal(spss_condition("df$a == 1"), "a = 1")
  expect_equal(spss_condition("(df$a != 1 & df$b >= 0.25)"), "a ~= 1 AND b >= 0.25")
  expect_equal(spss_condition("((df$a == 1 | df$b == 1) | df$c == 1)"), "a = 1 OR b = 1 OR c = 1")
  expect_equal(spss_condition("df$a == 1 & (df$b == 1 | df$c == 1)"), "a = 1 AND (b = 1 OR c = 1)")
  expect_equal(spss_condition("!((df$a == 1 | df$a == -9))"), "NOT (a = 1 OR a = -9)")
  expect_equal(spss_condition("!is.na(df$a)"), "NOT MISSING(a)")
})


test_that("%in% is false for a missing value, as in R", {
  expect_equal(spss_condition("df$a %in% c(1, 3)"), "NOT MISSING(a) AND ANY(a, 1, 3)")
  expect_equal(spss_condition("df$b == 1 | df$a %in% 2"), "b = 1 OR (NOT MISSING(a) AND ANY(a, 2))")
  expect_equal(spss_condition("!df$a %in% c(1, 3)"), "NOT (NOT MISSING(a) AND ANY(a, 1, 3))")
})


test_that("a condition with no SPSS here is flagged, not guessed", {
  expect_error(spss_condition("grepl('x', df$a)"), class = "spss_untranslatable")
  expect_error(spss_condition("df$a %in% c('x', 'y')"), class = "spss_untranslatable")
})


test_that("each verb's SPSS handles -8/-9 and missing values as its R does", {
  emit <- spss_emitters()

  expect_equal(emit$dv_copy(list(source_col = "a", sentinels = "missing"), "new"),
               c("COMPUTE new = a.", "IF (ANY(new, -8, -9)) new = $SYSMIS."))

  total <- emit$dv_row_total(list(value_cols = c("a", "b"), na_as_zero = TRUE, sentinels = "zero"), "total")
  expect_true("DO REPEAT column = a b." %in% total)
  expect_true("  IF (MISSING(#part)) #part = 0." %in% total)

  # AGGREGATE's SUM skips missing values, so without na_as_zero they are counted
  household <- emit$dv_sum_to_household(list(value_col = "a", by = "hh", na_as_zero = FALSE, sentinels = "keep"), "hh_a")
  expect_true("  /tmp_missing = NUMISS(tmp_amount)." %in% household)
  expect_true("IF (tmp_missing > 0) hh_a = $SYSMIS." %in% household)

  kept <- emit$dv_keep_if(list(condition = "df$c == 1", value_col = "a", otherwise = NA, sentinels = "zero"), "kept")
  expect_equal(kept[3:8], c("COMPUTE kept = $SYSMIS.", "DO IF (c = 1).", "  COMPUTE kept = #value.",
                            "ELSE.", "  COMPUTE kept = $SYSMIS.", "END IF."))

  # SPSS gives 0 / missing = 0 where R gives NA
  present <- emit$dv_discount_to_present_value(
    list(amount_col = "a", age_col = "age", rate = 0.05, target_age = 66, sentinels = "missing"), "pv"
  )
  expect_true("IF (MISSING(#years)) pv = $SYSMIS." %in% present)
  expect_error(emit$dv_discount_to_present_value(list(rate = NULL), "pv"), class = "spss_untranslatable")

  yearly <- emit$dv_annualise(
    list(amount_col = "amt", period_col = "per", period_codes = c(1, 2), multipliers = c(52, 12), sentinels = "zero"), "yearly"
  )
  expect_equal(yearly[1:2], c("RECODE per (1=52) (2=12) (ELSE=SYSMIS) INTO yearly.", "COMPUTE yearly = amt * yearly."))
  expect_true("IF (#amount_missing OR #period_missing) yearly = 0." %in% yearly)

  any_of <- emit$dv_flag_any_of(list(value_cols = c("c1", "c2"), match_values = c(2, 3)), "any")
  expect_true("IF (NVALID(c1, c2) = 0) any = $SYSMIS." %in% any_of)
})


test_that("a long command is wrapped onto indented lines", {
  lines <- spss_wrap(paste0("DO IF (", paste0("a", 1:20, " = 1", collapse = " OR "), ")."))

  expect_gt(length(lines), 1L)
  expect_true(all(nchar(lines) <= 78L))
  expect_true(all(startsWith(lines[-1L], "  ")))
})


test_that("a hand-written step gets a note pointing to its R", {
  step <- list(dv = "x", label = NA, derive = function(df) {
    df$x <- df$a
    df
  })

  expect_equal(spss_step_code(step), "* No SPSS given: this DV is written in R by hand, see the R below.")
})


test_that("a verb step ends with its label", {
  step <- list(dv = "copy_a", label = "Copy of a", derive = function(df) dv_copy(df, source_col = "a", new_col = "copy_a"))

  expect_equal(spss_step_code(step), c("COMPUTE copy_a = a.", "VARIABLE LABELS copy_a \"Copy of a\"."))
})
