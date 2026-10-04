# test_all_verbs_csv DVs
# Written by autospec::write_dv_suite() on 2026-10-04.
# Each step builds one DV. Edit freely: this file is not overwritten.
# Run just this topic with run_dv_suite(df, "outputs/R9/dv_suite_trial", topics = "test_all_verbs_csv").

steps <- list()

# var_a ----------------------------------------------------------------------
# specs/TEST_ALL_VERBS.csv, sheet Sheet1, row 3
#   source_value
steps$var_a <- dv_step(
  label = "Copy value",
  inputs = "source_value",
  derive = function(df) {
    dv_copy(
      df,
      source_col = "source_value",
      new_col = "var_a",
      sentinels = settings$sentinels
    )
  },
  uid = "dv_000001"
)

# row_total ------------------------------------------------------------------
# specs/TEST_ALL_VERBS.csv, sheet Sheet1, row 4
#   left_value + right_value
steps$row_total <- dv_step(
  label = "Row total",
  inputs = c("left_value", "right_value"),
  derive = function(df) {
    dv_row_total(
      df,
      value_cols = c("left_value", "right_value"),
      new_col = "row_total",
      na_as_zero = settings$na_as_zero,
      sentinels = settings$sentinels
    )
  },
  uid = "dv_000002"
)

# difference -----------------------------------------------------------------
# specs/TEST_ALL_VERBS.csv, sheet Sheet1, row 5
#   left_value - right_value
steps$difference <- dv_step(
  label = "Difference",
  inputs = c("left_value", "right_value"),
  derive = function(df) {
    dv_difference(
      df,
      minuend_col = "left_value",
      subtrahend_col = "right_value",
      new_col = "difference",
      sentinels = settings$sentinels_in_calculations
    )
  },
  uid = "dv_000003"
)

# positive_flag --------------------------------------------------------------
# specs/TEST_ALL_VERBS.csv, sheet Sheet1, row 6
#   IF score > 0 THEN positive_flag = 1 ELSE positive_flag = 0
steps$positive_flag <- dv_step(
  label = "Positive flag",
  inputs = "score",
  derive = function(df) {
    dv_flag_positive(
      df,
      value_col = "score",
      new_col = "positive_flag",
      threshold = 0,
      sentinels = settings$sentinels
    )
  },
  uid = "dv_000004"
)

# var_j ----------------------------------------------------------------------
# specs/TEST_ALL_VERBS.csv, sheet Sheet1, row 7
#   IF category IN (1, 2) THEN var_j = 1 ELSE var_j = 0
# Plan notes: confirm the drafted condition
steps$var_j <- dv_step(
  label = "Category flag",
  inputs = "category",
  derive = function(df) {
    dv_flag_if(
      df,
      condition = df$category %in% c(1, 2),
      new_col = "var_j",
      otherwise = 0
    )
  },
  uid = "dv_000005"
)

# any_choice -----------------------------------------------------------------
# specs/TEST_ALL_VERBS.csv, sheet Sheet1, row 8
#   IF ANY(2, choice_a, choice_b) THEN any_choice = 1 ELSE any_choice = 0
steps$any_choice <- dv_step(
  label = "Any choice",
  inputs = c("choice_a", "choice_b"),
  derive = function(df) {
    dv_flag_any_of(
      df,
      value_cols = c("choice_a", "choice_b"),
      match_values = 2,
      new_col = "any_choice"
    )
  },
  uid = "dv_000006"
)

# clean_value ----------------------------------------------------------------
# specs/TEST_ALL_VERBS.csv, sheet Sheet1, row 9
#   clean_value = raw_value; IF clean_value < 0 THEN clean_value = 0
# Plan notes: spec floors every negative at zero (decision D1)
steps$clean_value <- dv_step(
  label = "Clean value",
  inputs = "raw_value",
  derive = function(df) {
    dv_clear_sentinels(
      df,
      value_col = "raw_value",
      new_col = "clean_value",
      codes = settings$codes,
      replacement = settings$replacement
    )
  },
  uid = "dv_000007"
)

# ratio ----------------------------------------------------------------------
# specs/TEST_ALL_VERBS.csv, sheet Sheet1, row 10
#   IF denominator > 0 THEN ratio = numerator / denominator ELSE ratio = 0
steps$ratio <- dv_step(
  label = "Ratio",
  inputs = c("denominator", "numerator"),
  derive = function(df) {
    dv_ratio(
      df,
      numerator_col = "numerator",
      denominator_col = "denominator",
      new_col = "ratio",
      otherwise = 0,
      sentinels = settings$sentinels_in_calculations
    )
  },
  uid = "dv_000008"
)

# annualised -----------------------------------------------------------------
# specs/TEST_ALL_VERBS.csv, sheet Sheet1, row 11
#   IF period = 1 THEN annualised = amount * 52
#   IF period = 2 THEN annualised = amount * 26
#   IF period = 3 THEN annualised = amount * 12
# Plan notes: uses the spec's own period codes
steps$annualised <- dv_step(
  label = "Annualised amount",
  inputs = c("period", "amount"),
  derive = function(df) {
    dv_annualise(
      df,
      amount_col = "amount",
      period_col = "period",
      new_col = "annualised",
      target = "annual",
      period_codes = c(1, 2, 3),
      multipliers = c(52, 26, 12),
      sentinels = settings$sentinels
    )
  },
  uid = "dv_000009"
)

# midpoint -------------------------------------------------------------------
# specs/TEST_ALL_VERBS.csv, sheet Sheet1, row 12
#   Mid-point taken from gcontvbR9_i banded amount
steps$midpoint <- dv_step(
  label = "Band midpoint",
  inputs = "gcontvbR9_i",
  derive = function(df) {
    dv_band_midpoint(
      df,
      band_col = "gcontvbR9_i",
      new_col = "midpoint",
      table = "gcontvb",
      sentinels = settings$sentinels
    )
  },
  uid = "dv_000010"
)

# var_f ----------------------------------------------------------------------
# specs/TEST_ALL_VERBS.csv, sheet Sheet1, row 13
#   IF category = 2 THEN var_f = amount ELSE var_f = 0
# Plan notes: confirm the drafted condition
steps$var_f <- dv_step(
  label = "Kept amount",
  inputs = c("category", "amount"),
  derive = function(df) {
    dv_keep_if(
      df,
      condition = df$category == 2,
      value_col = "amount",
      new_col = "var_f",
      otherwise = 0,
      sentinels = settings$sentinels
    )
  },
  uid = "dv_000011"
)

# present_value --------------------------------------------------------------
# specs/TEST_ALL_VERBS.csv, sheet Sheet1, row 14
#   present_value = future_amount / (1 + 0.05) ^ (66 - age)
# Plan notes: confirm rate = 0.05 in settings.R; confirm target_age = 66 in settings.R
steps$present_value <- dv_step(
  label = "Present value",
  inputs = c("future_amount", "age"),
  derive = function(df) {
    dv_discount_to_present_value(
      df,
      amount_col = "future_amount",
      age_col = "age",
      new_col = "present_value",
      rate = settings$rate,
      target_age = settings$target_age,
      sentinels = settings$sentinels_in_calculations
    )
  },
  uid = "dv_000012"
)

# hh_total -------------------------------------------------------------------
# specs/TEST_ALL_VERBS.csv, sheet Sheet1, row 15
#   aggregate from person level (var_a)
steps$hh_total <- dv_step(
  label = "Household total",
  inputs = "var_a",
  derive = function(df) {
    dv_sum_to_household(
      df,
      value_col = "var_a",
      new_col = "hh_total",
      by = settings$by,
      na_as_zero = settings$na_as_zero,
      sentinels = settings$sentinels
    )
  },
  uid = "dv_000013"
)

# hh_any ---------------------------------------------------------------------
# specs/TEST_ALL_VERBS.csv, sheet Sheet1, row 16
#   MAX(positive_flag) (Aggregated from person level)
steps$hh_any <- dv_step(
  label = "Anyone positive",
  inputs = "positive_flag",
  derive = function(df) {
    dv_any_to_household(
      df,
      flag_col = "positive_flag",
      new_col = "hh_any",
      by = settings$by
    )
  },
  uid = "dv_000014"
)
