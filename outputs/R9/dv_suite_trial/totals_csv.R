# totals_csv DVs
# Written by autospec::write_dv_suite() on 2026-10-19.
# Each step builds one DV. Edit freely: this file is not overwritten.
# Run just this topic with run_dv_suite(df, "outputs/R9/dv_suite_trial", topics = "totals_csv").

steps <- list()

# f --------------------------------------------------------------------------
# specs/totals.csv, sheet Sheet1, row 2
#   F = D + E
steps$f <- dv_step(
  label = "D plus E",
  inputs = c("d", "e"),
  derive = function(df) {
    dv_row_total(
      df,
      value_cols = c("d", "e"),
      new_col = "f",
      na_as_zero = settings$na_as_zero,
      sentinels = settings$sentinels
    )
  },
  uid = "REQ-TOTAL-003"
)

# e --------------------------------------------------------------------------
# specs/totals.csv, sheet Sheet1, row 3
#   E = B + C
# Plan notes: unknown variable B, did you mean F?; unknown variable C, did you mean F?
steps$e <- dv_step(
  label = "B plus C",
  inputs = c("b", "c"),
  derive = function(df) {
    dv_row_total(
      df,
      value_cols = c("b", "c"),
      new_col = "e",
      na_as_zero = settings$na_as_zero,
      sentinels = settings$sentinels
    )
  },
  uid = "REQ-TOTAL-002"
)

# d --------------------------------------------------------------------------
# specs/totals.csv, sheet Sheet1, row 4
#   D = A + B
# Plan notes: unknown variable A, did you mean F?; unknown variable B, did you mean F?
steps$d <- dv_step(
  label = "A plus B",
  inputs = c("a", "b"),
  derive = function(df) {
    dv_row_total(
      df,
      value_cols = c("a", "b"),
      new_col = "d",
      na_as_zero = settings$na_as_zero,
      sentinels = settings$sentinels
    )
  },
  uid = "REQ-TOTAL-001"
)
