# test_csv DVs
# Written by autospec::write_dv_suite() on 2026-10-19.
# Each step builds one DV. Edit freely: this file is not overwritten.
# Run just this topic with run_dv_suite(df, "outputs/R9/dv_suite_trial", topics = "test_csv").

steps <- list()

# var_c1 ---------------------------------------------------------------------
# specs/TEST.csv, sheet Sheet1, row 8
#   IF var_a > 0  AND var_b1 > 0 AND var_d1 > 0 THEN
#   var_c1 = var_b1 - YearR9
# Not written yet: no verb fits this derivation
# Plan notes: no verb matches this shape; unknown variable var_b1, did you mean var_c1?; unknown variable var_d1, did you mean var_c1?; unknown variable YearR9; Difference in years between the expected repayment end date and interview year.
steps$var_c1 <- dv_step(
  label = "Years until expected Mail Order 1 repayment date",
  inputs = c(
    "var_a",
    "var_b1",
    "var_d1",
    "YearR9"
  ),
  derive = NULL, # replace NULL with function(df) { ... } that returns df with the DV added
  uid = "dv_000001"
)

# var_c2 ---------------------------------------------------------------------
# specs/TEST.csv, sheet Sheet1, row 8
#   IF var_a > 0  AND var_b2 > 0 AND var_d2 > 0 THEN
#   var_c2 = var_b2 - YearR9
# Not written yet: no verb fits this derivation
# Plan notes: no verb matches this shape; unknown variable var_b2, did you mean var_c2?; unknown variable var_d2, did you mean var_c2?; unknown variable YearR9; Difference in years between the expected repayment end date and interview year.
steps$var_c2 <- dv_step(
  label = "Years until expected Mail Order 2 repayment date",
  inputs = c(
    "var_a",
    "var_b2",
    "var_d2",
    "YearR9"
  ),
  derive = NULL, # replace NULL with function(df) { ... } that returns df with the DV added
  uid = "dv_000002"
)

# var_e1 ---------------------------------------------------------------------
# specs/TEST.csv, sheet Sheet1, row 9
#   IF var_a > 0  AND var_b1 > 0 AND var_d1 > 0 THEN
#   var_e1 = var_d1 - MonthR9
# Not written yet: no verb fits this derivation
# Plan notes: no verb matches this shape; unknown variable var_b1, did you mean var_c1?; unknown variable var_d1, did you mean var_c1?; unknown variable MonthR9; Difference in months between the expected repayment end date and interview month.
steps$var_e1 <- dv_step(
  label = "Month offset to expected Mail Order 1 repayment date",
  inputs = c(
    "var_a",
    "var_b1",
    "var_d1",
    "MonthR9"
  ),
  derive = NULL, # replace NULL with function(df) { ... } that returns df with the DV added
  uid = "dv_000003"
)

# var_e2 ---------------------------------------------------------------------
# specs/TEST.csv, sheet Sheet1, row 9
#   IF var_a > 0  AND var_b2 > 0 AND var_d2 > 0 THEN
#   var_e2 = var_d2 - MonthR9
# Not written yet: no verb fits this derivation
# Plan notes: no verb matches this shape; unknown variable var_b2, did you mean var_c2?; unknown variable var_d2, did you mean var_c2?; unknown variable MonthR9; Difference in months between the expected repayment end date and interview month.
steps$var_e2 <- dv_step(
  label = "Month offset to expected Mail Order 2 repayment date",
  inputs = c(
    "var_a",
    "var_b2",
    "var_d2",
    "MonthR9"
  ),
  derive = NULL, # replace NULL with function(df) { ... } that returns df with the DV added
  uid = "dv_000004"
)

# var_h1 ---------------------------------------------------------------------
# specs/TEST.csv, sheet Sheet1, row 10
#   IF var_c1 ==0  AND  var_e1 ==0 THEN
#       var_h1 = 1
#   ELIF var_c1 ==0  AND  var_e1 ==-1 THEN
#       var_h1 = 1
#   ELSE var_h1 = var_c1*12 + var_e1
# Not written yet: no verb fits this derivation
# Plan notes: no verb matches this shape; Where var_h = 0 (as year and month of interview equals year and month of final payment of mail order) set var_h = 1 to allow for final payment
steps$var_h1 <- dv_step(
  label = "Number of monthly installments left to pay mail order",
  inputs = c("var_c1", "var_e1"),
  derive = NULL, # replace NULL with function(df) { ... } that returns df with the DV added
  uid = "dv_000005"
)

# var_h2 ---------------------------------------------------------------------
# specs/TEST.csv, sheet Sheet1, row 10
#   IF var_c2 ==0  AND  var_e2 ==0 THEN
#       var_h2 = 1
#   ELIF var_c2 ==0  AND  var_e2 ==-1 THEN
#       var_h2 = 1
#   ELSE var_h2 = var_c2*12 + var_e2
# Not written yet: no verb fits this derivation
# Plan notes: no verb matches this shape; Where var_h = 0 (as year and month of interview equals year and month of final payment of mail order) set var_h = 1 to allow for final payment
steps$var_h2 <- dv_step(
  label = "Number of monthly installments left to pay mail order",
  inputs = c("var_c2", "var_e2"),
  derive = NULL, # replace NULL with function(df) { ... } that returns df with the DV added
  uid = "dv_000006"
)

# var_f1 ---------------------------------------------------------------------
# specs/TEST.csv, sheet Sheet1, row 14
#   IF var_g1 = 1 THEN
#       var_f1 = var_h1*(var_i1*52/12)
#   IF var_g1 = 2 THEN
#       var_f1 = var_h1 * (var_i1*26/12)
#       IF var_g1 = 3 THEN
#           var_f1 = var_h1 * var_i1
#       IF var_g1 = 4 THEN
#           var_f1 = var_h1 * (var_i1*13/24)
#       IF var_g1 = 5 THEN
#           var_f1 = var_h1 * (var_i1*(4/12))
#       IF var_g1 = 6 THEN
#            var_f1 = var_i1
# Not written yet: no verb fits this derivation
# Plan notes: no verb matches this shape; unknown variable var_g1, did you mean var_c1?; unknown variable var_i1, did you mean var_c1?
steps$var_f1 <- dv_step(
  label = "Amount outstanding on 1 mail order account",
  inputs = c("var_g1", "var_h1", "var_i1"),
  derive = NULL, # replace NULL with function(df) { ... } that returns df with the DV added
  uid = "dv_000007"
)

# var_f2 ---------------------------------------------------------------------
# specs/TEST.csv, sheet Sheet1, row 14
#   IF var_g2 = 1 THEN
#       var_f2 = var_h2*(var_i2*52/12)
#   IF var_g2 = 2 THEN
#       var_f2 = var_h2 * (var_i2*26/12)
#       IF var_g2 = 3 THEN
#           var_f2 = var_h2 * var_i2
#       IF var_g2 = 4 THEN
#           var_f2 = var_h2 * (var_i2*13/24)
#       IF var_g2 = 5 THEN
#           var_f2 = var_h2 * (var_i2*(4/12))
#       IF var_g2 = 6 THEN
#            var_f2 = var_i2
# Not written yet: no verb fits this derivation
# Plan notes: no verb matches this shape; unknown variable var_g2, did you mean var_c2?; unknown variable var_i2, did you mean var_c2?
steps$var_f2 <- dv_step(
  label = "Amount outstanding on 2 mail order account",
  inputs = c("var_g2", "var_h2", "var_i2"),
  derive = NULL, # replace NULL with function(df) { ... } that returns df with the DV added
  uid = "dv_000008"
)

# var_m ----------------------------------------------------------------------
# specs/TEST.csv, sheet Sheet1, row 23
#   var_m = var_f(1) +  var_f(2)
# Not written yet: no verb fits this derivation
# Plan notes: no verb matches this shape
steps$var_m <- dv_step(
  label = "Total outstanding on mail order",
  inputs = character(),
  derive = NULL, # replace NULL with function(df) { ... } that returns df with the DV added
  uid = "dv_000010"
)

# var_p ----------------------------------------------------------------------
# specs/TEST.csv, sheet Sheet1, row 27
#   IF ANY(2, var_o(1-2)) then var_p = 1 (otherwise 0)
# Plan notes: OTHERWISE written in brackets; value with no variable name inside a branch; unknown variable var_o1, did you mean var_c1?; unknown variable var_o2, did you mean var_c2?
steps$var_p <- dv_step(
  label = "Whether has arrears on Mail Order account",
  inputs = c("var_o1", "var_o2"),
  derive = function(df) {
    dv_flag_any_of(
      df,
      value_cols = c("var_o1", "var_o2"),
      match_values = 2,
      new_col = "var_p"
    )
  },
  uid = "dv_000011"
)

# var_s1 ---------------------------------------------------------------------
# specs/TEST.csv, sheet Sheet1, row 28
#   var_s1 = var_r1
# Plan notes: unknown variable var_r1, did you mean var_c1?
steps$var_s1 <- dv_step(
  label = "Total amount owed 1 mail order arrears",
  inputs = "var_r1",
  derive = function(df) {
    dv_copy(
      df,
      source_col = "var_r1",
      new_col = "var_s1",
      sentinels = settings$sentinels
    )
  },
  uid = "dv_000012"
)

# var_s2 ---------------------------------------------------------------------
# specs/TEST.csv, sheet Sheet1, row 28
#   var_s2 = var_r2
# Plan notes: unknown variable var_r2, did you mean var_c2?
steps$var_s2 <- dv_step(
  label = "Total amount owed 2 mail order arrears",
  inputs = "var_r2",
  derive = function(df) {
    dv_copy(
      df,
      source_col = "var_r2",
      new_col = "var_s2",
      sentinels = settings$sentinels
    )
  },
  uid = "dv_000013"
)

# var_t ----------------------------------------------------------------------
# specs/TEST.csv, sheet Sheet1, row 29
#   var_t = var_s(1) + var_s(2)
# Not written yet: no verb fits this derivation
# Plan notes: no verb matches this shape; NR TO CHECK: Just check about BNPL vs. MO etc. and R8-R9
steps$var_t <- dv_step(
  label = "Total outstanding on mail order arrears",
  inputs = character(),
  derive = NULL, # replace NULL with function(df) { ... } that returns df with the DV added
  uid = "dv_000014"
)

# var_n ----------------------------------------------------------------------
# specs/TEST.csv, sheet Sheet1, row 23
#   aggregate from person level (var_m)
steps$var_n <- dv_step(
  label = "Hhold value of outstanding mail order accounts",
  inputs = "var_m",
  derive = function(df) {
    dv_sum_to_household(
      df,
      value_col = "var_m",
      new_col = "var_n",
      by = settings$by,
      na_as_zero = settings$na_as_zero,
      sentinels = settings$sentinels
    )
  },
  uid = "dv_000015"
)

# var_u ----------------------------------------------------------------------
# specs/TEST.csv, sheet Sheet1, row 29
#   aggregate from person level (var_t)
steps$var_u <- dv_step(
  label = "Hhold value of mail order arrears",
  inputs = "var_t",
  derive = function(df) {
    dv_sum_to_household(
      df,
      value_col = "var_t",
      new_col = "var_u",
      by = settings$by,
      na_as_zero = settings$na_as_zero,
      sentinels = settings$sentinels
    )
  },
  uid = "dv_000016"
)
