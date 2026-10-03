test_that("a plain rename matches dv_copy", {
  match <- match_verb("DVCISAvR9 = FCISAvR9_i", "DVCISAvR9", "person")

  expect_identical(match$verb, "dv_copy")
  expect_identical(match$status, "auto")
  expect_identical(match$args$source_col, "FCISAvR9_i")
  expect_identical(match$inputs, "FCISAvR9_i")
})


test_that("adding named columns matches dv_row_total", {
  match <- match_verb("DVISAValR9_SUM = DVCISAvR9 + DVIISAvR9 + DVKISAvR9",
                      "DVISAValR9_SUM", "person")

  expect_identical(match$verb, "dv_row_total")
  expect_identical(match$args$value_cols, c("DVCISAvR9", "DVIISAvR9", "DVKISAvR9"))
})


test_that("aggregating to household matches dv_sum_to_household however it is worded", {
  wordings <- c(
    "aggregate from person level (DVCISAvR9)",
    "SUM(DVCISAvR9) - Aggregated from person level",
    "DVCISAvR9_aggr = SUM(DVCISAvR9)"
  )

  for (wording in wordings) {
    match <- match_verb(wording, "DVCISAvR9_aggr", "household")

    expect_identical(match$verb, "dv_sum_to_household", info = wording)
    expect_identical(match$args$value_col, "DVCISAvR9", info = wording)
  }
})


test_that("SUM of one column at person level is not a household total", {
  match <- match_verb("SUM(DVCISAvR9)", "Something", "person")

  expect_false(identical(match$verb, "dv_sum_to_household"))
})


test_that("MAX of a flag at household level matches dv_any_to_household", {
  match <- match_verb("Max (PropDVHseValR9) (Aggregated from person level)",
                      "PropDVHseValR9_Aggr", "household")

  expect_identical(match$verb, "dv_any_to_household")
  expect_identical(match$args$flag_col, "PropDVHseValR9")
})


test_that("a positive-value flag matches dv_flag_positive", {
  match <- match_verb(
    "if DVHseValR9 > 0 then PropDVHseValR9 =1;  (otherwise PropDVHseValR9 =0 )",
    "PropDVHseValR9", "person"
  )

  expect_identical(match$verb, "dv_flag_positive")
  expect_identical(match$status, "auto")
  expect_identical(match$args$value_col, "DVHseValR9")
  expect_identical(match$args$threshold, 0)
})


test_that("a compound condition drafts dv_flag_if with an R condition", {
  match <- match_verb(
    "IF CommiR9 in (4,5) THEN BillCredKeepNoR9=1; ELSE BillCredKeepNoR9=0;",
    "BillCredKeepNoR9", "person"
  )

  expect_identical(match$verb, "dv_flag_if")
  expect_identical(match$status, "needs_review")
  expect_identical(match$condition, "CommiR9 %in% c(4, 5)")
  expect_identical(match$args$otherwise, 0)
})


test_that("a flag with no else branch is drafted with otherwise = 0 and decision D5", {
  match <- match_verb("IF FTypeAcc_binary1r9_i = 1\n    CurrAccR9=1.", "CurrAccR9", "person")

  expect_identical(match$verb, "dv_flag_if")
  expect_identical(match$args$otherwise, 0)
  expect_true(any(grepl("decision D5", match$reasons)))
})


test_that("ANY across numbered columns matches dv_flag_any_of", {
  match <- match_verb(
    "IF ANY(2, DLBeh(1-3)R9_i) THEN DVHasLnArR9 = 1 ELSE DVHasLnArR9 = 0",
    "DVHasLnArR9", "person"
  )

  expect_identical(match$verb, "dv_flag_any_of")
  expect_identical(match$args$value_cols, "DLBeh(1-3)R9_i")
  expect_identical(match$args$match_values, 2)
})


test_that("copy then floor at zero matches dv_clear_sentinels with decision D1", {
  match <- match_verb(
    "DVHseValR9 = UValSR9_i;\nif DVHseValR9 < 0 then DVHseValR9 = 0",
    "DVHseValR9", "person"
  )

  expect_identical(match$verb, "dv_clear_sentinels")
  expect_identical(match$args$value_col, "UValSR9_i")
  expect_identical(match$inputs, "UValSR9_i")
  expect_true(any(grepl("decision D1", match$reasons)))
})


test_that("a guarded division matches dv_ratio", {
  match <- match_verb("if D > 0 then R = N / D; else R = 0;", "R", "household")

  expect_identical(match$verb, "dv_ratio")
  expect_identical(match$args$numerator_col, "N")
  expect_identical(match$args$denominator_col, "D")
})


test_that("a period table matches dv_annualise for review", {
  text <- paste(
    "IF p = 1 THEN d = amt * 52",
    "IF p = 2 THEN d = amt * 26",
    "IF p = 5 THEN d = amt * 12",
    sep = "\n"
  )

  match <- match_verb(text, "d", "person")

  expect_identical(match$verb, "dv_annualise")
  expect_identical(match$status, "needs_review")
  expect_identical(match$args$amount_col, "amt")
  expect_identical(match$args$period_col, "p")
  expect_identical(match$args$target, "annual")
})


test_that("a monthly period table is recognised", {
  text <- paste(
    "IF oft = 1 THEN d = (ins * 52) / 12",
    "IF oft = 2 THEN d = (ins * 26) / 12",
    "IF oft = 3 THEN d = ins",
    sep = "\n"
  )

  expect_identical(match_verb(text, "d", "person")$args$target, "monthly")
})


test_that("the spec's own period table is copied into the arguments", {
  text <- paste(
    "IF oft = 1 THEN d = (ins * 52) / 12",
    "IF oft = 2 THEN d = (ins * 26) / 12",
    "IF oft = 3 THEN d = ins",
    sep = "\n"
  )

  match <- match_verb(text, "d", "person")

  expect_identical(match$args$period_codes, c(1, 2, 3))
  expect_equal(match$args$multipliers, c(52 / 12, 26 / 12, 1))
  expect_true(any(grepl("spec's own period codes", match$reasons)))
})


test_that("a band midpoint matches dv_band_midpoint with its table", {
  match <- match_verb(
    "Mid-point taken from UGdVbSR9_i banded amount (upper band should be coded as 300,000)",
    "HousGdsTR9", "person"
  )

  expect_identical(match$verb, "dv_band_midpoint")
  expect_identical(match$status, "auto")
  expect_identical(match$args$table, "ugdvbs")
})


test_that("a selection matches dv_keep_if with a drafted condition", {
  match <- match_verb(
    "IF DLType1R9_i = 2 THEN SLNS1R9 = DSLamt1R9_i ELSE SLNS1R9 = 0",
    "SLNS1R9", "person"
  )

  expect_identical(match$verb, "dv_keep_if")
  expect_identical(match$args$value_col, "DSLamt1R9_i")
  expect_identical(match$condition, "DLType1R9_i == 2")
})


test_that("a condition with a range that does not match the DV is left for a person", {
  match <- match_verb(
    "IF FTypeAcc_binary(1-3)r9_i = 0 AND FTypeInv_binary(1-6)r9_i=0 THEN HasNoFAR9=1.",
    "HasNoFAR9", "person"
  )

  expect_identical(match$verb, "dv_flag_if")
  expect_true(is.na(match$condition))
  expect_true(any(grepl("write it by hand", match$reasons)))
})


test_that("1/2 coding is left for hand-written code with decision D7", {
  match <- match_verb(
    "If a = 1 THEN FrstSchR9=1.\nOtherwise, FrstSchR9=2.",
    "FrstSchR9", "person"
  )

  expect_identical(match$status, "hand_written")
  expect_true(any(grepl("decision D7", match$reasons)))
})


test_that("value labels, upstream notes and R code are not matched", {
  expect_identical(
    match_verb("1.0: 'Yes', 2.0: 'No'", "SJob2R8", "input")$status,
    "not_a_derivation"
  )
  expect_identical(
    match_verb("Imputed from derived DVUniAmtAnnualR9_i (the annualised version)",
               "DVUniAmtAnnualR9", "person")$status,
    "not_a_derivation"
  )
  expect_identical(
    match_verb("ANNUALISE <- function(amt, pd) {", "DVUGrsPayAnnualR9_i", "person")$status,
    "hand_written"
  )
})


test_that("an unparseable derivation needs review and says why", {
  match <- match_verb("x = (a, b)", "x", "person")

  expect_identical(match$status, "needs_review")
  expect_true(is.na(match$verb))
  expect_false(is.na(match$parse_error))
})


test_that("a derivation that assigns to a different name is noted", {
  match <- match_verb(
    "IF DLType1R9_i = 3 THEN DVHasSLBR9 = 1 ELSE DVHasSLBR9 = 0",
    "DVHasSBLR9", "person"
  )

  expect_true(any(grepl("assigns to DVHasSLBR9, not DVHasSBLR9", match$notes)))
})


test_that("every verb the matcher proposes is in the registry", {
  texts <- list(
    c("a = b", "a", "person"),
    c("t = a + b", "t", "person"),
    c("SUM(x)", "x_aggr", "household"),
    c("IF x > 0 THEN f = 1 ELSE f = 0", "f", "person"),
    c("IF x = 1 THEN f = 1 ELSE f = 0", "f", "person"),
    c("if d > 0 then r = n / d; else r = 0;", "r", "household")
  )

  for (item in texts) {
    verb <- match_verb(item[[1L]], item[[2L]], item[[3L]])$verb
    expect_true(verb %in% registered_verbs(), info = item[[1L]])
  }
})


test_that("dv must be a single name", {
  expect_error(match_verb("a = b", NA_character_))
})
