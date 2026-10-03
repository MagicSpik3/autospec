test_that("inputs exclude what the derivation writes and keep the original case", {
  analysis <- analyse_derivation(
    "if OthMortR9 > 0 then PropOthMortR9 =1;  (otherwise PropOthMortR9 =0 )",
    dv = "PropOthMortR9"
  )

  expect_identical(analysis$kind, "parsed")
  expect_identical(analysis$inputs, "OthMortR9")
  expect_identical(analysis$outputs, "PropOthMortR9")
})


test_that("prose is never reported as an input", {
  analysis <- analyse_derivation(
    "Mid-point taken from UGdVbSR9_i banded amount (upper band should be coded as 300,000)",
    dv = "HousGdsTR9"
  )

  expect_identical(analysis$kind, "midpoint")
  expect_identical(analysis$inputs, "UGdVbSR9_i")
})


test_that("inputs are put into their known spelling", {
  analysis <- analyse_derivation(
    "TOTCSCR9_SUM = SUM(TOTCSC_transR9_SUM + TOTCSC_persisR9_SUM)",
    dv = "TOTCSCR9_SUM",
    known_names = c("TOTCSC_transR9_SUM", "TOTCSC_persisR9_sum")
  )

  expect_identical(analysis$inputs, c("TOTCSC_transR9_SUM", "TOTCSC_persisR9_sum"))
})


test_that("numbered ranges survive as whole names", {
  analysis <- analyse_derivation(
    "IF DCSCamOS(1-5)R9_i = -9 THEN DCSCamOS(1-5)R9 = 0 OTHERWISE DCSCamOS(1-5)R9 = DCSCamOS(1-5)R9_i",
    dv = "DCSCamOS(1-5)R9"
  )

  expect_identical(analysis$inputs, "DCSCamOS(1-5)R9_i")
})


test_that("an implied target is recorded as the DV", {
  analysis <- analyse_derivation("If p = 1 then = amt * 52", dv = "DVoinRrAnnualr9")

  expect_identical(analysis$outputs, "DVoinRrAnnualr9")
})


test_that("a stray term with no operator is noted", {
  analysis <- analyse_derivation("HFINLR9_SUM = a + b\nDVFEOptVR9", dv = "HFINLR9_SUM")

  expect_true(any(grepl("stray term with no operator: DVFEOptVR9", analysis$notes)))
})


test_that("value labels and input blocks are not derivations", {
  expect_identical(analyse_derivation("1.0: 'Employee'", dv = "x")$kind, "value_labels")
  expect_identical(analyse_derivation("a = b", dv = "x", level = "input")$kind, "value_labels")
})


test_that("a parse failure is reported with its error", {
  analysis <- analyse_derivation("x = (a, b)", dv = "x")

  expect_identical(analysis$kind, "unparsed")
  expect_false(is.na(analysis$parse_error))
})
