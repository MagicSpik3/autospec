make_catalogue_rows <- function(variable, instructions, level = "person",
                                sheet_name = "Sheet1") {
  data.table::data.table(
    file_name = "spec.xlsx",
    sheet_name = sheet_name,
    level = level,
    excel_row = seq_along(variable) + 1L,
    variable = variable,
    label = NA_character_,
    instructions = instructions
  )
}


test_that("a plan has the plan columns and one row per catalogue row", {
  catalogue <- make_catalogue_rows(
    c("DVCISAvR9", "DVCISAvR9_aggr"),
    c("DVCISAvR9 = FCISAvR9_i", "aggregate from person level (DVCISAvR9)"),
    level = c("person", "household")
  )

  plan <- build_dv_plan(catalogue)

  expect_identical(names(plan), plan_columns())
  expect_identical(plan$verb, c("dv_copy", "dv_sum_to_household"))
  expect_identical(plan$args[[1L]], "source_col = \"FCISAvR9_i\"")
  expect_identical(plan$status, c("auto", "auto"))
  expect_true(all(is.na(plan$reviewed_by)))
})


test_that("ranged outputs are split into atomic rows with a generated UID", {
  catalogue <- make_catalogue_rows(
    "MOyr(1-2)R9",
    "IF DMOnumR9_i > 0 AND DMoWhnY(1-2)R9_i > 0 THEN MOyr(1-2)R9 = DMOwhnY(1-2)R9_i - YearR9"
  )

  plan <- suppressMessages(build_dv_plan(catalogue))

  expect_identical(plan$dv, c("MOyr1R9", "MOyr2R9"))
  expect_true(all(grepl("^dv_\\d{6}$", plan$uid)))
  expect_true(grepl("MOyr1R9", plan$instructions[[1L]]))
  expect_true(grepl("MOyr2R9", plan$instructions[[2L]]))
})


test_that("existing atomic UIDs are preserved when drafting the plan", {
  catalogue <- make_catalogue_rows(
    c("MOyr1R9", "MOyr2R9"),
    c(
      "IF DMOnumR9_i > 0 AND DMoWhnY(1-2)R9_i > 0 THEN MOyr1R9 = DMOwhnY1R9_i - YearR9",
      "IF DMOnumR9_i > 0 AND DMoWhnY(1-2)R9_i > 0 THEN MOyr2R9 = DMOwhnY2R9_i - YearR9"
    )
  )
  catalogue$uid <- c("dv_000001", "dv_000002")

  plan <- suppressMessages(build_dv_plan(catalogue))

  expect_identical(plan$uid, c("dv_000001", "dv_000002"))
})


test_that("a source UID expands deterministically for ranged outputs", {
  catalogue <- make_catalogue_rows(
    "MOyr(1-2)R9",
    "IF amount > 0 THEN MOyr(1-2)R9 = amount"
  )
  catalogue$uid <- "REQ-MAIL-ORDER"

  plan <- suppressMessages(build_dv_plan(catalogue))

  expect_identical(plan$uid, c("REQ-MAIL-ORDER_1", "REQ-MAIL-ORDER_2"))
})


test_that("duplicate source UIDs are rejected", {
  catalogue <- make_catalogue_rows(c("a", "b"), c("a = x", "b = y"))
  catalogue$uid <- c("REQ-DUPLICATE", "REQ-DUPLICATE")

  expect_error(suppressMessages(build_dv_plan(catalogue)), "UIDs are not unique")
})


test_that("arguments are written as readable R code", {
  expect_identical(
    format_plan_args(list(value_cols = c("a", "b"), threshold = 0.2, otherwise = NA)),
    "value_cols = c(\"a\", \"b\"), threshold = 0.2, otherwise = NA"
  )
})


test_that("a DV defined twice with the same text has one row to sign off", {
  catalogue <- make_catalogue_rows(
    c("DVNIINVr9", "DVNIINVr9"),
    c("DVNIINVr9= FIncVr9_i", "DVNIINVr9= FIncVr9_i"),
    sheet_name = c("R9_Other_Invest_Pension_Ben", "R9_Other_Income")
  )

  plan <- suppressMessages(build_dv_plan(catalogue))

  expect_identical(plan$status, c("auto", "duplicate"))
  expect_match(plan$notes[[1L]], "also defined on R9_Other_Income row 3 with the same text; this is the row to sign off")
  expect_match(plan$notes[[2L]], "same as R9_Other_Invest_Pension_Ben row 2, which is the row to sign off")
})


test_that("a DV defined twice with different text is left for the reviewer", {
  catalogue <- make_catalogue_rows(
    c("DVGiPPenr9", "DVGiPPenr9"),
    c("DVGiPPenr9 = a", "DVGiPPenr9 = b"),
    sheet_name = c("R9_Other_Invest_Pension_Ben", "R9_Pension_Income")
  )

  plan <- suppressMessages(build_dv_plan(catalogue))

  expect_identical(plan$status, c("needs_review", "needs_review"))
  expect_true(all(grepl("with different text: sign off the right one", plan$notes)))
})


test_that("a DV signed off on both rows keeps one sign-off when rebuilt", {
  catalogue <- make_catalogue_rows(
    c("DVNIINVr9", "DVNIINVr9"),
    c("DVNIINVr9= FIncVr9_i", "DVNIINVr9= FIncVr9_i"),
    sheet_name = c("Sheet1", "Sheet2")
  )

  previous <- suppressMessages(build_dv_plan(catalogue))
  previous$status <- "reviewed"
  previous$reviewed_by <- "JD"

  rebuilt <- suppressMessages(build_dv_plan(catalogue, previous_plan = previous))

  expect_identical(rebuilt$status, c("reviewed", "duplicate"))
  expect_identical(rebuilt$reviewed_by, c("JD", NA))
})


test_that("the reviewer's choice of row to sign off is kept when rebuilt", {
  catalogue <- make_catalogue_rows(
    c("DVNIINVr9", "DVNIINVr9"),
    c("DVNIINVr9= FIncVr9_i", "DVNIINVr9= FIncVr9_i"),
    sheet_name = c("Sheet1", "Sheet2")
  )

  previous <- suppressMessages(build_dv_plan(catalogue))
  previous$status <- c("duplicate", "reviewed")

  rebuilt <- suppressMessages(build_dv_plan(catalogue, previous_plan = previous))

  expect_identical(rebuilt$status, c("duplicate", "reviewed"))
})


test_that("reading a plan says how far review has got", {
  catalogue <- make_catalogue_rows(c("a", "b", "c"), c("a = x", "b = x", "c = x"))
  plan <- suppressMessages(build_dv_plan(catalogue))
  plan$status <- c("reviewed", "auto", "duplicate")
  path <- tempfile(fileext = ".csv")
  suppressMessages(write_dv_plan(plan, path))

  messages <- character()
  withCallingHandlers(read_dv_plan(path), message = function(condition) {
    messages <<- c(messages, conditionMessage(condition))
    invokeRestart("muffleMessage")
  })

  expect_true(any(grepl("1 signed off", messages)))
  expect_true(any(grepl("1 still to sign off", messages)))
  expect_true(any(grepl("1 duplicate", messages)))
})


test_that("a near miss of a known DV name is reported", {
  catalogue <- make_catalogue_rows(
    c("DVFLfFSVR9", "DVFLfSVR9_aggr"),
    c("DVFLfFSVR9 = FLfFSVR9_i", "aggregate from person level (DVFLfSVR9)"),
    level = c("person", "household")
  )

  plan <- build_dv_plan(catalogue)

  expect_match(plan$notes[[2L]], "did you mean DVFLfFSVR9")
})


test_that("every unknown input is reported when the data's names are supplied", {
  catalogue <- make_catalogue_rows("DVCISAvR9", "DVCISAvR9 = FCISAvR9_i")

  plan <- build_dv_plan(catalogue, data_names = "SomethingElse")

  expect_match(plan$notes[[1L]], "unknown variable FCISAvR9_i")
})


test_that("a signed-off row is kept when the spec text has not changed", {
  catalogue <- make_catalogue_rows("DVCISAvR9", "DVCISAvR9 = FCISAvR9_i")

  previous <- build_dv_plan(catalogue)
  previous$status <- "reviewed"
  previous$reviewed_by <- "JD"
  previous$reviewed_on <- "2026-09-14"
  previous$args <- "source_col = \"FCISAvR9_i_edited\""

  rebuilt <- build_dv_plan(catalogue, previous_plan = previous)

  expect_identical(rebuilt$status, "reviewed")
  expect_identical(rebuilt$reviewed_by, "JD")
  expect_identical(rebuilt$args, "source_col = \"FCISAvR9_i_edited\"")
})


test_that("a signed-off row is redrafted when the spec text has changed", {
  catalogue <- make_catalogue_rows("DVCISAvR9", "DVCISAvR9 = FCISAvR9_i")

  previous <- build_dv_plan(catalogue)
  previous$status <- "reviewed"
  previous$reviewed_by <- "JD"

  changed <- make_catalogue_rows("DVCISAvR9", "DVCISAvR9 = FCISAvR9_new_i")
  rebuilt <- build_dv_plan(changed, previous_plan = previous)

  expect_identical(rebuilt$status, "auto")
  expect_true(is.na(rebuilt$reviewed_by))
  expect_match(rebuilt$notes, "reviewed by JD")
})


test_that("a plan survives a round trip through CSV", {
  catalogue <- make_catalogue_rows(
    c("a", "b"),
    c("a = x", "IF x > 0 THEN\n  b = 1\nELSE\n  b = 0")
  )

  plan <- build_dv_plan(catalogue)
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path))

  write_dv_plan(plan, path)
  read_back <- read_dv_plan(path)

  expect_identical(read_back$instructions, plan$instructions)
  expect_identical(read_back$args, plan$args)
  expect_identical(read_back$excel_row, plan$excel_row)
})


test_that("an unrecognised status is refused", {
  catalogue <- make_catalogue_rows("a", "a = x")
  plan <- build_dv_plan(catalogue)
  plan$status <- "signed"

  expect_error(check_plan_columns(plan), "unrecognised")
})


test_that("the catalogue must have the columns the plan needs", {
  expect_error(build_dv_plan(data.frame(variable = "a")), "missing")
})


test_that("sheet_priority settles a DV defined on two sheets, whatever the text", {
  catalogue <- make_catalogue_rows(
    c("DVGiPPenr9", "DVGiPPenr9"),
    c("DVGiPPenr9 = a", "DVGiPPenr9 = b"),
    sheet_name = c("R9_Other_Invest_Pension_Ben ", "R9_Pension_Income")
  )

  plan <- suppressMessages(build_dv_plan(catalogue, sheet_priority = "R9_Pension_Income"))

  expect_identical(plan$status, c("duplicate", "auto"))
  expect_match(plan$notes[[1L]], "preferred by sheet_priority")
  expect_false(grepl("different text", plan$notes[[2L]]))
})


test_that("sheet_priority matches sheet names ignoring surrounding spaces", {
  catalogue <- make_catalogue_rows(
    c("DVGiPPenr9", "DVGiPPenr9"),
    c("DVGiPPenr9 = a", "DVGiPPenr9 = b"),
    sheet_name = c("R9_Other_Invest_Pension_Ben ", "R9_Pension_Income")
  )

  plan <- suppressMessages(build_dv_plan(catalogue, sheet_priority = "R9_Other_Invest_Pension_Ben"))

  expect_identical(plan$status, c("auto", "duplicate"))
})


test_that("a DV on sheets sheet_priority does not list is left for the reviewer", {
  catalogue <- make_catalogue_rows(c("other", "other"), c("other = x", "other = y"),
                                   sheet_name = c("Sheet1", "Sheet2"))

  plan <- suppressMessages(build_dv_plan(catalogue, sheet_priority = "R9_Pension_Income"))

  expect_identical(plan$status, c("needs_review", "needs_review"))
})


test_that("a sign-off overruled by sheet_priority is flagged on the preferred row", {
  catalogue <- make_catalogue_rows(
    c("DVGiPPenr9", "DVGiPPenr9"),
    c("DVGiPPenr9 = a", "DVGiPPenr9 = b"),
    sheet_name = c("First", "Second")
  )
  previous <- suppressMessages(build_dv_plan(catalogue))
  previous$status[[1L]] <- "reviewed"
  previous$reviewed_by[[1L]] <- "JD"

  plan <- suppressMessages(build_dv_plan(catalogue, previous_plan = previous, sheet_priority = "Second"))

  expect_identical(plan$status, c("duplicate", "needs_review"))
  expect_match(plan$notes[[2L]], "was signed off on First row 2 with different code")
})


test_that("a sign-off moves to the preferred row when both write the same code", {
  catalogue <- make_catalogue_rows(
    c("DVGiPPenr9", "DVGiPPenr9"),
    c("DVGiPPenr9 = a", "DVGiPPenr9 =  a "),
    sheet_name = c("First", "Second")
  )
  previous <- suppressMessages(build_dv_plan(catalogue))
  previous$status <- c("reviewed", "duplicate")
  previous$reviewed_by <- c("JD", NA)

  plan <- suppressMessages(build_dv_plan(catalogue, previous_plan = previous, sheet_priority = "Second"))

  expect_identical(plan$status, c("duplicate", "reviewed"))
  expect_identical(plan$reviewed_by, c(NA, "JD"))
})


test_that("reading a plan warns about sign-offs with no name and on rows that are not derivations", {
  catalogue <- make_catalogue_rows(c("a", "b"), c("a = x", "b = x"))
  plan <- suppressMessages(build_dv_plan(catalogue))
  plan$status <- "reviewed"
  plan$reviewed_by <- c(NA, "JD")
  plan$verb[[2L]] <- NA
  plan$notes[[2L]] <- "value labels, not a derivation"
  path <- tempfile(fileext = ".csv")
  suppressMessages(write_dv_plan(plan, path))

  messages <- messages_from(read_dv_plan(path))

  expect_true(any(grepl("1 row is marked reviewed with no", messages)))
  expect_true(any(grepl("1 row marked reviewed has notes saying it is not a derivation", messages)))
})
