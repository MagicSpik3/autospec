issue_catalogue <- function(variable, instructions, sheet_name = "Sheet1") {
  data.table::data.table(
    file_name = "spec.xlsx",
    sheet_name = sheet_name,
    level = "person",
    excel_row = seq_along(variable) + 1L,
    variable = variable,
    label = NA_character_,
    instructions = instructions
  )
}


draft <- function(variable, instructions, data_names = NULL, sheet_name = "Sheet1") {
  suppressMessages(build_dv_plan(issue_catalogue(variable, instructions, sheet_name), data_names = data_names))
}


# Classifying messages

test_that("each spec defect message gets its code, and review prompts get none", {
  messages <- c(
    "no else branch: drafted as 0 where the condition is false, as the published DVs are (decision D5)",
    "spec gives no value for a negative denominator",
    "no derivation text",
    "no variable name against this derivation",
    "value with no variable name inside a branch",
    "unknown variable var_b1, did you mean var_c1?",
    "also defined on S2 row 3 with different text: sign off the right one and set the others' status to duplicate (decision D8)",
    "could not parse: expected ')' at 'THEN'",
    "dash character used as minus",
    "IF condition wrapped in brackets",
    "OTHERWISE written in brackets",
    "unclosed bracket before IF removed",
    "unmatched closing bracket removed",
    "numbered range written in square brackets",
    "confirm the drafted condition",
    "no verb matches this shape",
    "spec floors every negative at zero (decision D1)"
  )

  expect_identical(spec_issue_codes(messages), c(
    "no_else", "missing_value", "no_derivation", "no_dv_name", "branch_without_name",
    "unknown_variable", "duplicate_dv", "unparsed",
    "typo", "typo", "typo", "typo", "typo", "typo",
    NA, NA, NA
  ))
})


# Missing ELSE, found from the parsed spec whatever verb matches

test_that("an IF with no ELSE is missing its ELSE", {
  expect_true(missing_else("IF a > 0 THEN x = b"))
  expect_true(missing_else("IF period = 1 THEN x = a * 52\nIF period = 2 THEN x = a * 26"))
})


test_that("an IF with an ELSE, or after the DV is already set, is not missing one", {
  expect_false(missing_else("IF a > 0 THEN x = b ELSE x = 0"))
  expect_false(missing_else("x = a; IF x < 0 THEN x = 0"))
  expect_false(missing_else("x = a - b"))
})


test_that("text that cannot be parsed is not reported as missing an ELSE", {
  expect_false(missing_else("IF (a > 0 THEN"))
  expect_false(missing_else(NA_character_))
})


# The plan's spec_issues column

test_that("a plan has a spec_issues column after notes", {
  plan <- draft("x", "x = a + b", data_names = c("a", "b"))

  expect_true("spec_issues" %in% names(plan))
  expect_identical(match("spec_issues", names(plan)), match("notes", names(plan)) + 1L)
  expect_true(is.na(plan$spec_issues))
})


test_that("a missing ELSE is a spec issue, drafted as 0 as before, and leaves notes", {
  plan <- draft("x", "IF a > 0 THEN x = b")

  expect_identical(plan$verb, "dv_keep_if")
  expect_match(plan$args, "otherwise = 0", fixed = TRUE)
  expect_match(plan$spec_issues, "[no_else]", fixed = TRUE)
  expect_false(grepl("no else branch", plan$notes, ignore.case = TRUE))
})


test_that("a missing ELSE is a spec issue on a row no verb fits, too", {
  plan <- draft("var_c1", "IF var_a > 0 AND var_b1 > 0 AND var_d1 > 0 THEN var_c1 = var_b1 - YearR9")

  expect_identical(plan$status, "hand_written")
  expect_match(plan$spec_issues, "[no_else]", fixed = TRUE)
  expect_identical(lengths(regmatches(plan$spec_issues, gregexpr("[no_else]", plan$spec_issues, fixed = TRUE))), 1L)
})


test_that("an unknown variable and a repaired typo are spec issues, moved out of notes", {
  plan <- draft("x", "x = a – zz", data_names = "a")

  expect_match(plan$spec_issues, "[unknown_variable] unknown variable zz", fixed = TRUE)
  expect_match(plan$spec_issues, "[typo] dash character used as minus", fixed = TRUE)
  expect_false(grepl("unknown variable|dash character", plan$notes))
})


test_that("a DV defined twice with different text is a spec issue on both rows", {
  plan <- draft(c("x", "x"), c("x = a", "x = b"), data_names = c("a", "b"), sheet_name = c("S1", "S2"))

  expect_identical(grepl("[duplicate_dv]", plan$spec_issues, fixed = TRUE), c(TRUE, TRUE))
})


test_that("review prompts stay in notes and are not spec issues", {
  plan <- draft("x", "IF category = 2 THEN x = amount ELSE x = 0", data_names = c("category", "amount"))

  expect_match(plan$notes, "confirm the drafted condition")
  expect_true(is.na(plan$spec_issues))
})


test_that("a row with a spec issue is never auto", {
  plan <- draft("x", "x = zz", data_names = "a")

  expect_identical(plan$verb, "dv_copy")
  expect_identical(plan$status, "needs_review")
})


test_that("a clean row has no spec issues and stays auto", {
  plan <- draft("x", "x = a + b", data_names = c("a", "b"))

  expect_true(is.na(plan$spec_issues))
  expect_identical(plan$status, "auto")
})


test_that("a plan written before spec_issues existed still reads", {
  path <- tempfile(fileext = ".csv")
  suppressMessages(write_dv_plan(fake_plan_row("a"), path))

  expect_no_error(suppressMessages(read_dv_plan(path)))
})


# The list for the spec owner

test_that("the spec issue table has one row per issue, with the code and the spec text", {
  plan <- draft(c("x", "y"), c("IF a > 0 THEN x = b", "y = a – zz"), data_names = c("a", "b"))

  table <- spec_issue_table(plan)

  expect_setequal(table$code, c("no_else", "unknown_variable", "typo"))
  expect_identical(table$dv[table$code == "no_else"], "x")
  expect_identical(table$instructions[table$code == "no_else"], "IF a > 0 THEN x = b")
  expect_true(all(c("uid", "file_name", "sheet_name", "excel_row", "issue") %in% names(table)))
})


test_that("a plan with no spec issues gives an empty table", {
  expect_identical(nrow(spec_issue_table(draft("x", "x = a + b", data_names = c("a", "b")))), 0L)
  expect_identical(nrow(spec_issue_table(fake_plan_row("a"))), 0L)
})


test_that("without the data's names, an input that is not a DV is not a spec issue, but a near miss is", {
  plan <- draft(c("DVa", "DVb"), c("DVa = raw_input", "DVb = DVaa"))

  expect_true(is.na(plan$spec_issues[[1L]]))
  expect_match(plan$notes[[1L]], "unknown variable raw_input")
  expect_identical(plan$status[[1L]], "auto")
  expect_match(plan$spec_issues[[2L]], "[unknown_variable] unknown variable DVaa, did you mean DVa?", fixed = TRUE)
})
