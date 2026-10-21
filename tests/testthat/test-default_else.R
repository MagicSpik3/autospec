else_catalogue <- function(variable, instructions) {
  data.table::data.table(
    file_name = "spec.xlsx",
    sheet_name = "Sheet1",
    level = "person",
    excel_row = seq_along(variable) + 1L,
    variable = variable,
    label = NA_character_,
    instructions = instructions
  )
}


draft_with_else <- function(variable, instructions, default_else, data_names = c("a", "b", "c", "period", "amount")) {
  suppressMessages(build_dv_plan(else_catalogue(variable, instructions), data_names = data_names,
                                 default_else = default_else))
}


# The value and its checks

test_that("the default ELSE may be 0, -9 or NA, written as numbers or text", {
  expect_identical(parse_default_else(0L), 0)
  expect_identical(parse_default_else(-9), -9)
  expect_identical(parse_default_else("-9"), -9)
  expect_identical(parse_default_else("NA"), NA_real_)
  expect_identical(parse_default_else("na"), NA_real_)
  expect_identical(parse_default_else(NA), NA_real_)
})


test_that("any other default ELSE is refused, naming the setting", {
  expect_error(parse_default_else(5), "default_else")
  expect_error(parse_default_else("missing"), "default_else")
  expect_error(parse_default_else(c(0, -9)), "default_else")
  expect_error(parse_default_else(NULL), "default_else")
})


# Drafting with the default

test_that("match_verb keeps drafting 0 for a missing ELSE when no default is given", {
  match <- match_verb("IF a > 0 THEN x = b", "x")

  expect_identical(match$args$otherwise, 0)
})


test_that("match_verb drafts the given default for a missing ELSE, and says so", {
  kept <- match_verb("IF a > 0 THEN x = b", "x", default_else = -9)
  flag <- match_verb("IF a > 0 THEN x = 1", "x", default_else = NA_real_)

  expect_identical(kept$verb, "dv_keep_if")
  expect_identical(kept$args$otherwise, -9)
  expect_match(paste(kept$reasons, collapse = "; "), "drafted as -9", fixed = TRUE)
  expect_identical(flag$verb, "dv_flag_if")
  expect_identical(flag$args$otherwise, NA_real_)
})


test_that("an ELSE the spec gives is used, not the default", {
  match <- match_verb("IF a > 0 THEN x = b ELSE x = 0", "x", default_else = -9)

  expect_identical(match$args$otherwise, 0)
})


test_that("match_verb refuses a default ELSE that is not 0, -9 or NA", {
  expect_error(match_verb("IF a > 0 THEN x = b", "x", default_else = 5), "default_else")
})


# The plan records that the ELSE was implied

test_that("a plan has an implied_else column after spec_issues", {
  plan <- draft_with_else("x", "x = a + b", 0)

  expect_identical(match("implied_else", names(plan)), match("spec_issues", names(plan)) + 1L)
  expect_true(is.na(plan$implied_else))
})


test_that("a missing ELSE is drafted with the default and recorded as implied", {
  plan <- draft_with_else("x", "IF a > 0 THEN x = b", -9)

  expect_match(plan$args, "otherwise = -9", fixed = TRUE)
  expect_identical(plan$implied_else, "-9")
  expect_match(plan$spec_issues, "[no_else]", fixed = TRUE)
  expect_match(plan$spec_issues, "-9", fixed = TRUE)
})


test_that("an NA default is drafted and recorded as NA", {
  plan <- draft_with_else("x", "IF a > 0 THEN x = b", NA_real_)

  expect_match(plan$args, "otherwise = NA", fixed = TRUE)
  expect_identical(plan$implied_else, "NA")
})


test_that("a missing ELSE on a row no verb fits is recorded as implied, for whoever writes it", {
  plan <- draft_with_else(
    "var_c1", "IF var_a > 0 AND var_b1 > 0 AND var_d1 > 0 THEN var_c1 = var_b1 - YearR9", -9,
    data_names = c("var_a", "var_b1", "var_d1", "YearR9")
  )

  expect_identical(plan$status, "hand_written")
  expect_identical(plan$implied_else, "-9")
  expect_match(plan$spec_issues, "[no_else] no ELSE branch: -9", fixed = TRUE)
})


test_that("a row whose spec gives its ELSE has nothing implied", {
  plan <- draft_with_else("x", "IF a > 0 THEN x = b ELSE x = c", -9)

  expect_true(is.na(plan$implied_else))
  expect_true(is.na(plan$spec_issues))
})


test_that("a missing ELSE the drafted verb cannot fill is flagged but not recorded as implied", {
  plan <- draft_with_else(
    "x", "IF period = 1 THEN x = amount * 52\nIF period = 2 THEN x = amount * 26\nIF period = 3 THEN x = amount * 12", -9
  )

  expect_identical(plan$verb, "dv_annualise")
  expect_match(plan$spec_issues, "[no_else]", fixed = TRUE)
  expect_true(is.na(plan$implied_else))
})


test_that("a plan written before implied_else existed still reads", {
  path <- tempfile(fileext = ".csv")
  suppressMessages(write_dv_plan(fake_plan_row("a"), path))

  expect_no_error(suppressMessages(read_dv_plan(path)))
})


# The suite shows it to whoever writes the step

test_that("a step's comment shows its spec issues and the implied ELSE", {
  plan <- draft_with_else(
    "var_c1", "IF var_a > 0 AND var_b1 > 0 AND var_d1 > 0 THEN var_c1 = var_b1 - YearR9", -9,
    data_names = c("var_a", "var_b1", "var_d1", "YearR9")
  )
  plan$status <- "reviewed"
  suite_dir <- tempfile("dv_suite")

  suppressMessages(write_dv_suite(plan, suite_dir, data_names = c("var_a", "var_b1", "var_d1", "YearR9")))
  code <- unlist(lapply(list.files(suite_dir, pattern = "[.]R$", full.names = TRUE), readLines))

  expect_true(any(grepl("^# Spec issues: \\[no_else\\]", code)))
  expect_true(any(grepl("^# Implied ELSE: -9 where no branch applies \\(default_else in the config\\)", code)))
})
