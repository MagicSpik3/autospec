plan_row <- function(dv, verb = NA_character_, args = NA_character_,
                     condition = NA_character_, status = "reviewed",
                     inputs = NA_character_, label = NA_character_,
                     instructions = "spec text") {
  data.frame(
    dv = dv, level = "person", verb = verb, inputs = inputs, args = args,
    condition = condition, status = status, reviewed_by = "JD",
    reviewed_on = NA_character_, notes = NA_character_,
    file_name = "Test_DV_Spec_R9.xlsx", sheet_name = "Sheet1", excel_row = 2L,
    label = label, instructions = instructions, stringsAsFactors = FALSE
  )
}


write_quietly <- function(plan, ...) {
  suite_dir <- file.path(tempfile(), "dv_suite")
  suppressMessages(write_dv_suite(plan, suite_dir, ...))
  suite_dir
}


test_that("a signed-off row is written as a call to its verb", {
  suite_dir <- write_quietly(
    plan_row("b", "dv_copy", args = "source_col = \"a\"", label = "Copy of a")
  )

  lines <- readLines(file.path(suite_dir, "test.R"))

  expect_true("steps$b <- dv_step(" %in% lines)
  expect_true("  label = \"Copy of a\"," %in% lines)
  expect_true("  inputs = \"a\"," %in% lines)
  expect_true("      source_col = \"a\"," %in% lines)
  expect_true("      new_col = \"b\"," %in% lines)
  expect_true("      sentinels = settings$sentinels" %in% lines)
  expect_silent(parse(file.path(suite_dir, "test.R")))
})


test_that("the written suite reads back and builds the DVs with labels", {
  plan <- rbind(
    plan_row("total", "dv_row_total", args = "value_cols = c(\"copy_a\", \"b\")", label = "Total"),
    plan_row("copy_a", "dv_copy", args = "source_col = \"a\"", label = "Copy of a"),
    plan_row("flag", "dv_flag_if", condition = "CommiR9 %in% c(4, 5)")
  )

  suite_dir <- write_quietly(plan, data_names = c("a", "b", "commir9", "hhserial"))
  df <- data.frame(hhserial = c(1, 2), a = c(1, 2), b = c(10, 20), commir9 = c(4, 1))

  result <- suppressMessages(run_dv_suite(df, suite_dir))

  expect_identical(result$total, c(11, 22), ignore_attr = TRUE)
  expect_identical(result$flag, c(1, 0), ignore_attr = TRUE)
  expect_identical(attr(result$total, "label"), "Total")
  expect_true(all(attr(result, "dv_suite_report")$outcome == "built"))
})


test_that("a numbered range DV is written as one step per index", {
  suite_dir <- write_quietly(plan_row("C(1-2)", "dv_copy", args = "source_col = \"S(1-2)\""),
                             data_names = c("s1", "s2"))

  suite <- suppressMessages(read_dv_suite(suite_dir))
  result <- suppressMessages(run_dv_suite(data.frame(s1 = 1, s2 = 2), suite))

  expect_identical(names(suite$steps), c("c1", "c2"))
  expect_identical(result$c2, 2)
})


test_that("a DV with two different numbered ranges is written as one step per combination", {
  suite_dir <- write_quietly(
    plan_row("M(1-2)_interest(1-3)", "dv_copy", args = "source_col = \"S(1-2)_rate(1-3)\""),
    data_names = paste0("s", rep(1:2, each = 3), "_rate", 1:3)
  )

  suite <- suppressMessages(read_dv_suite(suite_dir))
  result <- suppressMessages(run_dv_suite(data.frame(s2_rate3 = 23, s1_rate2 = 12), suite, dvs = c("m2_interest3", "m1_interest2")))

  expect_identical(names(suite$steps), paste0("m", rep(1:2, each = 3), "_interest", 1:3))
  expect_identical(result$m2_interest3, 23, ignore_attr = TRUE)
  expect_identical(result$m1_interest2, 12, ignore_attr = TRUE)
})


test_that("settings are referenced from settings.R, not written per DV", {
  suite_dir <- write_quietly(plan_row("t", "dv_sum_to_household", args = "value_col = \"v\""))

  lines <- readLines(file.path(suite_dir, "test.R"))

  expect_true("      by = settings$by," %in% lines)
  difference_lines <- readLines(file.path(
    write_quietly(plan_row("d", "dv_difference", args = "minuend_col = \"a\", subtrahend_col = \"b\"")), "test.R"
  ))
  expect_true("      sentinels = settings$sentinels_in_calculations" %in% difference_lines)
  expect_true(file.exists(file.path(suite_dir, "settings.R")))
})


test_that("rows not signed off and rows with no verb are written as stubs", {
  plan <- rbind(
    plan_row("drafted", "dv_copy", args = "source_col = \"a\"", status = "auto"),
    plan_row("by_hand", status = "hand_written", inputs = "a; b",
             instructions = "IF a = 1 THEN by_hand = 1\nOTHERWISE by_hand = 2")
  )

  suite_dir <- write_quietly(plan)
  lines <- readLines(file.path(suite_dir, "test.R"))
  suite <- suppressMessages(read_dv_suite(suite_dir))

  expect_null(suite$steps$drafted$derive)
  expect_null(suite$steps$by_hand$derive)
  expect_identical(suite$steps$by_hand$inputs, c("a", "b"))
  expect_true(any(grepl("#   OTHERWISE by_hand = 2", lines, fixed = TRUE)))
  expect_true(any(grepl("#     source_col = \"a\",", lines, fixed = TRUE)))
  expect_true(file.exists(file.path(suite_dir, "tests", "test-test.R")))
  expect_silent(parse(file.path(suite_dir, "tests", "test-test.R")))
})


test_that("reviewed_only = FALSE writes drafted calls as working code", {
  suite_dir <- write_quietly(
    plan_row("drafted", "dv_copy", args = "source_col = \"a\"", status = "auto"),
    reviewed_only = FALSE
  )

  suite <- suppressMessages(read_dv_suite(suite_dir))

  expect_true(is.function(suite$steps$drafted$derive))
})


test_that("rows marked not_a_derivation are left out but listed", {
  plan <- rbind(
    plan_row("b", "dv_copy", args = "source_col = \"a\""),
    plan_row("elsewhere", status = "not_a_derivation", instructions = "Derived in Income DV Spec")
  )

  suite_dir <- write_quietly(plan)
  lines <- readLines(file.path(suite_dir, "test.R"))

  expect_false(any(grepl("steps$elsewhere", lines, fixed = TRUE)))
  expect_true(any(grepl("#   elsewhere: Derived in Income DV Spec", lines, fixed = TRUE)))
})


test_that("existing files are not overwritten", {
  suite_dir <- write_quietly(plan_row("b", "dv_copy", args = "source_col = \"a\""))
  topic_file <- file.path(suite_dir, "test.R")
  writeLines("# edited by hand", topic_file)

  suppressMessages(write_dv_suite(plan_row("b", "dv_copy", args = "source_col = \"a\""), suite_dir))

  expect_identical(readLines(topic_file), "# edited by hand")
})


test_that("a DV signed off twice in different ways is refused", {
  plan <- rbind(
    plan_row("b", "dv_copy", args = "source_col = \"a\""),
    plan_row("B", "dv_copy", args = "source_col = \"z\"")
  )

  expect_error(write_quietly(plan), "more than once in the plan, in different ways")
})


test_that("a DV signed off twice the same way is written once", {
  plan <- rbind(
    plan_row("b", "dv_copy", args = "source_col = \"a\""),
    plan_row("b", "dv_copy", args = "source_col = \"a\"")
  )
  plan$sheet_name <- c("Sheet1", "Sheet2")

  suite <- suppressMessages(read_dv_suite(write_quietly(plan)))

  expect_identical(names(suite$steps), "b")
})


test_that("duplicate rows are left out and listed at the top of the file", {
  plan <- rbind(
    plan_row("b", "dv_copy", args = "source_col = \"a\""),
    plan_row("b", "dv_copy", args = "source_col = \"z\"", status = "duplicate")
  )
  plan$excel_row <- c(2L, 9L)

  suite_dir <- write_quietly(plan)
  lines <- readLines(file.path(suite_dir, "test.R"))
  suite <- suppressMessages(read_dv_suite(suite_dir))

  expect_identical(suite$steps$b$inputs, "a")
  expect_true("#   b (Sheet1 row 9)" %in% lines)
})


test_that("args naming a setting leave the step unwritten", {
  suite_dir <- write_quietly(plan_row("t", "dv_sum_to_household", args = "value_col = \"v\", by = \"hh\""))
  suite <- suppressMessages(read_dv_suite(suite_dir))

  expect_null(suite$steps$t$derive)
})


test_that("topics are named after the spec file", {
  expect_identical(
    suite_topic(c("specs/Financial_Wealth_DV_Spec_R9.xlsx", "Pension_Wealth_DV_Spec_R9_reformat.xlsx")),
    c("financial_wealth", "pension_wealth")
  )
})


test_that("conditions are rewritten to read columns from df", {
  code <- columns_to_df(str2lang("commir9 %in% c(4, 5) & !is.na(x)"), c("CommiR9", "x"))

  expect_identical(deparse(code), "df$CommiR9 %in% c(4, 5) & !is.na(df$x)")
  expect_identical(code_column_names(code), c("CommiR9", "x"))
})


test_that("plan args may hold only values", {
  expect_identical(parse_plan_args("value_cols = c(\"a\", \"b\"), otherwise = NA"),
                   list(value_cols = c("a", "b"), otherwise = NA))
  expect_error(parse_plan_args("source_col = a"), "looks like a variable")
  expect_error(parse_plan_args("source_col = system(\"x\")"), "only values")
})


test_that("names follow the data's case, whatever case the spec uses", {
  plan <- plan_row("DVCopyR9", "dv_copy", args = "source_col = \"FCISAvR9_i\"")
  written_lines <- function(...) readLines(file.path(write_quietly(plan, ...), "test.R"))

  lower_lines <- written_lines(data_names = c("fcisavr9_i", "hhserial"))
  upper_lines <- written_lines(data_names = c("FCISAVR9_I", "HHSERIAL"))
  mixed_lines <- written_lines(data_names = c("fcIsaVr9_i", "HHSerial"))

  expect_true("steps$dvcopyr9 <- dv_step(" %in% lower_lines)
  expect_true("      source_col = \"fcisavr9_i\"," %in% lower_lines)
  expect_true("steps$DVCOPYR9 <- dv_step(" %in% upper_lines)
  expect_true("      source_col = \"FCISAVR9_I\"," %in% upper_lines)
  expect_true("steps$DVCopyR9 <- dv_step(" %in% mixed_lines)
  expect_true("      source_col = \"fcIsaVr9_i\"," %in% mixed_lines)
})


test_that("a DV already in the data keeps the data's spelling, and name_case overrides the style", {
  plan <- rbind(
    plan_row("DVOldR9", "dv_copy", args = "source_col = \"a\""),
    plan_row("DVNewR9", "dv_copy", args = "source_col = \"a\"")
  )

  lines <- readLines(file.path(
    write_quietly(plan, data_names = c("a", "DVOLDR9"), name_case = "lower"), "test.R"
  ))

  expect_true("steps$DVOLDR9 <- dv_step(" %in% lines)
  expect_true("steps$dvnewr9 <- dv_step(" %in% lines)
})


test_that("the household identifier in settings.R is spelt as the data spells it", {
  suite_dir <- write_quietly(plan_row("b", "dv_copy", args = "source_col = \"a\""),
                             data_names = c("A", "HHSerial"))

  expect_true("  by = \"HHSerial\"," %in% readLines(file.path(suite_dir, "settings.R")))
})


test_that("one DV spelt two ways in the spec is written one way", {
  plan <- rbind(
    plan_row("DVBldValR9", "dv_copy", args = "source_col = \"a\""),
    plan_row("total", "dv_copy", args = "source_col = \"DVblDValR9\"")
  )

  suite <- suppressMessages(read_dv_suite(write_quietly(plan, data_names = c("a", "Mixed"))))

  expect_identical(suite$steps$total$inputs, "DVBldValR9")
})


test_that("data with names differing only in case is refused", {
  expect_error(
    write_quietly(plan_row("b", "dv_copy", args = "source_col = \"a\""), data_names = c("a", "A")),
    "differ only in case"
  )
})



test_that("a DV already in the data with nothing written is pointed out", {
  plan <- rbind(
    plan_row("copy_a", "dv_copy", args = "source_col = \"a\""),
    plan_row("old_dv", status = "hand_written", inputs = "a"),
    plan_row("new_dv", status = "hand_written", inputs = "a")
  )
  suite_dir <- file.path(tempfile(), "dv_suite")

  messages <- character()
  withCallingHandlers(
    write_dv_suite(plan, suite_dir, data_names = c("a", "OLD_DV", "copy_a")),
    message = function(condition) {
      messages <<- c(messages, conditionMessage(condition))
      invokeRestart("muffleMessage")
    }
  )

  lines <- readLines(file.path(suite_dir, "test.R"))
  old_step <- which(lines == "steps$OLD_DV <- dv_step(")
  new_step <- which(lines == "steps$new_dv <- dv_step(")

  expect_true(any(grepl("^# Already in the data", lines[seq_len(old_step)])))
  expect_false(any(grepl("^# Already in the data", lines[(old_step + 1L):new_step])))
  expect_true(any(grepl("2 of 3 DVs are already in the data", messages)))
  expect_true(any(grepl("1 of them has nothing written yet", messages) & grepl("OLD_DV", messages) & grepl("builds it", messages)))
})


test_that("settings.R takes the household identifier from the data", {
  plan <- plan_row("t", "dv_sum_to_household", args = "value_col = \"v\"")

  with_round <- write_quietly(plan, data_names = c("v", "HHSerialR9"))
  expect_true("  by = \"HHSerialR9\"," %in% readLines(file.path(with_round, "settings.R")))

  without <- write_quietly(plan, data_names = c("v", "household"))
  expect_true("  by = \"hhserial\"," %in% readLines(file.path(without, "settings.R")))
})


test_that("only the chosen topics are written, and an unknown topic is refused", {
  plan <- rbind(
    plan_row("b", "dv_copy", args = "source_col = \"a\""),
    plan_row("d", "dv_copy", args = "source_col = \"a\"")
  )
  plan$file_name <- c("Property_Wealth_DV_Spec_R9.xlsx", "Income_DV_Spec_R9.xlsx")

  suite_dir <- write_quietly(plan, topics = "income")

  expect_identical(sort(list.files(suite_dir, pattern = "[.]R$")), c("income.R", "settings.R"))
  expect_error(write_quietly(plan, topics = "incme"), "Unknown topic")
})


test_that("a DV signed off twice in different ways stops, unless duplicates are not strict", {
  plan <- rbind(
    plan_row("b", "dv_copy", args = "source_col = \"a\""),
    plan_row("b", "dv_copy", args = "source_col = \"c\"")
  )
  plan$sheet_name <- c("S1", "S2")

  expect_error(write_quietly(plan), "signed off more than once")

  suite_dir <- write_quietly(plan, strict_duplicates = FALSE)
  expect_true("      source_col = \"a\"," %in% readLines(file.path(suite_dir, "test.R")))
})
