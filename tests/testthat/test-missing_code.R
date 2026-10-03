missing_code_suite <- function(missing_code_text = ", missing_code = -9") {
  make_suite_dir <- tempfile("dv_suite")
  dir.create(make_suite_dir)
  writeLines("settings <- list(by = \"hhserial\", sentinels = \"zero\", na_as_zero = FALSE)",
             file.path(make_suite_dir, "settings.R"))
  writeLines(c(
    "steps <- list()",
    paste0("steps$total <- dv_step(label = 'Total', inputs = c('a', 'b'),",
           "  derive = function(df) dv_row_total(df, value_cols = c('a', 'b'), new_col = 'total',",
           "    na_as_zero = settings$na_as_zero, sentinels = settings$sentinels)", missing_code_text, ")"),
    paste0("steps$by_hand <- dv_step(label = 'By hand', inputs = 'a',",
           "  derive = function(df) { df$by_hand <- df$a * 2; df }", missing_code_text, ")")
  ), file.path(make_suite_dir, "money.R"))
  make_suite_dir
}

missing_code_data <- function() {
  data.frame(hhserial = 1:5, a = c(1, -9, -8, NA, 4), b = c(10, 20, 30, 40, -9))
}


test_that("-8 and -9 inputs count as missing, and missing in the DV is written as -9", {
  result <- suppressMessages(run_dv_suite(missing_code_data(), missing_code_suite()))

  expect_identical(result$total, c(11, -9, -9, -9, -9), ignore_attr = TRUE)
  expect_identical(result$by_hand, c(2, -9, -9, -9, 8), ignore_attr = TRUE)
})


test_that("the inputs are put back as they were", {
  result <- suppressMessages(run_dv_suite(missing_code_data(), missing_code_suite()))

  expect_identical(result$a, missing_code_data()$a)
  expect_identical(result$b, missing_code_data()$b)
})


test_that("without missing_code, the step's own sentinels setting applies", {
  result <- suppressMessages(run_dv_suite(missing_code_data(), missing_code_suite("")))

  expect_identical(result$total[1:3], c(11, 20, 30), ignore_attr = TRUE)
  expect_identical(result$by_hand[2], -18, ignore_attr = TRUE)
})


test_that("run_dv_step applies missing_code too, and the report counts the coded values", {
  suite <- suppressMessages(read_dv_suite(missing_code_suite()))

  expect_identical(run_dv_step(missing_code_data(), suite, "total")$total, c(11, -9, -9, -9, -9), ignore_attr = TRUE)

  result <- suppressMessages(run_dv_suite(missing_code_data(), suite))
  report <- attr(result, "dv_suite_report")
  expect_match(report$detail[report$dv == "total"], "4 missing written as -9")
})


test_that("missing_code must be one number", {
  suite_dir <- missing_code_suite(", missing_code = 'minus nine'")

  expect_error(suppressMessages(read_dv_suite(suite_dir)), "missing_code must be one number")
})


test_that("a plan row's missing_code is written into its step", {
  plan <- fake_plan_row("copy_a", "dv_copy", args = "source_col = \"a\"")
  plan$missing_code <- "-9"
  suite_dir <- file.path(tempfile(), "dv_suite")

  suppressMessages(write_dv_suite(plan, suite_dir, data_names = "a"))

  lines <- readLines(file.path(suite_dir, "test.R"))
  expect_true(any(grepl("^  missing_code = -9 # ", lines)))
  expect_true("  }," %in% lines)
  suite <- suppressMessages(read_dv_suite(suite_dir))
  expect_identical(suite$steps$copy_a$missing_code, -9)
})


test_that("a stub keeps its missing_code for when it is written", {
  plan <- fake_plan_row("copy_a", "dv_copy", args = "source_col = \"a\"", status = "auto", reviewed_by = NA)
  plan$missing_code <- "-9"
  suite_dir <- file.path(tempfile(), "dv_suite")

  suppressMessages(write_dv_suite(plan, suite_dir, data_names = "a"))

  expect_silent(parse(file.path(suite_dir, "test.R")))
  expect_true(any(grepl("^  derive = NULL, # replace", readLines(file.path(suite_dir, "test.R")))))
})


test_that("a missing_code that is not a number stops the write", {
  plan <- fake_plan_row("copy_a", "dv_copy", args = "source_col = \"a\"")
  plan$missing_code <- "minus nine"

  expect_error(suppressMessages(write_dv_suite(plan, file.path(tempfile(), "s"), data_names = "a")), "not a number")
})


test_that("the plan has a missing_code column, kept when the plan is rebuilt", {
  catalogue <- data.table::data.table(
    file_name = "spec.xlsx", sheet_name = "Sheet1", level = "person", excel_row = 2L,
    variable = "DVa", label = NA_character_, instructions = "DVa = x"
  )
  previous <- suppressMessages(build_dv_plan(catalogue))
  expect_true("missing_code" %in% names(previous))
  previous$missing_code <- "-9"

  catalogue$instructions <- "DVa = y"
  rebuilt <- suppressMessages(build_dv_plan(catalogue, previous_plan = previous))

  expect_identical(rebuilt$missing_code, "-9")
})


test_that("a plan written before missing_code existed still reads", {
  plan <- fake_plan_row("copy_a", "dv_copy", args = "source_col = \"a\"")
  path <- tempfile(fileext = ".csv")
  utils::write.csv(plan, path, row.names = FALSE, na = "")

  read_back <- suppressMessages(read_dv_plan(path))

  expect_true(is.na(read_back$missing_code))
})


test_that("sync points out written steps whose missing_code differs from the plan", {
  plan <- fake_plan_row("copy_a", "dv_copy", args = "source_col = \"a\"")
  suite_dir <- file.path(tempfile(), "dv_suite")
  suppressMessages(write_dv_suite(plan, suite_dir, data_names = "a"))
  plan$missing_code <- "-9"

  messages <- messages_from(sync_dv_suite(plan, suite_dir, data_names = "a"))

  expect_true(any(grepl("different missing_code from the plan", messages) & grepl("copy_a", messages)))
  expect_identical(suite_step_index(suite_dir)$missing_code, NA_real_)
})


test_that("the plain R export gives the same values as the suite", {
  suite <- suppressMessages(read_dv_suite(missing_code_suite()))
  expected <- suppressMessages(run_dv_suite(missing_code_data(), suite))

  for (dv in c("total", "by_hand")) {
    code <- plain_step_code(suite$steps[[dv]], suite$settings)
    script_env <- new.env()
    script_env$df <- missing_code_data()
    eval(parse(text = code$lines), envir = script_env)

    expect_identical(as.numeric(script_env$df[[dv]]), as.numeric(expected[[dv]]))
    expect_identical(script_env$df$a, missing_code_data()$a)
  }
})


test_that("the SPSS sets -8/-9 inputs to missing, puts them back and codes missing in the DV", {
  step <- list(dv = "copy_a", label = NA, inputs = "a", missing_code = -9,
               derive = function(df) dv_copy(df, source_col = "a", new_col = "copy_a"))

  lines <- spss_step_code(step)

  expect_identical(lines[2:3], c("COMPUTE tmp_given1 = a.", "RECODE a (-8, -9 = SYSMIS)."))
  expect_true("COMPUTE a = tmp_given1." %in% lines)
  expect_true("RECODE copy_a (SYSMIS = -9)." %in% lines)
  expect_identical(utils::tail(lines, 1), "DELETE VARIABLES tmp_given1.")
})
