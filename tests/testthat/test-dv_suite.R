make_suite <- function(topic_lines, settings_lines = "settings <- list(by = \"hhserial\")") {
  suite_dir <- tempfile("dv_suite")
  dir.create(suite_dir)
  writeLines(settings_lines, file.path(suite_dir, "settings.R"))

  for (topic in names(topic_lines)) {
    writeLines(topic_lines[[topic]], file.path(suite_dir, paste0(topic, ".R")))
  }

  suite_dir
}


example_suite <- function() {
  make_suite(list(
    savings = c(
      "steps <- list()",
      "steps$total <- dv_step(label = 'Total', inputs = c('copy_a', 'b'),",
      "  derive = function(df) dv_row_total(df, c('copy_a', 'b'), 'total'))",
      "steps$copy_a <- dv_step(label = 'Copy of a', inputs = 'a',",
      "  derive = function(df) dv_copy(df, 'a', 'copy_a'))",
      "steps$todo <- dv_step(label = 'To do', inputs = 'a', derive = NULL)"
    ),
    debt = c(
      "steps <- list()",
      "steps$needs_x <- dv_step(label = NA, inputs = 'x',",
      "  derive = function(df) dv_copy(df, 'x', 'needs_x'))",
      "steps$broken <- dv_step(label = NA, inputs = 'a', derive = function(df) stop('boom'))",
      "steps$by_hand <- dv_step(label = 'By hand', inputs = 'a',",
      "  derive = function(df) { df$by_hand <- df$a * 2; df })"
    )
  ))
}


test_that("the suite reads every topic file into named steps", {
  suite <- suppressMessages(read_dv_suite(example_suite()))

  expect_s3_class(suite, "dv_suite")
  expect_setequal(names(suite$steps), c("total", "copy_a", "todo", "needs_x", "broken", "by_hand"))
  expect_identical(suite$steps$total$topic, "savings")
  expect_identical(suite$settings$by, "hhserial")
})


test_that("steps run in dependency order and each outcome is reported", {
  df <- data.frame(a = c(1, 2), b = c(10, 20))

  result <- suppressMessages(run_dv_suite(df, example_suite()))
  report <- attr(result, "dv_suite_report")

  expect_identical(result$total, c(11, 22), ignore_attr = TRUE)
  expect_identical(result$by_hand, c(2, 4), ignore_attr = TRUE)
  expect_identical(attr(result$copy_a, "label"), "Copy of a")
  expect_identical(report$outcome[report$dv == "todo"], "not written")
  expect_identical(report$outcome[report$dv == "needs_x"], "skipped")
  expect_identical(report$outcome[report$dv == "broken"], "failed")
  expect_match(report$detail[report$dv == "broken"], "boom")
})


test_that("the suite report carries the plan uid for each row", {
  plan <- data.frame(
    uid = c("dv_000001", "dv_000002"),
    dv = c("copy_a", "total"),
    level = c("person", "person"),
    verb = c("dv_copy", "dv_row_total"),
    inputs = c("a; b", "copy_a; b"),
    args = c("source_col = \"a\"", "value_cols = c(\"copy_a\", \"b\")"),
    condition = c(NA_character_, NA_character_),
    missing_code = c(NA_character_, NA_character_),
    status = c("reviewed", "reviewed"),
    reviewed_by = c("JD", "JD"),
    reviewed_on = c("2026-09-14", "2026-09-14"),
    notes = c(NA_character_, NA_character_),
    file_name = c("Example.xlsx", "Example.xlsx"),
    sheet_name = c("Sheet1", "Sheet1"),
    excel_row = c(10L, 11L),
    label = c("Copy of a", "Total"),
    instructions = c("copy_a = a", "total = copy_a + b"),
    stringsAsFactors = FALSE
  )

  suite <- suppressMessages(write_suite_folder(plan, tempfile("suite"), data_names = c("a", "b"), reviewed_only = TRUE, overwrite = TRUE))
  df <- data.frame(a = c(1, 2), b = c(10, 20))
  result <- suppressMessages(run_dv_suite(df, dirname(suite[[1L]]), dvs = c("copy_a", "total")))

  expect_identical(attr(result, "dv_suite_report")$uid, c("dv_000001", "dv_000002"))
})


test_that("stop_on_error stops at the first failing step", {
  df <- data.frame(a = 1, b = 1)

  expect_error(
    suppressMessages(run_dv_suite(df, example_suite(), stop_on_error = TRUE)),
    "Building broken failed"
  )
})


test_that("chosen DVs run with the steps that build their inputs", {
  df <- data.frame(a = 1, b = 10)

  result <- suppressMessages(run_dv_suite(df, example_suite(), dvs = "total"))

  expect_identical(attr(result, "dv_suite_report")$dv, c("copy_a", "total"))
})


test_that("inputs already in the data are not rebuilt", {
  df <- data.frame(a = 1, b = 10, copy_a = 5)

  result <- suppressMessages(run_dv_suite(df, example_suite(), dvs = "total"))

  expect_identical(attr(result, "dv_suite_report")$dv, "total")
  expect_identical(result$total, 15, ignore_attr = TRUE)
})


test_that("a chosen topic runs only its own steps", {
  df <- data.frame(a = 1)

  result <- suppressMessages(run_dv_suite(df, example_suite(), topics = "debt"))

  expect_setequal(attr(result, "dv_suite_report")$dv, c("needs_x", "broken", "by_hand"))
})


test_that("no topics chosen, as from an empty config, runs every step", {
  df <- data.frame(a = 1)

  result <- suppressMessages(run_dv_suite(df, example_suite(), topics = character()))

  expect_identical(nrow(attr(result, "dv_suite_report")), 6L)
})


test_that("unknown topics and DVs are refused with a suggestion", {
  suite <- suppressMessages(read_dv_suite(example_suite()))

  expect_error(suppressMessages(run_dv_suite(data.frame(a = 1), suite, topics = "pension")), "Unknown topic")
  expect_message(
    try(run_dv_suite(data.frame(a = 1), suite, dvs = "totl"), silent = TRUE),
    "did you mean total"
  )
})


test_that("a data.table goes in and comes out as a data.table", {
  df <- data.table::data.table(a = 1, b = 2)

  result <- suppressMessages(run_dv_suite(df, example_suite(), dvs = "total"))

  expect_true(data.table::is.data.table(result))
  expect_identical(result$total, 3, ignore_attr = TRUE)
})


test_that("a circular dependency is refused", {
  suite_dir <- make_suite(list(loop = c(
    "steps <- list()",
    "steps$p <- dv_step(label = NA, inputs = 'q', derive = function(df) dv_copy(df, 'q', 'p'))",
    "steps$q <- dv_step(label = NA, inputs = 'p', derive = function(df) dv_copy(df, 'p', 'q'))"
  )))

  expect_error(suppressMessages(run_dv_suite(data.frame(z = 1), suite_dir)), "circular")
})


test_that("a DV built in two topic files is refused", {
  step <- "steps$same <- dv_step(label = NA, inputs = 'a', derive = NULL)"
  suite_dir <- make_suite(list(one = c("steps <- list()", step), two = c("steps <- list()", step)))

  expect_error(suppressMessages(read_dv_suite(suite_dir)), "more than one step")
})


test_that("a malformed step names its file", {
  suite_dir <- make_suite(list(bad = c(
    "steps <- list()",
    "steps$x <- dv_step(label = NA, inputs = 1, derive = NULL)"
  )))

  expect_error(suppressMessages(read_dv_suite(suite_dir)), "steps\\$x in bad.R")
})


test_that("run_dv_step builds one DV and stops on problems", {
  suite <- suppressMessages(read_dv_suite(example_suite()))

  result <- suppressMessages(run_dv_step(data.frame(a = 3), suite, "copy_a"))

  expect_identical(result$copy_a, 3, ignore_attr = TRUE)
  expect_error(suppressMessages(run_dv_step(data.frame(a = 3), suite, "todo")), "not written yet")
  expect_error(suppressMessages(run_dv_step(data.frame(z = 3), suite, "copy_a")), "missing inputs")
})


test_that("labels are attached to data frames and data.tables", {
  df <- set_dv_label(data.frame(x = 1), "x", "An example")
  dt <- set_dv_label(data.table::data.table(x = 1), "x", "An example")

  expect_identical(attr(df$x, "label"), "An example")
  expect_identical(attr(dt$x, "label"), "An example")
  expect_null(attr(set_dv_label(data.frame(x = 1), "x", NA)$x, "label"))
})


test_that("labelling a copy does not label the column it was copied from", {
  df <- suppressMessages(dv_copy(data.frame(a = 1), "a", "b"))

  df <- set_dv_label(df, "b", "Copy")

  expect_null(attr(df$a, "label"))
})


test_that("dv_step keeps what it is given", {
  step <- dv_step(label = "L", inputs = "a", derive = NULL)

  expect_s3_class(step, "dv_step")
  expect_identical(step$inputs, "a")
})


test_that("DVs can be asked for in any case", {
  df <- data.frame(a = 1, b = 10)
  suite <- suppressMessages(read_dv_suite(example_suite()))

  result <- suppressMessages(run_dv_suite(df, suite, dvs = "TOTAL"))

  expect_identical(attr(result, "dv_suite_report")$dv, c("copy_a", "total"))
  expect_identical(suppressMessages(run_dv_step(df, suite, "Copy_A"))$copy_a, 1, ignore_attr = TRUE)
})


test_that("data spelling variables in another case runs, and keeps its own spelling", {
  df <- data.frame(A = 1, B = 10, TOTAL = 0)

  result <- suppressMessages(run_dv_suite(df, example_suite(), dvs = "total"))

  expect_identical(names(result), c("A", "B", "TOTAL", "copy_a"))
  expect_identical(result$TOTAL, 11, ignore_attr = TRUE)
  expect_identical(attr(result$TOTAL, "label"), "Total")
  expect_message(run_dv_suite(df, example_suite(), dvs = "total"), "matched ignoring case")

  one_step <- suppressMessages(run_dv_step(data.frame(A = 1), example_suite(), "copy_a"))
  expect_identical(names(one_step), c("A", "copy_a"))
})


test_that("a household identifier in another case from the data runs", {
  suite_dir <- make_suite(
    list(topic = c("steps <- list()",
                   "steps$n <- dv_step(label = NA, inputs = 'a', derive = function(df) dv_sum_to_household(df, 'a', 'n', by = settings$by))")),
    settings_lines = "settings <- list(by = \"HHSerial\")"
  )

  result <- suppressMessages(run_dv_suite(data.frame(a = c(1, 2), hhserial = c(1, 1)), suite_dir))

  expect_identical(result$n, c(3, 3), ignore_attr = TRUE)
  expect_identical(names(result), c("a", "hhserial", "n"))
})


test_that("a suite spelling one variable two ways is refused", {
  suite_dir <- make_suite(list(topic = c(
    "steps <- list()",
    "steps$b <- dv_step(label = NA, inputs = 'a', derive = function(df) { df$b <- df$a; df })",
    "steps$c <- dv_step(label = NA, inputs = 'A', derive = function(df) { df$c <- df$A; df })"
  )))

  expect_error(suppressMessages(run_dv_suite(data.frame(a = 1), suite_dir)), "more than one way")
})


test_that("data with columns differing only in case is refused", {
  expect_error(
    suppressMessages(run_dv_suite(data.frame(a = 1, A = 2, b = 3, check.names = FALSE), example_suite())),
    "differ only in case"
  )
})


test_that("a step creating a column in a different case from the data or its name fails", {
  suite_dir <- make_suite(list(topic = c(
    "steps <- list()",
    "steps$b <- dv_step(label = NA, inputs = 'a', derive = function(df) { df$B <- df$a; df })",
    "steps$c <- dv_step(label = NA, inputs = 'a', derive = function(df) { df$c <- 1; df$A <- 2; df })"
  )))

  result <- suppressMessages(run_dv_suite(data.frame(a = 1), suite_dir))
  report <- attr(result, "dv_suite_report")

  expect_match(report$detail[report$dv == "b"], "it created B")
  expect_match(report$detail[report$dv == "c"], "created A but the data already has a")
})


test_that("two steps building one variable in different cases are refused", {
  suite_dir <- make_suite(list(
    one = c("steps <- list()", "steps$dvager9 <- dv_step(label = NA, inputs = 'a', derive = NULL)"),
    two = c("steps <- list()", "steps$DVAgeR9 <- dv_step(label = NA, inputs = 'a', derive = NULL)")
  ))

  expect_error(suppressMessages(read_dv_suite(suite_dir)), "more than one step")
})


test_that("a DV already in the data is reported before it is replaced", {
  df <- data.frame(a = 1, b = 10, total = 99)

  expect_message(run_dv_suite(df, example_suite(), dvs = "total"), "already exist")
})



test_that("a step not written yet leaves a DV already in the data as it is", {
  df <- data.frame(a = c(1, 2), b = c(10, 20), todo = c(5, 6))

  result <- suppressMessages(run_dv_suite(df, example_suite(), topics = "savings"))
  report <- attr(result, "dv_suite_report")

  expect_identical(result$todo, c(5, 6))
  expect_identical(report$outcome[report$dv == "todo"], "not written")
  expect_match(report$detail[report$dv == "todo"], "already in the data")
})
