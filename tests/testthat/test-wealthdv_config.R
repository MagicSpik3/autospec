make_project <- function(round = 10, spec_names = c("Income_DV_Spec_R10.xlsx", "Income_DV_Spec_R9.xlsx")) {
  project <- tempfile("project")
  dir.create(file.path(project, "specs"), recursive = TRUE)
  project <- normalizePath(project, winslash = "/")
  file.create(file.path(project, "specs", spec_names))

  config_path <- file.path(project, "wealthdv_config.yaml")
  suppressMessages(create_wealthdv_config(config_path, round = round))

  lines <- readLines(config_path)
  lines <- sub("^spec_folder: .*$", paste0("spec_folder: '", file.path(project, "specs"), "'"), lines)
  lines <- sub("^output_folder: .*$", paste0("output_folder: '", project, "/outputs/R{round}'"), lines)
  lines <- sub("^suite_folder: .*$", paste0("suite_folder: '", project, "/dv_suite/R{round}'"), lines)
  writeLines(lines, config_path)

  config_path
}


test_that("a new config fills the round in everywhere {round} appears", {
  config <- suppressMessages(read_wealthdv_config(make_project(round = 10)))

  expect_identical(config$round, 10L)
  expect_identical(config$spec_pattern, "_DV_Spec_R10.*[.]xlsx$")
  expect_true("R10_finalised_changelog" %in% config$exclude_sheets)
  expect_true(" Final accounting structure" %in% config$exclude_sheets)
  expect_match(config$suite_folder, "dv_suite/R10$")
})


test_that("only this round's spec workbooks are found", {
  config <- suppressMessages(read_wealthdv_config(make_project(round = 10)))

  expect_identical(basename(config$spec_files), "Income_DV_Spec_R10.xlsx")
})


test_that("Excel lock files are ignored and users are told to close workbooks", {
  project <- tempfile("project")
  dir.create(file.path(project, "specs"), recursive = TRUE)
  project <- normalizePath(project, winslash = "/")

  file.create(file.path(project, "specs", "TEST_DV_Spec_R9.xlsx"))
  file.create(file.path(project, "specs", "~$TEST_DV_Spec_R9.xlsx"))

  config_path <- file.path(project, "wealthdv_config.yaml")
  suppressMessages(create_wealthdv_config(config_path, round = 9))

  lines <- readLines(config_path)
  lines <- sub("^spec_folder: .*$", paste0("spec_folder: '", file.path(project, "specs"), "'"), lines)
  lines <- sub("^output_folder: .*$", paste0("output_folder: '", project, "/outputs/R{round}'"), lines)
  lines <- sub("^suite_folder: .*$", paste0("suite_folder: '", project, "/dv_suite/R{round}'"), lines)
  writeLines(lines, config_path)

  output <- capture.output(read_wealthdv_config(config_path), type = "message")
  expect_true(any(grepl("close all open workbook", output, ignore.case = TRUE)))

  config <- suppressMessages(read_wealthdv_config(config_path))
  expect_identical(basename(config$spec_files), "TEST_DV_Spec_R9.xlsx")
})


test_that("output paths sit in the output folder, which is created", {
  config <- suppressMessages(read_wealthdv_config(make_project()))

  expect_true(dir.exists(config$output_folder))
  expect_identical(config$plan_file, file.path(config$output_folder, "dv_plan.csv"))
  expect_identical(config$trial_suite_folder, file.path(config$output_folder, "dv_suite_trial"))
})


test_that("an unset data file is reported, not an error", {
  expect_message(read_wealthdv_config(make_project()), "data_file.*not set")
})


test_that("a missing round or setting stops with a clear message", {
  config_path <- make_project()
  lines <- readLines(config_path)

  writeLines(sub("^round: .*$", "round:", lines), config_path)
  expect_error(suppressMessages(read_wealthdv_config(config_path)), "whole number")

  writeLines(lines[!grepl("^data_file:", lines)], config_path)
  expect_error(suppressMessages(read_wealthdv_config(config_path)), "missing settings")
})


test_that("a missing config file points to create_wealthdv_config()", {
  expect_message(
    try(read_wealthdv_config(tempfile(fileext = ".yaml")), silent = TRUE),
    "create_wealthdv_config"
  )
})


test_that("an existing config is not overwritten", {
  config_path <- make_project(round = 10)
  before <- readLines(config_path)

  suppressMessages(create_wealthdv_config(config_path, round = 11))

  expect_identical(readLines(config_path), before)
})


test_that("name_case defaults to auto when missing and must be recognised", {
  config_path <- make_project()
  lines <- readLines(config_path)

  expect_identical(suppressMessages(read_wealthdv_config(config_path))$name_case, "auto")

  writeLines(lines[!grepl("^name_case:", lines)], config_path)
  expect_identical(suppressMessages(read_wealthdv_config(config_path))$name_case, "auto")

  writeLines(sub("^name_case: .*$", "name_case: 'camel'", lines), config_path)
  expect_error(suppressMessages(read_wealthdv_config(config_path)), "name_case")
})


test_that("scripts_folder defaults to the output folder and fills in the round", {
  config_path <- make_project()
  lines <- readLines(config_path)

  config <- suppressMessages(read_wealthdv_config(config_path))
  expect_identical(config$scripts_folder, file.path(config$output_folder, "dv_scripts"))

  writeLines(lines[!grepl("^scripts_folder:", lines)], config_path)
  expect_identical(suppressMessages(read_wealthdv_config(config_path))$scripts_folder, config$scripts_folder)

  writeLines(sub("^scripts_folder: .*$", "scripts_folder: 'D:/git/was-dvs-R{round}'", lines), config_path)
  expect_identical(suppressMessages(read_wealthdv_config(config_path))$scripts_folder, "D:/git/was-dvs-R10")
})


test_that("the round must be a whole number", {
  expect_error(suppressMessages(create_wealthdv_config(tempfile(), round = 9.5)), "whole number")
})


test_that("topics and sheet_priority are empty when missing or left as []", {
  config_path <- make_project()
  lines <- readLines(config_path)

  config <- suppressMessages(read_wealthdv_config(config_path))
  expect_identical(config$topics, character())
  expect_identical(config$sheet_priority, character())

  writeLines(lines[!grepl("^(topics|sheet_priority):", lines)], config_path)
  config <- suppressMessages(read_wealthdv_config(config_path))
  expect_identical(config$topics, character())
  expect_identical(config$sheet_priority, character())
})


test_that("topics are read and checked against the spec workbooks", {
  config_path <- make_project()
  lines <- readLines(config_path)

  writeLines(sub("^topics: .*$", "topics:\n  - 'income'", lines), config_path)
  config <- suppressMessages(read_wealthdv_config(config_path))
  expect_identical(config$topics, "income")
  expect_identical(config$all_topics, "income")

  writeLines(sub("^topics: .*$", "topics:\n  - 'incme'", lines), config_path)
  expect_error(suppressMessages(read_wealthdv_config(config_path)), "unknown topic")
})


test_that("sheet_priority fills in the round", {
  config_path <- make_project(round = 10)
  lines <- readLines(config_path)

  writeLines(sub("^sheet_priority: .*$", "sheet_priority:\n  - 'R{round}_Pension_Income'", lines), config_path)

  expect_identical(suppressMessages(read_wealthdv_config(config_path))$sheet_priority, "R10_Pension_Income")
})


test_that("show_topics lists each spec workbook's topic", {
  config <- suppressMessages(read_wealthdv_config(make_project(round = 10)))

  topics <- suppressMessages(show_topics(config))

  expect_identical(topics$topic, "income")
  expect_identical(topics$spec_file, "Income_DV_Spec_R10.xlsx")
  expect_true(topics$chosen)
})
