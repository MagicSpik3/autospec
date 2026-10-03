make_csv_project <- function() {
  repo_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/")
  spec_source <- file.path(repo_root, "specs", "TEST.csv")

  project <- tempfile("autospec_csv")
  spec_dir <- file.path(project, "specs")
  dir.create(spec_dir, recursive = TRUE)

  file.copy(spec_source, file.path(spec_dir, "TEST.csv"), overwrite = TRUE)

  config_path <- file.path(project, "autospec_config.yaml")
  config_lines <- c(
    "round: 9",
    paste0("spec_folder: '", spec_dir, "'"),
    "spec_pattern: 'TEST.*[.]csv$'",
    "exclude_sheets:",
    "  - 'Major Changes'",
    "data_file: 'dummy_data.csv'",
    "name_case: 'lower'",
    paste0("output_folder: '", project, "/outputs/R{round}'"),
    paste0("suite_folder: '", project, "/dv_suite/R{round}'"),
    "output_data_file: ''",
    "topics: []",
    "sheet_priority: []"
  )
  writeLines(config_lines, config_path)

  suppressMessages(read_autospec_config(config_path))
}


test_that("the sample TEST.csv spec is accepted by the catalogue and plan pipeline", {
  config <- make_csv_project()

  catalogue <- suppressMessages(make_catalogue(config$spec_files))

  expect_gt(nrow(catalogue), 0L)
  expect_true(any(grepl("var_f", catalogue$variable, ignore.case = TRUE)))
  expect_true(any(grepl("var_j", catalogue$variable, ignore.case = TRUE)))

  plan <- suppressMessages(update_dv_plan(config, data_names = c("var_a", "var_b", "var_d", "var_f")))

  expect_true(file.exists(config$plan_file))
  expect_true(any(grepl("var_j", plan$dv, ignore.case = TRUE)))
})


test_that("the sample TEST.csv spec can be turned into a runnable suite", {
  config <- make_csv_project()

  suppressMessages(update_dv_plan(config, data_names = c("var_a", "var_b", "var_d", "var_f")))
  plan <- suppressMessages(read_dv_plan(config$plan_file))
  suite_dir <- file.path(config$output_folder, "suite_test")
  suppressMessages(write_dv_suite(plan, suite_dir, data_names = c("var_a", "var_b", "var_d", "var_f")))

  topic_file <- list.files(file.path(suite_dir, "tests"), pattern = "^test-.*\\.R$", full.names = TRUE)

  expect_true(dir.exists(suite_dir))
  expect_true(file.exists(file.path(suite_dir, "settings.R")))
  expect_true(file.exists(file.path(suite_dir, "tests", "helper-suite.R")))
  expect_true(length(topic_file) > 0L)
})
