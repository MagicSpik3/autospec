all_verbs_project <- function() {
  repo_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/")
  project <- tempfile("autospec_all_verbs")
  spec_folder <- file.path(project, "specs")
  data_folder <- file.path(project, "test_data")
  dir.create(spec_folder, recursive = TRUE)
  dir.create(data_folder, recursive = TRUE)

  spec_file <- file.path(spec_folder, "TEST_ALL_VERBS.csv")
  data_file <- file.path(data_folder, "test_input.csv")
  file.copy(file.path(repo_root, "specs", "TEST_ALL_VERBS.csv"), spec_file)
  file.copy(file.path(repo_root, "test_data", "test_input.csv"), data_file)

  config_path <- file.path(project, "autospec_config.yaml")
  config_lines <- c(
    "round: 9",
    paste0("spec_folder: '", spec_folder, "'"),
    "spec_pattern: 'TEST_ALL_VERBS[.]csv$'",
    "exclude_sheets: []",
    paste0("data_file: '", data_file, "'"),
    "name_case: 'lower'",
    paste0("output_folder: '", project, "/outputs/R{round}'"),
    paste0("suite_folder: '", project, "/dv_suite/R{round}'"),
    "output_data_file: ''"
  )
  writeLines(config_lines, config_path)

  config <- suppressMessages(read_autospec_config(config_path))
  list(config = config, data = utils::read.csv(data_file, check.names = FALSE))
}


build_all_verbs_demo <- function() {
  fixture <- all_verbs_project()
  config <- fixture$config
  input <- fixture$data

  catalogue <- suppressMessages(make_catalogue(config$spec_files))
  plan <- suppressMessages(update_dv_plan(config, data_names = names(input)))
  input_audit <- make_required_input_table(plan, data_names = names(input))

  plan$status <- "reviewed"
  plan$reviewed_by <- "testthat"
  plan$reviewed_on <- as.character(Sys.Date())
  suppressMessages(write_dv_plan(plan, config$plan_file))
  plan <- suppressMessages(read_dv_plan(config$plan_file))

  suppressMessages(sync_dv_suite(
    plan,
    config$suite_folder,
    data_names = names(input),
    name_case = config$name_case
  ))

  settings_file <- file.path(config$suite_folder, "settings.R")
  settings <- readLines(settings_file)
  settings[settings == "  rate = NULL,"] <- "  rate = 0.05,"
  writeLines(settings, settings_file)

  problems <- suppressMessages(check_dv_suite(config$suite_folder, data_names = names(input)))
  expect_false(
    any(problems$severity == "error"),
    info = paste(problems$dv[problems$severity == "error"],
                 problems$problem[problems$severity == "error"], collapse = "\n")
  )

  result <- suppressMessages(run_dv_suite(
    input,
    config$suite_folder,
    stop_on_error = TRUE
  ))

  list(
    catalogue = catalogue,
    plan = plan,
    input_audit = input_audit,
    suite = suppressMessages(read_dv_suite(config$suite_folder)),
    input = input,
    result = result
  )
}


all_verbs_demo <- build_all_verbs_demo()


test_that("the demo traces every verb and reports every required input", {
  expect_identical(sort(all_verbs_demo$plan$verb), sort(registered_verbs()))
  expect_identical(all_verbs_demo$plan$uid, sprintf("REQ-HW-%03d", seq_len(14L)))

  reordered_plan <- suppressMessages(build_dv_plan(
    all_verbs_demo$catalogue[rev(seq_len(nrow(all_verbs_demo$catalogue))), ],
    data_names = names(all_verbs_demo$input)
  ))
  by_output <- function(plan) {
    plan$uid[match(sort(plan$dv), plan$dv)]
  }
  expect_identical(by_output(reordered_plan), by_output(all_verbs_demo$plan))

  expect_true(
    all(all_verbs_demo$input_audit$available),
    info = paste(all_verbs_demo$input_audit$variable[!all_verbs_demo$input_audit$available], collapse = ", ")
  )
  expect_setequal(names(all_verbs_demo$suite$steps), all_verbs_demo$plan$dv)
})


expected_demo_values <- list(
  var_a = c(2, 3, 4),
  row_total = c(12, 24, 35),
  difference = c(8, 16, 25),
  positive_flag = c(1, 0, 0),
  var_j = c(1, 0, 1),
  any_choice = c(1, 1, 0),
  clean_value = c(10, 0, 0),
  ratio = c(5, 0, 3),
  annualised = c(5200, 5200, 480),
  midpoint = c(2500, 7500, 15000),
  var_f = c(100, 0, 0),
  present_value = c(
    1000 / (1.05 ^ 10),
    1200 / (1.05 ^ 6),
    500 / 1.05
  ),
  hh_total = c(5, 5, 4),
  hh_any = c(1, 1, 0)
)


test_requirement <- function(uid, dv, expected) {
  test_that(paste0("Requirement UID ", uid, ": output for ", dv, " matches the test data"), {
    expect_identical(all_verbs_demo$suite$steps[[dv]]$uid, uid)
    report <- attr(all_verbs_demo$result, "dv_suite_report")
    report_row <- report[report$uid == uid, , drop = FALSE]
    expect_identical(report_row$dv, dv)
    expect_identical(report_row$outcome, "built")
    expect_equal(all_verbs_demo$result[[dv]], expected, ignore_attr = TRUE)
  })
}


for (row_index in seq_len(nrow(all_verbs_demo$plan))) {
  row <- all_verbs_demo$plan[row_index, ]
  test_requirement(row$uid, row$dv, expected_demo_values[[row$dv]])
}
