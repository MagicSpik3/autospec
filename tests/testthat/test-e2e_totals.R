test_that("totals wait for producer UIDs and then run in dependency order", {
  repo_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/")
  project <- tempfile("autospec_totals")
  spec_folder <- file.path(project, "specs")
  data_folder <- file.path(project, "test_data")
  dir.create(spec_folder, recursive = TRUE)
  dir.create(data_folder, recursive = TRUE)
  file.copy(file.path(repo_root, "specs", "totals.csv"), spec_folder)
  file.copy(file.path(repo_root, "test_data", "totals_input.csv"), data_folder)

  config_path <- file.path(project, "autospec_config.yaml")
  writeLines(c(
    "round: 1",
    paste0("spec_folder: '", spec_folder, "'"),
    "spec_pattern: 'totals[.]csv$'",
    "exclude_sheets: []",
    paste0("data_file: '", file.path(data_folder, "totals_input.csv"), "'"),
    "name_case: 'spec'",
    paste0("output_folder: '", project, "/outputs/R{round}'"),
    paste0("suite_folder: '", project, "/dv_suite/R{round}'"),
    "output_data_file: ''"
  ), config_path)

  config <- suppressMessages(read_autospec_config(config_path))
  input <- utils::read.csv(config$data_file, check.names = FALSE)
  plan <- suppressMessages(update_dv_plan(config, data_names = names(input)))
  expect_identical(plan$uid, c("REQ-TOTAL-003", "REQ-TOTAL-002", "REQ-TOTAL-001"))

  input_audit <- make_required_input_table(plan, data_names = names(input))
  expect_identical(input_audit$provided_by_uids[input_audit$variable == "D"], "REQ-TOTAL-001")
  expect_identical(input_audit$provided_by_uids[input_audit$variable == "E"], "REQ-TOTAL-002")

  plan$status <- "reviewed"
  plan$reviewed_by <- "testthat"
  plan$reviewed_on <- "2026-10-04"
  suite_dir <- config$suite_folder
  suppressMessages(write_dv_suite(plan, suite_dir, data_names = names(input), name_case = "spec"))
  suite <- suppressMessages(read_dv_suite(suite_dir))

  before <- check_dv_readiness(input, suite)
  expect_true(all(before$ready[before$dv %in% c("D", "E")]))
  expect_false(before$ready[before$dv == "F"])
  expect_identical(before$blocked_by_uids[before$dv == "F"], "REQ-TOTAL-001; REQ-TOTAL-002")
  expect_error(
    suppressMessages(run_dv_suite(
      input, suite, dvs = "F", with_inputs = FALSE, require_ready = TRUE
    )),
    "not ready"
  )
  expect_error(
    suppressMessages(run_dv_suite(input, suite, require_ready = NA)),
    "require_ready"
  )

  totals <- suppressMessages(run_dv_suite(input, suite, stop_on_error = TRUE))
  total_report <- attr(totals, "dv_suite_report")
  expect_identical(total_report$dv, c("E", "D", "F"))
  expect_true(all(total_report$outcome == "built"))
  expect_identical(totals$F, c(121, 242, 363), ignore_attr = TRUE)

  stage_one <- suppressMessages(run_dv_suite(
    input,
    suite,
    dvs = "D",
    with_inputs = FALSE,
    require_ready = TRUE
  ))
  stage_one <- suppressMessages(run_dv_suite(
    stage_one,
    suite,
    dvs = "E",
    with_inputs = FALSE,
    require_ready = TRUE
  ))
  expect_true(all(attr(stage_one, "dv_suite_evidence")$outcome == "built"))
  expect_setequal(attr(stage_one, "dv_suite_evidence")$uid, c("REQ-TOTAL-001", "REQ-TOTAL-002"))

  after_producers <- check_dv_readiness(stage_one, suite, dvs = "F")
  expect_true(after_producers$ready)
  expect_identical(after_producers$blocked_by_uids, "")

  final <- suppressMessages(run_dv_suite(
    stage_one,
    suite,
    dvs = "F",
    with_inputs = FALSE,
    require_ready = TRUE
  ))
  expect_identical(final$F, c(121, 242, 363), ignore_attr = TRUE)
  expect_identical(attr(final, "dv_suite_report")$uid, "REQ-TOTAL-003")

  evidence_file <- file.path(project, "run_evidence.csv")
  stage_d <- suppressMessages(run_dv_suite(
    input,
    suite,
    dvs = "D",
    with_inputs = FALSE,
    require_ready = TRUE,
    evidence_file = evidence_file,
    evidence_id = "totals-snapshot-2026-10-04"
  ))
  stage_d_file <- file.path(project, "after_d.csv")
  utils::write.csv(stage_d, stage_d_file, row.names = FALSE)
  reloaded_after_d <- utils::read.csv(stage_d_file, check.names = FALSE)

  expect_true(check_dv_readiness(
    reloaded_after_d,
    suite,
    dvs = "E",
    evidence_file = evidence_file,
    evidence_id = "totals-snapshot-2026-10-04"
  )$ready)

  stage_e <- suppressMessages(run_dv_suite(
    reloaded_after_d,
    suite,
    dvs = "E",
    with_inputs = FALSE,
    require_ready = TRUE,
    evidence_file = evidence_file,
    evidence_id = "totals-snapshot-2026-10-04"
  ))
  stage_e_file <- file.path(project, "after_e.csv")
  utils::write.csv(stage_e, stage_e_file, row.names = FALSE)
  reloaded_after_e <- utils::read.csv(stage_e_file, check.names = FALSE)

  expect_true(check_dv_readiness(
    reloaded_after_e,
    suite,
    dvs = "F",
    evidence_file = evidence_file,
    evidence_id = "totals-snapshot-2026-10-04"
  )$ready)
  expect_false(check_dv_readiness(
    reloaded_after_e,
    suite,
    dvs = "F",
    evidence_file = evidence_file,
    evidence_id = "another-snapshot"
  )$ready)

  final_reloaded <- suppressMessages(run_dv_suite(
    reloaded_after_e,
    suite,
    dvs = "F",
    with_inputs = FALSE,
    require_ready = TRUE,
    evidence_file = evidence_file,
    evidence_id = "totals-snapshot-2026-10-04"
  ))
  expect_identical(final_reloaded$F, c(121, 242, 363), ignore_attr = TRUE)

  changed_suite <- suite
  changed_suite$steps$D$derive <- function(df) {
    df$D <- df$A + 999
    df
  }
  suppressMessages(run_dv_suite(
    reloaded_after_e,
    changed_suite,
    dvs = "D",
    with_inputs = FALSE,
    evidence_file = evidence_file,
    evidence_id = "totals-snapshot-2026-10-04"
  ))
  stale_code_readiness <- check_dv_readiness(
    reloaded_after_e,
    suite,
    dvs = "F",
    evidence_file = evidence_file,
    evidence_id = "totals-snapshot-2026-10-04"
  )
  expect_false(stale_code_readiness$ready)
  expect_identical(stale_code_readiness$blocked_by_uids, "REQ-TOTAL-001")

  broken_suite <- suite
  broken_suite$steps$D$derive <- function(df) stop("producer failed")
  expect_error(suppressMessages(run_dv_suite(
    reloaded_after_e,
    broken_suite,
    dvs = "D",
    with_inputs = FALSE,
    stop_on_error = TRUE,
    evidence_file = evidence_file,
    evidence_id = "totals-snapshot-2026-10-04"
  )), "Building D failed")

  after_failed_rerun <- check_dv_readiness(
    reloaded_after_e,
    suite,
    dvs = "F",
    evidence_file = evidence_file,
    evidence_id = "totals-snapshot-2026-10-04"
  )
  expect_false(after_failed_rerun$ready)
  expect_identical(after_failed_rerun$blocked_by_uids, "REQ-TOTAL-001")
})
