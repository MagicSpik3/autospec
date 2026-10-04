readiness_totals_fixture <- function() {
  catalogue <- data.frame(
    uid = c("REQ-TOTAL-001", "REQ-TOTAL-002", "REQ-TOTAL-003"),
    file_name = "totals.csv",
    sheet_name = "Person",
    level = "person",
    excel_row = 2:4,
    variable = c("D", "E", "F"),
    label = c("A plus B", "B plus C", "D plus E"),
    instructions = c("D = A + B", "E = B + C", "F = D + E"),
    stringsAsFactors = FALSE
  )
  plan <- suppressMessages(build_dv_plan(catalogue))
  plan$status <- "reviewed"
  plan$reviewed_by <- "testthat"
  plan$reviewed_on <- "2026-10-04"

  suite_dir <- tempfile("totals_suite")
  suppressMessages(write_dv_suite(
    plan,
    suite_dir,
    data_names = c("A", "B", "C"),
    name_case = "spec"
  ))

  list(
    suite = suppressMessages(read_dv_suite(suite_dir)),
    data = data.frame(A = c(1, 2, 3), B = c(10, 20, 30), C = c(100, 200, 300))
  )
}


test_that("readiness follows source columns and requires producer run evidence", {
  fixture <- readiness_totals_fixture()

  source_readiness <- check_dv_readiness(fixture$data, fixture$suite, dvs = c("D", "E"))
  expect_true(all(source_readiness$ready))

  final_readiness <- check_dv_readiness(fixture$data, fixture$suite, dvs = "F")
  expect_false(final_readiness$ready)
  expect_identical(final_readiness$blocked_by_uids, "REQ-TOTAL-001; REQ-TOTAL-002")
  expect_error(
    suppressMessages(run_dv_suite(
      fixture$data,
      fixture$suite,
      dvs = "F",
      with_inputs = FALSE,
      require_ready = TRUE
    )),
    "not ready"
  )

  staged <- suppressMessages(run_dv_suite(
    fixture$data,
    fixture$suite,
    dvs = "D",
    with_inputs = FALSE
  ))
  staged <- suppressMessages(run_dv_suite(
    staged,
    fixture$suite,
    dvs = "E",
    with_inputs = FALSE,
    require_ready = TRUE
  ))
  final_readiness <- check_dv_readiness(staged, fixture$suite, dvs = "F")
  expect_true(final_readiness$ready)
  expect_identical(final_readiness$blocked_by_uids, "")
  expect_setequal(attr(staged, "dv_suite_evidence")$uid, c("REQ-TOTAL-001", "REQ-TOTAL-002"))

  final <- suppressMessages(run_dv_suite(
    staged,
    fixture$suite,
    dvs = "F",
    with_inputs = FALSE,
    require_ready = TRUE
  ))
  expect_identical(final$F, c(121, 242, 363), ignore_attr = TRUE)
})


test_that("existing derived columns need trusted status or successful UID evidence", {
  fixture <- readiness_totals_fixture()
  existing_columns <- transform(fixture$data, D = A + B, E = B + C)

  readiness <- check_dv_readiness(existing_columns, fixture$suite, dvs = "F")
  expect_false(readiness$ready)
  expect_identical(readiness$blocked_by_uids, "REQ-TOTAL-001; REQ-TOTAL-002")

  trusted <- check_dv_readiness(
    existing_columns,
    fixture$suite,
    dvs = "F",
    trusted_inputs = c("D", "E")
  )
  expect_true(trusted$ready)
  expect_no_error(suppressMessages(run_dv_suite(
    existing_columns,
    fixture$suite,
    dvs = "F",
    with_inputs = FALSE,
    require_ready = TRUE,
    trusted_inputs = c("D", "E")
  )))
})


test_that("run evidence keeps the latest outcome per requirement UID", {
  previous <- data.frame(
    uid = c("REQ-1", "REQ-2"),
    dv = c("D", "E"),
    outcome = c("built", "built"),
    suite_signature = c("sig-d1", "sig-e1"),
    stringsAsFactors = FALSE
  )
  current <- data.frame(
    uid = c("REQ-1", "REQ-3"),
    dv = c("D", "F"),
    outcome = c("failed", "built"),
    suite_signature = c("sig-d2", "sig-f1"),
    stringsAsFactors = FALSE
  )

  evidence <- autospec:::update_dv_suite_evidence(previous, current)

  expect_identical(evidence$uid, c("REQ-2", "REQ-1", "REQ-3"))
  expect_identical(evidence$outcome, c("built", "failed", "built"))
  expect_identical(evidence$suite_signature, c("sig-e1", "sig-d2", "sig-f1"))
})


test_that("a step signature changes when its implementation or settings change", {
  step <- list(
    uid = "REQ-1",
    dv = "D",
    inputs = "A",
    missing_code = NULL,
    derive = function(df) {
      df$D <- df$A + 1
      df
    }
  )
  settings <- list(na_as_zero = TRUE)

  signature <- autospec:::suite_step_signature(step, settings)
  expect_identical(autospec:::suite_step_signature(step, settings), signature)

  changed_step <- step
  changed_step$derive <- function(df) {
    df$D <- df$A + 2
    df
  }
  expect_false(identical(autospec:::suite_step_signature(changed_step, settings), signature))
  expect_false(identical(
    autospec:::suite_step_signature(step, list(na_as_zero = FALSE)),
    signature
  ))
})


test_that("an empty suite produces an empty readiness report", {
  suite_dir <- tempfile("empty_suite")
  dir.create(suite_dir)
  writeLines("settings <- list()", file.path(suite_dir, "settings.R"))
  writeLines("steps <- list()", file.path(suite_dir, "empty.R"))
  suite <- suppressMessages(read_dv_suite(suite_dir))

  report <- check_dv_readiness(data.frame(A = 1), suite)

  expect_identical(nrow(report), 0L)
  expect_true(all(c("uid", "dv", "ready", "state") %in% names(report)))
})


test_that("persistent evidence requires both a ledger path and snapshot ID", {
  fixture <- readiness_totals_fixture()

  expect_error(
    check_dv_readiness(fixture$data, fixture$suite, evidence_file = tempfile()),
    "must be supplied together"
  )
  expect_error(
    check_dv_readiness(fixture$data, fixture$suite, evidence_id = "snapshot-1"),
    "must be supplied together"
  )
})
