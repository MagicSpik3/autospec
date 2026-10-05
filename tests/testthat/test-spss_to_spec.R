test_that("SPSS syntax files can be converted into a spec-like table", {
  root <- normalizePath(file.path(testthat::test_path("..", ".."), fsep = "/"), winslash = "/")
  sps_path <- file.path(root, "test_SPSS", "2_example.sps")

  expect_true(file.exists(sps_path))

  spec <- spss_to_spec(sps_path)

  expect_s3_class(spec, "data.frame")
  expect_true("source_file" %in% names(spec))
  expect_true("source_location" %in% names(spec))
  expect_true("level" %in% names(spec))
  expect_true("variable" %in% names(spec))
  expect_true("label" %in% names(spec))
  expect_true("derivation" %in% names(spec))
  expect_true("notes" %in% names(spec))
  expect_true("spss line number(s)" %in% names(spec))

  expect_true(any(spec$variable == "var_a09"))
  expect_true(any(spec$variable == "var_d04"))
  expect_true(any(spec$level == "household"))
  expect_true(any(spec$level == "person"))
  expect_true(all(nzchar(spec$`spss line number(s)`)))
  expect_true(any(grepl("Recalculate|Rederive|derived|household|grouped by", spec$notes, ignore.case = TRUE)))

  spec_csv_path <- tempfile(fileext = ".csv")
  spss_to_spec(sps_path, output_path = spec_csv_path, quiet = TRUE)
  catalogue <- suppressMessages(make_catalogue(
    spec_csv_path, exclude_sheets = character(), quiet = TRUE
  ))
  expect_setequal(unique(catalogue$level), c("person", "household"))
})


test_that("SPSS can flow through a catalogue, reviewed plan, R suite and result check", {
  root <- normalizePath(file.path(testthat::test_path("..", ".."), fsep = "/"), winslash = "/")
  sps_path <- file.path(root, "test_SPSS", "roundtrip_minimal.sps")
  spec_path <- tempfile(fileext = ".csv")
  suite_dir <- tempfile("spss_roundtrip_suite")
  input <- data.frame(gross = c(20, 15, 8), deductions = c(3, 15, 10))

  spec <- spss_to_spec(sps_path, output_path = spec_path, quiet = TRUE)

  expect_true(file.exists(spec_path))
  expect_true(any(grepl("var_a TO var_b", spec$notes, fixed = TRUE)))

  catalogue <- suppressMessages(make_catalogue(
    spec_path, exclude_sheets = character(), quiet = TRUE
  ))
  expect_identical(catalogue$variable, "net_value")
  expect_identical(catalogue$level, "person")
  expect_true(grepl("var_a TO var_b", catalogue$notes, fixed = TRUE))

  plan <- suppressMessages(build_dv_plan(catalogue, data_names = names(input)))
  expect_identical(plan$verb, "dv_difference")
  expect_identical(plan$inputs, "gross; deductions")
  expect_identical(plan$status, "auto")

  plan$status <- "reviewed"
  plan$reviewed_by <- "testthat"
  plan$reviewed_on <- "2026-10-05"
  suppressMessages(write_dv_suite(
    plan, suite_dir, data_names = names(input), name_case = "lower"
  ))

  suite <- suppressMessages(read_dv_suite(suite_dir))
  expect_true(is.function(suite$steps$net_value$derive))

  result <- suppressMessages(run_dv_suite(input, suite, stop_on_error = TRUE))
  expect_equal(result$net_value, c(17, 0, -2), ignore_attr = TRUE)
  expect_true(all(attr(result, "dv_suite_report")$outcome == "built"))
})
