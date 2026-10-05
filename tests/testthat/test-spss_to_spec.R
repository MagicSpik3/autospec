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
})
