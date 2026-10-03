test_that("the excluded sheets are returned as exact names", {
  excluded <- default_exclude_sheets(round = 9)

  expect_type(excluded, "character")
  expect_false(any(duplicated(excluded)))
})


test_that("the leading space in a sheet name is preserved", {
  # The name is matched exactly against the workbook, and this sheet really
  # is called " Final accounting structure".
  expect_true(" Final accounting structure" %in% default_exclude_sheets())
})


test_that("change logs and imputation lists are excluded", {
  excluded <- default_exclude_sheets(round = 9)

  expect_true(
    all(
      c(
        "Imputation_list",
        "Change_log",
        "LA Change_log",
        "DV Change Log",
        "R9_finalised_changelog"
      ) %in% excluded
    )
  )
})


test_that("sheets are excluded by exact name, not by pattern", {
  # Sheets such as R9_Housing_Costs must survive: only the listed names go.
  sheet_names <- c(
    "Imputation_list",
    "R9_Housing_Costs",
    "Change_log",
    "Physical_R9"
  )

  expect_identical(
    setdiff(sheet_names, default_exclude_sheets()),
    c("R9_Housing_Costs", "Physical_R9")
  )
})


test_that("sheet names carrying the round follow the round asked for", {
  expect_true("R10_finalised_changelog" %in% default_exclude_sheets(round = 10))
  expect_false(any(grepl("R9", default_exclude_sheets(round = 10))))
  expect_false(any(grepl("{round}", default_exclude_sheets(), fixed = TRUE)))
  expect_false(any(grepl("R[0-9]+_", default_exclude_sheets())))
})

