# A spec workbook with one sheet: a header row and a derivation per variable
write_spec_workbook <- function(path, variables, derivations, sheet = "Sheet1") {
  workbook <- if (file.exists(path)) openxlsx::loadWorkbook(path) else openxlsx::createWorkbook()
  openxlsx::addWorksheet(workbook, sheet)

  rows <- data.frame(
    variable = variables, label = paste("Label for", variables), derivation = derivations,
    notes = NA_character_, e = NA_character_, f = NA_character_, g = NA_character_,
    stringsAsFactors = FALSE
  )
  names(rows) <- c("Variable Name", "Label", "Derivation", "Notes", "E", "F", "G")

  openxlsx::writeData(workbook, sheet, rows)
  openxlsx::saveWorkbook(workbook, path, overwrite = TRUE)
  invisible(path)
}


spec_config <- function(sheet_priority = character()) {
  config <- fake_config(sheet_priority = sheet_priority)
  dir.create(config$spec_folder, recursive = TRUE)
  config$spec_files <- file.path(config$spec_folder, "Test_DV_Spec_R9.xlsx")
  config
}


test_that("the first run writes the catalogue and the plan", {
  skip_if_not_installed("openxlsx")
  config <- spec_config()
  write_spec_workbook(config$spec_files, c("DVa", "DVb"), c("DVa = x", "DVb = y"))

  plan <- suppressMessages(update_dv_plan(config))

  expect_true(file.exists(config$catalogue_file))
  expect_true(file.exists(config$plan_file))
  expect_identical(tolower(plan$dv), c("dva", "dvb"))
})


test_that("running again keeps sign-offs and backs up the old plan", {
  skip_if_not_installed("openxlsx")
  config <- spec_config()
  write_spec_workbook(config$spec_files, c("DVa", "DVb"), c("DVa = x", "DVb = y"))
  plan <- suppressMessages(update_dv_plan(config))
  plan$status[[1L]] <- "reviewed"
  plan$reviewed_by[[1L]] <- "JD"
  suppressMessages(write_dv_plan(plan, config$plan_file))

  plan <- suppressMessages(update_dv_plan(config))

  expect_identical(plan$status[[1L]], "reviewed")
  expect_length(list.files(file.path(config$output_folder, "plan_backups")), 1L)
})


test_that("DVs defined twice with different text are listed, unless sheet_priority settles them", {
  skip_if_not_installed("openxlsx")
  config <- spec_config()
  write_spec_workbook(config$spec_files, "DVa", "DVa = x", sheet = "First")
  write_spec_workbook(config$spec_files, "DVa", "DVa = y", sheet = "Second")

  suppressMessages(update_dv_plan(config))
  expect_true(file.exists(config$duplicates_file))

  config$sheet_priority <- "Second"
  plan <- suppressMessages(update_dv_plan(config))

  expect_false(file.exists(config$duplicates_file))
  expect_identical(plan$status[plan$sheet_name == "First"], "duplicate")
})


test_that("what changed since the last plan is reported", {
  previous <- rbind(
    fake_plan_row("kept", instructions = "kept = x"),
    fake_plan_row("changed", instructions = "changed = x"),
    fake_plan_row("gone")
  )
  plan <- rbind(
    fake_plan_row("kept", instructions = "kept = x"),
    fake_plan_row("changed", status = "needs_review", instructions = "changed = y"),
    fake_plan_row("new", status = "auto")
  )

  messages <- messages_from(report_plan_changes(previous, plan))

  expect_true(any(grepl("1 new row", messages)))
  expect_true(any(grepl("1 row no longer", messages)))
  expect_true(any(grepl("1 sign-off dropped", messages)))
})


test_that("a DV signed off on two rows with the same code is not waiting", {
  plan <- rbind(
    fake_plan_row("b", "dv_copy", args = "source_col = \"a\"", sheet_name = "S1"),
    fake_plan_row("b", "dv_copy", args = "source_col = \"a\"", sheet_name = "S2")
  )

  expect_identical(nrow(duplicates_to_decide(plan)), 0L)

  plan$args[[2L]] <- "source_col = \"c\""
  expect_identical(nrow(duplicates_to_decide(plan)), 2L)
})


test_that("the config must come from read_wealthdv_config()", {
  expect_error(suppressMessages(update_dv_plan(list())), "not a wealthdv config")
})
