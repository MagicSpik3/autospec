# A config like read_autospec_config() returns, with everything in a temporary folder
fake_config <- function(topics = character(), all_topics = "test", sheet_priority = character()) {
  project <- tempfile("project")
  output_folder <- file.path(project, "outputs")
  dir.create(output_folder, recursive = TRUE)

  list(
    round = 9L,
    spec_folder = file.path(project, "specs"),
    spec_files = character(),
    exclude_sheets = character(),
    name_case = "auto",
    output_folder = output_folder,
    suite_folder = file.path(project, "dv_suite"),
    catalogue_file = file.path(output_folder, "QA_derivation_catalogue.csv"),
    plan_file = file.path(output_folder, "dv_plan.csv"),
    duplicates_file = file.path(output_folder, "QA_duplicate_dvs.csv"),
    trial_suite_folder = file.path(output_folder, "dv_suite_trial"),
    trial_report_file = file.path(output_folder, "QA_dv_suite_trial_report.csv"),
    report_file = file.path(output_folder, "QA_dv_suite_report.csv"),
    topics = topics,
    all_topics = all_topics,
    sheet_priority = sheet_priority
  )
}


# One plan row, signed off unless said otherwise
fake_plan_row <- function(dv, verb = NA_character_, args = NA_character_,
                          condition = NA_character_, status = "reviewed",
                          reviewed_by = "JD", label = NA_character_,
                          file_name = "Test_DV_Spec_R9.xlsx", sheet_name = "Sheet1",
                          instructions = "spec text", notes = NA_character_) {
  data.frame(
    dv = dv, level = "person", verb = verb, inputs = NA_character_, args = args,
    condition = condition, status = status, reviewed_by = reviewed_by,
    reviewed_on = NA_character_, notes = notes,
    file_name = file_name, sheet_name = sheet_name, excel_row = 2L,
    label = label, instructions = instructions, stringsAsFactors = FALSE
  )
}


# Collect the messages an expression prints
messages_from <- function(expr) {
  messages <- character()
  withCallingHandlers(expr, message = function(condition) {
    messages <<- c(messages, conditionMessage(condition))
    invokeRestart("muffleMessage")
  })
  messages
}
