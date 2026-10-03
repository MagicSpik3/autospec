# Fill in a dv_config.xlsx the way a user would in Excel
set_run_config <- function(config_file, data_file = NULL, save_as = NULL, run = NULL, order = NULL) {
  workbook <- openxlsx::loadWorkbook(config_file)
  settings <- readxl::read_excel(config_file, sheet = "Settings")
  topics <- readxl::read_excel(config_file, sheet = "Topics")

  set_setting <- function(name, value) {
    if (!is.null(value)) {
      openxlsx::writeData(workbook, "Settings", value, startCol = 2, startRow = match(name, settings$Setting) + 1L)
    }
  }

  set_topic <- function(column, values) {
    for (file in names(values)) {
      openxlsx::writeData(workbook, "Topics", values[[file]],
                          startCol = match(column, names(topics)), startRow = match(file, topics$File) + 1L)
    }
  }

  set_setting("Data file", data_file)
  set_setting("Save as", save_as)
  set_topic("Run?", run)
  set_topic("Order", order)

  openxlsx::saveWorkbook(workbook, config_file, overwrite = TRUE)
  invisible(config_file)
}
