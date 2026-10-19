#' Record in the plan what the suite shows for each DV
#'
#' Reads the suite to see which DVs are written and which have a unit test,
#' filled in or still a stub, then runs the unit tests and records which pass.
#' The four tracking columns of the plan (`DV Written`, `Unit Test Stub`,
#' `Unit Test Populated`, `Unit Test Passed`) are set from what it finds, so
#' values typed there are replaced. The old plan is backed up first.
#'
#' @param config The config from [read_autospec_config()].
#'
#' @return The plan, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' update_dv_tracking(read_autospec_config())
#' }
update_dv_tracking <- function(config) {
  check_config_object(config)

  if (!file.exists(config$plan_file)) {
    cli::cli_alert_danger("There is no plan yet at {.file {config$plan_file}}. Draft it first: run the {.field plan} chunk, or Addins > autospec: Update the plan.")
    stop("There is no plan yet.", call. = FALSE)
  }

  plan <- read_dv_plan(config$plan_file)
  tracking <- suite_tracking(config$suite_folder)
  results <- unit_test_results(config$suite_folder)
  tracking$`Unit Test Passed` <- results$`Unit Test Passed`[match(tracking$dv, results$dv)]

  recorded <- record_tracking(plan, tracking)

  backup_plan_file(config)
  save_plan_file(recorded, config$plan_file)

  count <- function(column) sum(tracking_value(recorded, column) %in% "yes")
  cli::cli_alert_success(
    "Plan updated from the suite: {count('DV Written')} DV{?s} written, {count('Unit Test Populated')} with a filled-in unit test, {count('Unit Test Passed')} passing."
  )

  invisible(recorded)
}
