#' Try the DVs out on the data, before anything is signed off
#'
#' Writes every drafted call as working code into a throwaway trial suite,
#' replacing any earlier trial, and runs it on the chosen topics. Nothing is
#' signed off and neither the plan nor the real suite is touched, so there is
#' no need to mark rows reviewed to see output. A DV on several rows is built
#' from the first. The trial uses the real suite's `settings.R` when there is
#' one, so the discount rate and sentinel choices made there apply.
#'
#' @param df The data to add DVs to.
#' @param config The config from [read_autospec_config()].
#' @param topics Topic names to run; defaults to `topics` in the config, or
#'   every topic when that is empty. Steps in other topics that build an input
#'   are run too.
#' @param plan Optional plan; by default the plan file, or a plan drafted
#'   fresh from the specs when there is no plan file yet.
#'
#' @return `df` with the trial DVs added and the report attached, as from
#'   [run_dv_suite()]. The report is also saved to the trial report file.
#' @export
#'
#' @examples
#' \dontrun{
#' config <- read_autospec_config()
#' trial <- trial_run(was_data, config, topics = "property_wealth")
#' }
trial_run <- function(df, config, topics = config$topics, plan = NULL) {
  check_config_object(config)

  if (!is.data.frame(df)) {
    cli::cli_alert_danger("{.arg df} must be the data to add DVs to.")
    stop("`df` must be a data frame.", call. = FALSE)
  }

  topics <- chosen_topics(config, topics)

  if (is.null(plan)) {
    plan <- trial_plan(config)
  }

  trial_dir <- config$trial_suite_folder
  unlink(trial_dir, recursive = TRUE)

  suppressMessages(write_suite_folder(
    plan, trial_dir, data_names = names(df), reviewed_only = FALSE,
    name_case = config$name_case, overwrite = TRUE, strict_duplicates = FALSE
  ))

  cli::cli_h1("Trial run")
  cli::cli_alert_info("Every drafted DV is written as working code in {.file {trial_dir}}; sign-offs are not needed and nothing is changed in the plan or the suite.")

  real_settings <- file.path(config$suite_folder, "settings.R")

  if (file.exists(real_settings)) {
    file.copy(real_settings, file.path(trial_dir, "settings.R"), overwrite = TRUE)
    cli::cli_alert_info("Using the settings in {.file {real_settings}}.")
  } else {
    cli::cli_alert_info("Using the default settings; pension DVs need {.field rate} set in {.file {file.path(trial_dir, 'settings.R')}}.")
  }

  suite <- suppressMessages(read_dv_suite(trial_dir))
  report_missing_inputs(suite$steps, names(df), topics)

  result <- run_dv_suite(df, suite, topics = topics, report_file = config$trial_report_file)

  cli::cli_alert_info("This was a trial. To make it real, sign off rows in the plan and run {.code sync_dv_suite()}.")

  result
}


#' The plan a trial runs: the plan file, or one drafted fresh from the specs
#' @keywords internal
#' @noRd
trial_plan <- function(config) {
  if (file.exists(config$plan_file)) {
    cli::cli_alert_info("Trying out the drafted calls in {.file {config$plan_file}}.")
    return(suppressMessages(read_dv_plan(config$plan_file)))
  }

  if (length(config$spec_files) == 0L) {
    cli::cli_alert_danger("There is no plan and no spec workbooks to draft one from; check {.field spec_folder} in the config.")
    stop("Nothing to try out.", call. = FALSE)
  }

  cli::cli_alert_info("No plan yet, so one is drafted from the specs for the trial (not saved).")
  data_catalogue <- suppressMessages(make_catalogue(config$spec_files, exclude_sheets = config$exclude_sheets))
  suppressMessages(build_dv_plan(data_catalogue, sheet_priority = config$sheet_priority))
}


#' Say which inputs of the chosen steps neither the data nor any step supplies
#'
#' When many of them end in `_i` and the data has them without it, the data
#' is probably not the imputed file, and saying so saves a run of skipped steps.
#' @keywords internal
#' @noRd
report_missing_inputs <- function(steps, data_names, topics = NULL) {
  step_topics <- vapply(steps, function(step) if (is.null(step$topic)) NA_character_ else step$topic, character(1L))
  chosen <- if (is.null(topics)) steps else steps[step_topics %in% topics]

  inputs <- unique(unlist(lapply(chosen, `[[`, "inputs"), use.names = FALSE))
  missing_inputs <- inputs[!tolower(inputs) %in% tolower(c(data_names, names(steps)))]

  if (length(missing_inputs) == 0L) {
    cli::cli_alert_success("Every input the chosen DVs need is in the data or built by a step.")
    return(invisible(missing_inputs))
  }

  needing <- sum(vapply(chosen, function(step) any(step$inputs %in% missing_inputs), logical(1L)))

  cli::cli_alert_warning(
    "{length(missing_inputs)} input{?s} {?is/are} neither in the data nor built by any step, so {needing} DV{?s} will be skipped: {.val {utils::head(missing_inputs, 10)}}{if (length(missing_inputs) > 10) ', ...' else ''}"
  )

  ends_in_i <- grepl("_i$", missing_inputs, ignore.case = TRUE)
  without_i <- tolower(sub("_i$", "", missing_inputs, ignore.case = TRUE))
  imputed_gap <- sum(ends_in_i & without_i %in% tolower(data_names))

  if (imputed_gap > 0L) {
    cli::cli_alert_warning(
      "{imputed_gap} of them end in {.val _i} and the data has {?it/them} without it. The data may not be the imputed file; check {.field data_file} in the config."
    )
  }

  invisible(missing_inputs)
}
