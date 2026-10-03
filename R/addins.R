#' RStudio add-ins
#'
#' The autospec entries in RStudio's **Addins** menu. Each reads
#' `autospec_config.yaml` in the working folder and runs one step:
#' [autospec_status()], [update_dv_plan()], [trial_run()] (asking which topics)
#' or [sync_dv_suite()]. They use `was_data` when it is loaded; otherwise the
#' trial loads the data file into `was_data`, and the others read only its
#' column names. The trial's result is saved as `trial_data`.
#'
#' @return Whatever the step returns, invisibly.
#' @name autospec_addins
#' @keywords internal
NULL


#' @rdname autospec_addins
#' @export
addin_status <- function() {
  config <- addin_config()
  autospec_status(config, data_names = addin_data_names(config))
}


#' @rdname autospec_addins
#' @export
addin_update_plan <- function() {
  update_dv_plan(addin_config())
}


#' @rdname autospec_addins
#' @export
addin_trial_run <- function() {
  config <- addin_config()

  chosen <- utils::select.list(
    config$all_topics,
    preselect = config$topics,
    multiple = TRUE,
    title = "Topics to try (none for all)",
    graphics = TRUE
  )

  trial <- trial_run(addin_data(config), config, topics = chosen)
  assign("trial_data", trial, envir = globalenv())
  cli::cli_alert_success("The data with the trial DVs is in {.code trial_data}.")

  invisible(trial)
}


#' @rdname autospec_addins
#' @export
addin_sync_suite <- function() {
  config <- addin_config()

  sync_dv_suite(
    read_dv_plan(config$plan_file),
    config$suite_folder,
    data_names = addin_data_names(config),
    name_case = config$name_case,
    topics = chosen_topics(config)
  )
}


#' @keywords internal
#' @noRd
addin_config <- function(path = "autospec_config.yaml") {
  if (!file.exists(path)) {
    cli::cli_alert_danger("No {.file {path}} in {.file {getwd()}}. Open autospec.Rproj so R works in the autospec folder.")
    stop("Cannot find the config file.", call. = FALSE)
  }

  read_autospec_config(path)
}


#' The data: `was_data` when loaded, otherwise read from the config's data file into `was_data`
#' @keywords internal
#' @noRd
addin_data <- function(config) {
  if (exists("was_data", envir = globalenv(), inherits = FALSE)) {
    cli::cli_alert_info("Using {.code was_data}, already loaded.")
    return(get("was_data", envir = globalenv()))
  }

  check_data_file(config)
  cli::cli_alert_info("Loading {.file {config$data_file}} into {.code was_data}; this can take a minute.")
  was_data <- haven::read_sav(config$data_file)
  assign("was_data", was_data, envir = globalenv())

  was_data
}


#' The data's column names, without loading the data when it is not loaded already
#' @keywords internal
#' @noRd
addin_data_names <- function(config) {
  if (exists("was_data", envir = globalenv(), inherits = FALSE)) {
    return(names(get("was_data", envir = globalenv())))
  }

  if (!nzchar(config$data_file) || !file.exists(config$data_file) ||
      !requireNamespace("haven", quietly = TRUE)) {
    cli::cli_alert_info("The data file cannot be read, so names are not checked against it.")
    return(NULL)
  }

  names(haven::read_sav(config$data_file, n_max = 0))
}


#' @keywords internal
#' @noRd
check_data_file <- function(config) {
  if (!requireNamespace("haven", quietly = TRUE)) {
    cli::cli_alert_danger("The haven package is needed to read the data. Run setup_autospec.R to install it.")
    stop("haven is not installed.", call. = FALSE)
  }

  if (!nzchar(config$data_file) || !file.exists(config$data_file)) {
    cli::cli_alert_danger("Cannot find the data file {.file {config$data_file}}; set {.field data_file} in the config.")
    stop("Cannot find the data file.", call. = FALSE)
  }

  invisible(NULL)
}
