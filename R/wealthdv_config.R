#' Create the wealthdv config file for a round
#'
#' Writes `wealthdv_config.yaml`: every file path and setting the pipeline needs
#' for one survey round, with a comment on each. Edit it, then read it with
#' [read_wealthdv_config()]. An existing file is left alone unless
#' `overwrite = TRUE`.
#'
#' @param path Where to write the config.
#' @param round The round number, e.g. `10`. Leave `NULL` to fill it in by hand.
#' @param overwrite Whether to replace an existing file.
#'
#' @return `path`, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' create_wealthdv_config(round = 10)
#' }
create_wealthdv_config <- function(path = "wealthdv_config.yaml", round = NULL,
                                   overwrite = FALSE) {
  if (!is.null(round)) {
    check_round(round, "round")
  }

  if (file.exists(path) && !overwrite) {
    cli::cli_alert_warning(
      "{.file {path}} already exists and was left as it is. Use {.code overwrite = TRUE} to replace it."
    )
    return(invisible(path))
  }

  connection <- file(path, open = "w", encoding = "UTF-8")
  on.exit(close(connection))
  writeLines(config_template_lines(round), connection)

  cli::cli_alert_success("Config written to {.file {path}}.")
  cli::cli_ol(c(
    "Open it and set {.field round} and {.field data_file}.",
    "Check {.field spec_folder} points at this round's spec workbooks.",
    "Run {.code config <- read_wealthdv_config()} to check everything is found."
  ))

  invisible(path)
}


#' Read and check the wealthdv config file
#'
#' Reads `wealthdv_config.yaml`, fills in `{round}` wherever it appears, and
#' reports what it finds: the spec workbooks, the data file, and where outputs
#' go. The output folder is created if it does not exist.
#'
#' Besides the settings in the file, the result holds the paths the pipeline
#' writes to: `spec_files`, `catalogue_file`, `plan_file`, `rebuilt_plan_file`,
#' `duplicates_file`, `trial_suite_folder`, `trial_report_file` and
#' `report_file`, and `all_topics`, the topic of every spec workbook.
#'
#' Settings added after the first config files were made are optional and
#' take a default when missing: `name_case` is `"auto"`, `scripts_folder` is
#' a `dv_scripts` folder inside the output folder, `topics` is empty (every
#' topic) and `sheet_priority` is empty (no rule).
#'
#' @param path The config file.
#'
#' @return A named list of settings and paths, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' config <- read_wealthdv_config()
#' config$spec_files
#' }
read_wealthdv_config <- function(path = "wealthdv_config.yaml") {
  if (!is.character(path) || length(path) != 1L || !file.exists(path)) {
    cli::cli_alert_danger(
      "Cannot find the config file {.file {path}}. Create one with {.code create_wealthdv_config()}."
    )
    stop("Cannot find the config file.", call. = FALSE)
  }

  raw <- tryCatch(
    yaml::read_yaml(path),
    error = function(problem) {
      cli::cli_alert_danger(
        "{.file {path}} could not be read: {conditionMessage(problem)}. Check the indentation, and that text is in single quotes."
      )
      stop("The config file could not be read.", call. = FALSE)
    }
  )

  if (!is.list(raw)) {
    raw <- list()
  }

  missing_fields <- setdiff(config_fields(), names(raw))

  if (length(missing_fields) > 0L) {
    cli::cli_alert_danger(
      "{.file {path}} is missing {.field {missing_fields}}. Compare it with a fresh one from {.code create_wealthdv_config()}."
    )
    stop("The config file is missing settings.", call. = FALSE)
  }

  check_round(raw$round, "round")
  round <- as.integer(raw$round)

  text_fields <- setdiff(config_fields(), c("round", "exclude_sheets"))

  config <- list(round = round)

  for (field in text_fields) {
    value <- raw[[field]]

    if (is.null(value)) {
      value <- ""
    }

    if (!is.character(value) || length(value) != 1L) {
      cli::cli_alert_danger("{.field {field}} in {.file {path}} must be one piece of text in single quotes.")
      stop("A config setting has the wrong type.", call. = FALSE)
    }

    config[[field]] <- fill_round(value, round)
  }

  config$exclude_sheets <- fill_round(as.character(unlist(raw$exclude_sheets)), round)

  # Optional, so config files made before it existed still work.
  config$name_case <- if (is.null(raw$name_case)) "auto" else raw$name_case

  if (!is.character(config$name_case) || length(config$name_case) != 1L ||
      !config$name_case %in% name_case_options()) {
    cli::cli_alert_danger("{.field name_case} in {.file {path}} must be one of {.val {name_case_options()}}.")
    stop("The name_case setting is not recognised.", call. = FALSE)
  }

  # Optional too; defaults to a folder inside the output folder.
  config$scripts_folder <- if (is.null(raw$scripts_folder) || !nzchar(raw$scripts_folder)) {
    file.path(config$output_folder, "dv_scripts")
  } else {
    fill_round(as.character(raw$scripts_folder), round)
  }

  config$topics <- optional_names(raw$topics, "topics", path)
  config$sheet_priority <- fill_round(optional_names(raw$sheet_priority, "sheet_priority", path), round)

  pattern_problem <- tryCatch(
    {
      grepl(config$spec_pattern, "")
      NULL
    },
    error = function(problem) conditionMessage(problem)
  )

  if (!is.null(pattern_problem)) {
    cli::cli_alert_danger("{.field spec_pattern} is not a valid pattern: {pattern_problem}")
    stop("The spec pattern is not valid.", call. = FALSE)
  }

  output_folder <- config$output_folder
  config$spec_files <- if (dir.exists(config$spec_folder)) {
    all_files <- list.files(config$spec_folder, pattern = config$spec_pattern, full.names = TRUE)
    lock_files <- all_files[grepl("^~\\$.*", basename(all_files))]

    if (length(lock_files) > 0L) {
      cli::cli_alert_warning(
        "Ignoring Excel lock file{?s} {.file {basename(lock_files)}}. Close all open workbook{?s} before proceeding."
      )
    }

    setdiff(all_files, lock_files)
  } else {
    character()
  }
  config$catalogue_file <- file.path(output_folder, "QA_derivation_catalogue.csv")
  config$plan_file <- file.path(output_folder, "dv_plan.csv")
  config$rebuilt_plan_file <- file.path(output_folder, "dv_plan_rebuilt.csv")
  config$duplicates_file <- file.path(output_folder, "QA_duplicate_dvs.csv")
  config$trial_suite_folder <- file.path(output_folder, "dv_suite_trial")
  config$trial_report_file <- file.path(output_folder, "QA_dv_suite_trial_report.csv")
  config$report_file <- file.path(output_folder, "QA_dv_suite_report.csv")
  config$all_topics <- unique(suite_topic(config$spec_files))

  check_config_topics(config$topics, config$all_topics, path)
  report_config(config)

  invisible(config)
}


#' Stop unless given a config read by read_wealthdv_config()
#' @keywords internal
#' @noRd
check_config_object <- function(config) {
  needed <- c("round", "spec_files", "output_folder", "suite_folder", "plan_file", "all_topics")

  if (!is.list(config) || length(setdiff(needed, names(config))) > 0L) {
    cli::cli_alert_danger("{.arg config} must be the result of {.code read_wealthdv_config()}.")
    stop("`config` is not a wealthdv config.", call. = FALSE)
  }

  invisible(NULL)
}


#' The topics to work on: the chosen ones, or NULL for every topic
#' @keywords internal
#' @noRd
chosen_topics <- function(config, topics = config$topics) {
  if (length(topics) == 0L) NULL else topics
}


#' List the topics
#'
#' A topic is one spec workbook: its name before `_DV_Spec`, in lower case,
#' such as `property_wealth`. Use these names in `topics` in the config, or
#' in the `topics` argument of [trial_run()], [sync_dv_suite()] and
#' [run_dv_suite()].
#'
#' @param config The config from [read_wealthdv_config()].
#'
#' @return A data frame of `topic`, `spec_file` and `chosen`, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' show_topics(read_wealthdv_config())
#' }
show_topics <- function(config) {
  check_config_object(config)

  topics <- data.frame(
    topic = suite_topic(config$spec_files),
    spec_file = basename(config$spec_files),
    stringsAsFactors = FALSE
  )
  topics$chosen <- length(config$topics) == 0L | topics$topic %in% config$topics

  cli::cli_h2("Topics")
  cli::cli_bullets(stats::setNames(
    paste0("{.val ", topics$topic, "} from {.file ", topics$spec_file, "}",
           ifelse(topics$chosen, "", " (not chosen in the config)")),
    ifelse(topics$chosen, "*", " ")
  ))

  invisible(topics)
}


#' An optional list of names in the config: empty when missing or blank
#' @keywords internal
#' @noRd
optional_names <- function(value, field, path) {
  if (is.null(value)) {
    return(character())
  }

  value <- unlist(value)

  if (length(value) == 0L) {
    return(character())
  }

  if (!is.character(value)) {
    cli::cli_alert_danger("{.field {field}} in {.file {path}} must be a list of names in single quotes, one per line starting with a dash.")
    stop("A config setting has the wrong type.", call. = FALSE)
  }

  value[!is_blank(value)]
}


#' Stop when the config names a topic no spec workbook has
#' @keywords internal
#' @noRd
check_config_topics <- function(topics, all_topics, path) {
  unknown <- setdiff(topics, all_topics)

  if (length(unknown) > 0L && length(all_topics) > 0L) {
    cli::cli_alert_danger(
      "{.field topics} in {.file {path}} names {.val {unknown}}, which no spec workbook has. The topics are {.val {all_topics}}."
    )
    stop("The config names an unknown topic.", call. = FALSE)
  }

  invisible(NULL)
}


#' The settings every config file must have
#' @keywords internal
#' @noRd
config_fields <- function() {
  c("round", "spec_folder", "spec_pattern", "exclude_sheets", "data_file",
    "output_folder", "suite_folder", "output_data_file")
}


#' @keywords internal
#' @noRd
check_round <- function(round, name) {
  valid <- is.numeric(round) && length(round) == 1L && !is.na(round) &&
    round == floor(round) && round > 0

  if (!valid) {
    cli::cli_alert_danger("{.field {name}} must be a whole number such as {.val 10}.")
    stop("The round must be a whole number.", call. = FALSE)
  }

  invisible(NULL)
}


#' Put the round number wherever {round} appears
#' @keywords internal
#' @noRd
fill_round <- function(text, round) {
  gsub("{round}", as.character(round), text, fixed = TRUE)
}


#' Say what the config points at
#' @keywords internal
#' @noRd
report_config <- function(config) {
  cli::cli_h1("wealthdv config for Round {config$round}")

  if (!dir.exists(config$spec_folder)) {
    cli::cli_alert_warning("Spec folder {.file {config$spec_folder}} does not exist.")
  } else if (length(config$spec_files) == 0L) {
    cli::cli_alert_warning(
      "No workbooks in {.file {config$spec_folder}} match {.val {config$spec_pattern}}."
    )
  } else {
    cli::cli_alert_success(
      "{length(config$spec_files)} spec workbook{?s} in {.file {config$spec_folder}}: {.file {basename(config$spec_files)}}"
    )
  }

  cli::cli_alert_info("{length(config$exclude_sheets)} sheet name{?s} will be skipped.")

  if (length(config$topics) == 0L) {
    cli::cli_alert_info("Working on every topic{if (length(config$all_topics) > 0) paste0(': ', paste(config$all_topics, collapse = ', ')) else ''}.")
  } else {
    cli::cli_alert_info("Working on {length(config$topics)} topic{?s} ({.field topics}): {.val {config$topics}}.")
  }

  if (length(config$sheet_priority) > 0L) {
    cli::cli_alert_info("A DV on more than one sheet is built from the first of: {.val {config$sheet_priority}}.")
  }

  name_style <- c(
    auto = "in the data's naming style",
    lower = "in lower case",
    upper = "in upper case",
    spec = "as the spec spells them"
  )[[config$name_case]]
  cli::cli_alert_info("New DVs will be named {name_style} ({.field name_case}: {config$name_case}).")

  if (!nzchar(config$data_file)) {
    cli::cli_alert_warning("{.field data_file} is not set. Set it before writing the suite or building DVs.")
  } else if (!file.exists(config$data_file)) {
    cli::cli_alert_warning("Cannot find the data file {.file {config$data_file}}.")
  } else {
    cli::cli_alert_success("Data file found: {.file {config$data_file}}")
  }

  if (!dir.exists(config$output_folder)) {
    dir.create(config$output_folder, recursive = TRUE, showWarnings = FALSE)
    cli::cli_alert_success("Created output folder {.file {config$output_folder}}.")
  } else {
    cli::cli_alert_success("Outputs go to {.file {config$output_folder}}.")
  }

  topic_count <- length(setdiff(
    list.files(config$suite_folder, pattern = "[.]R$"),
    "settings.R"
  ))

  if (topic_count == 0L) {
    cli::cli_alert_info("No DV suite in {.file {config$suite_folder}} yet; {.code write_dv_suite()} creates it.")
  } else {
    cli::cli_alert_success("DV suite in {.file {config$suite_folder}}: {topic_count} topic file{?s}.")
  }

  if (nzchar(config$output_data_file)) {
    cli::cli_alert_info("Data with DVs will be saved to {.file {config$output_data_file}}.")
  }

  cli::cli_alert_info("DV scripts will be exported to {.file {config$scripts_folder}}.")

  invisible(NULL)
}


#' Lines of a new config file
#' @keywords internal
#' @noRd
config_template_lines <- function(round = NULL) {
  quote_yaml <- function(text) paste0("'", gsub("'", "''", text, fixed = TRUE), "'")

  c(
    "# wealthdv settings for one survey round.",
    "#",
    "# Edit the values after the colons. Keep text in single quotes and use forward",
    "# slashes (/) in paths. Wherever you write {round}, the round number is filled in.",
    "",
    "# The survey round, as a number",
    paste0("round: ", if (is.null(round)) "" else round),
    "",
    "# Folder holding the DV specification workbooks, and the pattern their file names follow",
    "spec_folder: 'specs'",
    "spec_pattern: '_DV_Spec_R{round}.*[.]xlsx$'",
    "",
    "# Sheets in the spec workbooks that hold no derivations, matched exactly (spaces count)",
    "exclude_sheets:",
    paste0("  - ", quote_yaml(exclude_sheet_templates())),
    "",
    "# The imputed person-level SPSS file the DVs are added to",
    "data_file: ''",
    "",
    "# How new DVs are spelt. Variable names are matched ignoring case, and inputs and DVs",
    "# already in the data always keep the data's spelling. 'auto' names new DVs in the",
    "# data's style: lower case, upper case, or as the spec spells them if the data mixes",
    "# cases. Or choose 'lower', 'upper' or 'spec'.",
    "name_case: 'auto'",
    "",
    "# Where the plan, QA reports and trial runs are saved; created if missing",
    "output_folder: 'outputs/R{round}'",
    "",
    "# Where this round's DV suite code lives",
    "suite_folder: 'dv_suite/R{round}'",
    "",
    "# Where the data with DVs added is saved as an SPSS file; leave blank to not save",
    "output_data_file: ''",
    "",
    "# Where the DVs are exported as plain R scripts for the team that applies them,",
    "# usually a checkout of the scripts repo. Leave blank for dv_scripts inside the output folder.",
    "scripts_folder: ''",
    config_optional_lines()
  )
}


#' Lines for the optional settings, also added to older config files by hand
#' @keywords internal
#' @noRd
config_optional_lines <- function() {
  c(
    "",
    "# Topics to work on: trial runs, writing the suite and running it. Leave as [] for every",
    "# topic. A topic is a spec workbook's name before _DV_Spec, in lower case, such as",
    "# 'property_wealth'. show_topics() lists them. For example:",
    "# topics:",
    "#   - 'property_wealth'",
    "#   - 'physical_wealth'",
    "topics: []",
    "",
    "# When a DV is defined on more than one sheet, build it from the row on the sheet highest",
    "# in this list and mark the others duplicate, whatever their text. Leave as [] to decide",
    "# each one in the plan. QA_duplicate_dvs.csv lists the DVs still to decide. For example:",
    "# sheet_priority:",
    "#   - 'R9_Pension_Income'",
    "#   - 'R9_Other_Invest_Pension_Ben '",
    "sheet_priority: []"
  )
}
