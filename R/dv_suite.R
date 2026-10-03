#' Define one step of the DV suite
#'
#' Used in the topic files [write_dv_suite()] creates, as
#' `steps$DVName <- dv_step(label = ..., inputs = ..., derive = ...)`.
#'
#' @param label The DV's variable label, attached to the column once built.
#' @param inputs Every column the step reads. [run_dv_suite()] uses these to
#'   run steps in the right order, so keep them up to date when editing.
#' @param derive A function taking `df` and returning it with the DV added, or
#'   `NULL` while the step is not written yet.
#' @param missing_code Optional code, such as `-9`, for DVs that must hold it
#'   where they are missing. While the DV is worked out, -8 and -9 in its
#'   `inputs` count as missing (`NA`); the inputs are then put back as they
#'   were, and every `NA` in the DV is written as this code.
#' @param uid Optional audit ID for the plan row that created the step. The
#'   suite report carries it so you can trace every built DV back to a row in
#'   the derivation plan.
#'
#' @return A `dv_step` object.
#' @export
#'
#' @examples
#' dv_step(
#'   label = "Value of cash ISAs",
#'   inputs = "FCISAvR9_i",
#'   derive = function(df) dv_copy(df, source_col = "FCISAvR9_i", new_col = "DVCISAvR9")
#' )
dv_step <- function(label, inputs, derive, missing_code = NULL, uid = NULL) {
  structure(list(label = label, inputs = inputs, derive = derive, missing_code = missing_code, uid = uid), class = "dv_step")
}


#' Run a step's derive, applying its missing_code if it has one
#' @keywords internal
#' @noRd
derive_step <- function(df, step) {
  if (is.null(step$missing_code)) {
    return(step$derive(df))
  }

  inputs <- intersect(setdiff(step$inputs, step$dv), names(df))
  inputs_as_given <- lapply(inputs, function(column) df[[column]])

  for (column in inputs) {
    df[[column]][df[[column]] %in% sentinel_codes()] <- NA
  }

  result <- step$derive(df)

  if (!is.data.frame(result) || nrow(result) != nrow(df) || !step$dv %in% names(result)) {
    return(result)
  }

  for (index in seq_along(inputs)) {
    result[[inputs[[index]]]] <- inputs_as_given[[index]]
  }

  result[[step$dv]][is.na(result[[step$dv]])] <- step$missing_code

  result
}


#' Read the DV suite
#'
#' Sources `settings.R` and every topic file in the suite folder.
#'
#' @param suite_dir The suite folder written by [write_dv_suite()].
#' @param topics Optional topic names (file names without `.R`) to read.
#'
#' @return A `dv_suite`: a list with `steps` (named by DV), `settings` and
#'   `dir`.
#' @export
#'
#' @examples
#' \dontrun{
#' suite <- read_dv_suite("dv_suite")
#' names(suite$steps)
#' }
read_dv_suite <- function(suite_dir = "dv_suite", topics = NULL) {
  if (!is.character(suite_dir) || length(suite_dir) != 1L || !dir.exists(suite_dir)) {
    cli::cli_alert_danger(
      "Cannot find the DV suite folder {.file {suite_dir}}. Create it with write_dv_suite()."
    )
    stop("Cannot find the DV suite folder.", call. = FALSE)
  }

  settings_file <- file.path(suite_dir, "settings.R")

  if (!file.exists(settings_file)) {
    cli::cli_alert_danger("{.file {settings_file}} is missing. Rewrite it with write_dv_suite().")
    stop("The suite has no settings.R.", call. = FALSE)
  }

  topic_files <- list.files(suite_dir, pattern = "\\.R$", full.names = TRUE)
  topic_files <- topic_files[basename(topic_files) != "settings.R"]
  available <- sub("\\.R$", "", basename(topic_files))

  if (!is.null(topics)) {
    unknown <- setdiff(topics, available)

    if (length(unknown) > 0L) {
      cli::cli_alert_danger("No topic file for {.val {unknown}}. Topics in the suite: {.val {available}}.")
      stop("Unknown topic requested.", call. = FALSE)
    }

    topic_files <- topic_files[available %in% topics]
  }

  settings_env <- new.env(parent = asNamespace("wealthdv"))
  source_suite_file(settings_file, settings_env)

  settings <- get0("settings", envir = settings_env, inherits = FALSE)

  if (!is.list(settings)) {
    cli::cli_alert_danger("{.file {settings_file}} must create a list called {.code settings}.")
    stop("settings.R does not define `settings`.", call. = FALSE)
  }

  cli::cli_alert_success("Read settings from {.file {settings_file}}.")

  steps <- list()

  for (path in topic_files) {
    topic <- sub("\\.R$", "", basename(path))
    topic_env <- new.env(parent = settings_env)
    source_suite_file(path, topic_env)

    file_steps <- get0("steps", envir = topic_env, inherits = FALSE)

    if (!is.list(file_steps) || (length(file_steps) > 0L && is.null(names(file_steps)))) {
      cli::cli_alert_danger("{.file {path}} must build a named list called {.code steps}.")
      stop("A topic file does not define `steps`.", call. = FALSE)
    }

    for (dv in names(file_steps)) {
      step <- check_dv_step(file_steps[[dv]], dv, path)
      existing <- match(tolower(dv), tolower(names(steps)))

      if (!is.na(existing)) {
        other <- steps[[existing]]
        cli::cli_alert_danger(
          "{other$dv} in {.file {basename(other$file)}} and {dv} in {.file {basename(path)}} build the same variable. Keep one."
        )
        stop("A DV is built by more than one step.", call. = FALSE)
      }

      step$dv <- dv
      step$topic <- topic
      step$file <- path
      steps[[dv]] <- step
    }

    written <- sum(vapply(file_steps, function(step) !is.null(step$derive), logical(1L)))

    cli::cli_alert_success(
      "Read {length(file_steps)} step{?s} from {.file {basename(path)}}: {written} written, {length(file_steps) - written} not written yet."
    )
  }

  structure(list(steps = steps, settings = settings, dir = suite_dir), class = "dv_suite")
}


#' Build DVs by running the suite
#'
#' Runs every step, or only the chosen topics and DVs, in dependency order.
#' Prints what each step did. A step that fails, is not written yet, or is
#' missing an input is reported and skipped, and the run carries on. Each DV
#' built gets its label attached.
#'
#' @param df The data to add DVs to.
#' @param suite The suite folder, or a suite from [read_dv_suite()].
#' @param topics Optional topic names to run, such as `"property_wealth"`;
#'   every topic when `NULL` or empty.
#' @param dvs Optional DV names to run.
#' @param with_inputs When running chosen topics or DVs, also run the steps
#'   that build any input not already in `df`.
#' @param stop_on_error Whether to stop at the first failing step.
#' @param report_file Optional CSV path for the report of every step.
#'
#' @return `df` with the DVs added. The report is attached as the attribute
#'   `"dv_suite_report"`.
#' @export
#'
#' @examples
#' \dontrun{
#' was_data <- run_dv_suite(was_data, "dv_suite")
#' was_data <- run_dv_suite(was_data, "dv_suite", topics = "property_wealth")
#' was_data <- run_dv_suite(was_data, "dv_suite", dvs = c("DVHValueR9", "HFINLR9_SUM"))
#' }
run_dv_suite <- function(df, suite = "dv_suite", topics = NULL, dvs = NULL,
                         with_inputs = TRUE, stop_on_error = FALSE,
                         report_file = NULL) {
  if (!is.data.frame(df)) {
    cli::cli_alert_danger("{.arg df} must be a data frame.")
    stop("`df` must be a data frame.", call. = FALSE)
  }

  cli::cli_h1("Building derived variables")

  suite <- as_dv_suite(suite)
  steps <- select_suite_steps(suite$steps, topics, dvs, with_inputs, names(df))
  steps <- steps[order_suite_steps(steps)]
  renaming <- case_renaming(steps, names(df), settings_column_names(suite$settings))
  df <- rename_columns(df, renaming$data, renaming$suite)

  cli::cli_alert_info("{length(steps)} DV{?s} to build, in dependency order.")

  already_there <- intersect(names(steps), names(df))

  if (length(already_there) > 0L) {
    cli::cli_alert_warning(
      "{length(already_there)} DV{?s} already {?exists/exist} in the data and will be replaced: {.val {utils::head(already_there, 10)}}{if (length(already_there) > 10) ', ...' else ''}"
    )
  }

  old_options <- options(wealthdv.in_suite = TRUE)
  on.exit(options(old_options), add = TRUE)

  registered <- registered_verbs()
  outcomes <- character(length(steps))
  details <- character(length(steps))
  current_topic <- ""

  for (index in seq_along(steps)) {
    step <- steps[[index]]

    if (!identical(step$topic, current_topic)) {
      cli::cli_h2(step$topic)
      current_topic <- step$topic
    }

    unbuilt_dvs <- setdiff(names(suite$steps), names(steps)[outcomes == "built"])
    result <- run_suite_step(df, step, unbuilt_dvs, registered)
    outcomes[[index]] <- result$outcome
    details[[index]] <- result$detail

    if (result$outcome == "built") {
      df <- result$df
    } else if (result$outcome == "failed" && stop_on_error) {
      stop("Building ", step$dv, " failed: ", result$detail, call. = FALSE)
    }
  }

  report <- data.frame(
    topic = as.character(vapply(steps, `[[`, character(1L), "topic")),
    dv = as.character(names(steps)),
    uid = vapply(steps, function(step) {
      value <- if (is.null(step$uid)) NA_character_ else step$uid
      if (length(value) == 0L || is.na(value)) NA_character_ else as.character(value)
    }, character(1L)),
    outcome = outcomes,
    detail = details,
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  report_suite_run(report, report_file)
  df <- rename_columns(df, renaming$suite, renaming$data)

  if (data.table::is.data.table(df)) {
    df <- data.table::setalloccol(df)
    data.table::setattr(df, "dv_suite_report", report)
  } else {
    attr(df, "dv_suite_report") <- report
  }

  df
}


#' Run a single step of the suite
#'
#' Stops on any problem, so it suits example tests and trying out one step.
#'
#' @param df The data.
#' @param suite The suite folder, or a suite from [read_dv_suite()].
#' @param dv The DV to build.
#'
#' @return `df` with the DV added and labelled.
#' @export
#'
#' @examples
#' \dontrun{
#' suite <- read_dv_suite("dv_suite")
#' run_dv_step(was_data, suite, "DVHValueR9")
#' }
run_dv_step <- function(df, suite, dv) {
  if (!is.data.frame(df)) {
    cli::cli_alert_danger("{.arg df} must be a data frame.")
    stop("`df` must be a data frame.", call. = FALSE)
  }

  suite <- as_dv_suite(suite)
  position <- match(tolower(dv), tolower(names(suite$steps)))
  step <- if (length(dv) == 1L && !is.na(position)) suite$steps[[position]] else NULL

  if (is.null(step)) {
    stop_unknown_dvs(dv, names(suite$steps))
  }

  dv <- step$dv

  if (is.null(step$derive)) {
    cli::cli_alert_danger("{dv} is not written yet; see {.file {step$file}}.")
    stop(dv, " is not written yet.", call. = FALSE)
  }

  chosen_step <- list(step)
  names(chosen_step) <- dv
  renaming <- case_renaming(chosen_step, names(df), settings_column_names(suite$settings))
  df <- rename_columns(df, renaming$data, renaming$suite)

  missing_inputs <- setdiff(step$inputs, names(df))

  if (length(missing_inputs) > 0L) {
    cli::cli_alert_danger("{dv} needs {.val {missing_inputs}}, which {?is/are} not in the data.")
    stop(dv, " is missing inputs.", call. = FALSE)
  }

  result <- derive_step(df, step)
  check_step_result(result, df, dv)

  rename_columns(set_dv_label(result, dv, step$label), renaming$suite, renaming$data)
}


#' Attach a variable label to a column
#'
#' Stored as the `"label"` attribute, which `haven::write_sav()` writes as the
#' SPSS variable label.
#'
#' @param df A data frame.
#' @param dv The column to label.
#' @param label The label. `NA` leaves the column unlabelled.
#'
#' @return `df` with the label attached.
#' @export
#'
#' @examples
#' df <- set_dv_label(data.frame(x = 1), "x", "An example")
#' attr(df$x, "label")
set_dv_label <- function(df, dv, label) {
  if (!is.data.frame(df) || !is.character(dv) || length(dv) != 1L || !dv %in% names(df)) {
    cli::cli_alert_danger("{.arg dv} must name one column of {.arg df}.")
    stop("`dv` must name one column of `df`.", call. = FALSE)
  }

  if (length(label) != 1L || is.na(label)) {
    return(df)
  }

  labelled <- df[[dv]]
  attr(labelled, "label") <- as.character(label)
  df[[dv]] <- labelled

  df
}


#' Run one step and describe what happened
#' @keywords internal
#' @noRd
run_suite_step <- function(df, step, unbuilt_dvs, registered) {
  if (is.null(step$derive)) {
    if (step$dv %in% names(df)) {
      cli::cli_alert_info("{step$dv}: not written yet, but already in the data, so left as it is.")
      return(list(outcome = "not written", detail = "derive is NULL; already in the data, left as it is"))
    }

    cli::cli_alert_warning("{step$dv}: not written yet, skipped.")
    return(list(outcome = "not written", detail = "derive is NULL"))
  }

  missing_inputs <- setdiff(step$inputs, names(df))

  if (length(missing_inputs) > 0L) {
    detail <- describe_missing_inputs(missing_inputs, unbuilt_dvs, names(df))
    cli::cli_alert_warning("{step$dv}: skipped, {detail}")
    return(list(outcome = "skipped", detail = detail))
  }

  step_warnings <- character()

  result <- tryCatch(
    withCallingHandlers(
      derive_step(df, step),
      warning = function(problem) {
        step_warnings <<- c(step_warnings, conditionMessage(problem))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(problem) problem
  )

  if (!inherits(result, "error")) {
    result <- tryCatch(
      {
        check_step_result(result, df, step$dv)
        result
      },
      error = function(problem) problem
    )
  }

  if (inherits(result, "error")) {
    detail <- conditionMessage(result)
    cli::cli_alert_danger("{step$dv}: failed. {detail}")
    return(list(outcome = "failed", detail = detail))
  }

  for (warning_text in step_warnings) {
    cli::cli_alert_warning("{step$dv}: {warning_text}")
  }

  if (length(intersect(code_function_names(step$derive), registered)) == 0L) {
    cli::cli_alert_success("{step$dv}: built by hand-written code.")
  }

  if (!is.null(step$missing_code)) {
    coded <- sum(result[[step$dv]] %in% step$missing_code)
    step_warnings <- c(step_warnings, paste0(coded, " missing written as ", step$missing_code))
    cli::cli_alert_info("{step$dv}: -8 and -9 inputs counted as missing; {coded} missing value{?s} written as {step$missing_code}.")
  }

  list(
    outcome = "built",
    detail = paste(step_warnings, collapse = "; "),
    df = set_dv_label(result, step$dv, step$label)
  )
}


#' @keywords internal
#' @noRd
check_step_result <- function(result, df, dv) {
  if (!is.data.frame(result)) {
    stop("derive must return the data frame, but returned ", class(result)[[1L]], call. = FALSE)
  }

  if (nrow(result) != nrow(df)) {
    stop("derive returned ", nrow(result), " rows but was given ", nrow(df), call. = FALSE)
  }

  clashes <- case_clashes(setdiff(names(result), names(df)), names(df))

  if (nrow(clashes) > 0L) {
    stop(
      "derive created ", paste0(clashes$name, " but the data already has ", clashes$known, collapse = "; "),
      ". Use the data's spelling.",
      call. = FALSE
    )
  }

  if (!dv %in% names(result)) {
    variant <- case_clashes(names(result), dv)$name
    stop(
      "derive did not create a column called ", dv,
      if (length(variant) > 0L) paste0(" (it created ", variant[[1L]], "; the spelling must match the step name)"),
      call. = FALSE
    )
  }

  invisible(NULL)
}


#' @keywords internal
#' @noRd
describe_missing_inputs <- function(missing_inputs, unbuilt_dvs, data_names) {
  from_steps <- intersect(missing_inputs, unbuilt_dvs)
  from_data <- setdiff(missing_inputs, from_steps)
  parts <- character()

  if (length(from_steps) > 0L) {
    parts <- c(parts, paste0(
      "needs ", paste(from_steps, collapse = ", "), ", which ",
      if (length(from_steps) == 1L) "was" else "were", " not built"
    ))
  }

  if (length(from_data) > 0L) {
    resolution <- resolve_variable_names(from_data, data_names)
    hints <- ifelse(
      resolution$how == "case",
      paste0(" (the data has ", resolution$resolved, ")"),
      ifelse(is.na(resolution$suggestion), "", paste0(" (did you mean ", resolution$suggestion, "?)"))
    )

    parts <- c(parts, paste0(
      "needs ", paste0(from_data, hints, collapse = ", "), ", not in the data"
    ))
  }

  paste(parts, collapse = "; ")
}


#' Column names held in the settings, such as the household identifier
#' @keywords internal
#' @noRd
settings_column_names <- function(settings) {
  by <- settings$by
  if (is.character(by) && length(by) == 1L && !is.na(by)) by else character()
}


#' @keywords internal
#' @noRd
as_dv_suite <- function(suite) {
  if (inherits(suite, "dv_suite")) {
    return(suite)
  }

  if (is.character(suite) && length(suite) == 1L) {
    return(read_dv_suite(suite))
  }

  cli::cli_alert_danger("{.arg suite} must be the suite folder or the result of read_dv_suite().")
  stop("`suite` must be a folder path or a dv_suite.", call. = FALSE)
}


#' @keywords internal
#' @noRd
source_suite_file <- function(path, envir) {
  tryCatch(
    sys.source(path, envir = envir, keep.source = TRUE),
    error = function(problem) {
      cli::cli_alert_danger("{.file {path}} could not be read: {conditionMessage(problem)}")
      stop("A suite file could not be read.", call. = FALSE)
    }
  )

  invisible(NULL)
}


#' Check one step read from a topic file
#' @keywords internal
#' @noRd
check_dv_step <- function(step, dv, path) {
  where <- paste0("steps$", dv, " in ", basename(path))

  fail <- function(problem) {
    cli::cli_alert_danger("{where}: {problem}")
    stop(where, ": ", problem, call. = FALSE)
  }

  if (!inherits(step, "dv_step")) {
    fail("must be made with dv_step()")
  }

  if (length(step$label) != 1L || !(is.na(step$label) || is.character(step$label))) {
    fail("label must be one piece of text in quotes, or NA")
  }

  if (!is.character(step$inputs) || anyNA(step$inputs)) {
    fail("inputs must be column names in quotes, such as c(\"a\", \"b\")")
  }

  if (!is.null(step$derive) && (!is.function(step$derive) || length(formals(step$derive)) == 0L)) {
    fail("derive must be NULL or function(df) { ... }")
  }

  if (!is.null(step$missing_code) &&
      (!is.numeric(step$missing_code) || length(step$missing_code) != 1L || is.na(step$missing_code))) {
    fail("missing_code must be one number, such as -9, or left out")
  }

  step$label <- if (is.na(step$label)) NA_character_ else step$label
  step
}


#' Choose steps by topic and DV, adding the steps that build missing inputs
#' @keywords internal
#' @noRd
select_suite_steps <- function(steps, topics, dvs, with_inputs, data_names) {
  # An empty choice, as from a config with no topics listed, means every step.
  if (length(topics) == 0L) topics <- NULL
  if (length(dvs) == 0L) dvs <- NULL

  if (is.null(topics) && is.null(dvs)) {
    return(steps)
  }

  step_topics <- vapply(steps, `[[`, character(1L), "topic")

  unknown_topics <- setdiff(topics, step_topics)

  if (length(unknown_topics) > 0L) {
    cli::cli_alert_danger(
      "No steps for the topic {.val {unknown_topics}}. Topics in the suite: {.val {unique(step_topics)}}."
    )
    stop("Unknown topic requested.", call. = FALSE)
  }

  if (!is.null(dvs)) {
    position <- match(tolower(dvs), tolower(names(steps)))

    if (anyNA(position)) {
      stop_unknown_dvs(dvs[is.na(position)], names(steps))
    }

    dvs <- names(steps)[position]
  }

  chosen <- union(names(steps)[step_topics %in% topics], dvs)
  cli::cli_alert_info("Running {length(chosen)} chosen DV{?s}.")

  if (with_inputs) {
    repeat {
      needed <- unlist(lapply(steps[chosen], `[[`, "inputs"), use.names = FALSE)
      not_in_data <- needed[!tolower(needed) %in% tolower(data_names)]
      upstream <- setdiff(intersect(not_in_data, names(steps)), chosen)

      if (length(upstream) == 0L) {
        break
      }

      chosen <- c(chosen, upstream)
    }

    extra <- setdiff(chosen, union(names(steps)[step_topics %in% topics], dvs))

    if (length(extra) > 0L) {
      cli::cli_alert_info(
        "Also running {length(extra)} step{?s} that build{?s/} inputs not yet in the data: {.val {extra}}."
      )
    }
  }

  steps[names(steps) %in% chosen]
}


#' @keywords internal
#' @noRd
stop_unknown_dvs <- function(unknown, step_names) {
  resolution <- resolve_variable_names(unknown, step_names)
  hints <- ifelse(
    resolution$how == "case",
    paste0(" (the suite has ", resolution$resolved, ")"),
    ifelse(is.na(resolution$suggestion), "", paste0(" (did you mean ", resolution$suggestion, "?)"))
  )

  cli::cli_alert_danger("No step builds {paste0(unknown, hints, collapse = ', ')}.")
  stop("Unknown DV requested.", call. = FALSE)
}


#' Order steps so each runs after the steps that build its inputs
#'
#' Keeps file order wherever the inputs allow.
#' @keywords internal
#' @noRd
order_suite_steps <- function(steps) {
  if (length(steps) == 0L) {
    return(integer())
  }

  step_names <- names(steps)

  dependencies <- lapply(steps, function(step) {
    setdiff(which(step_names %in% step$inputs), match(step$dv, step_names))
  })

  waiting_on <- lengths(dependencies)
  dependents <- lapply(seq_along(steps), function(index) {
    which(vapply(dependencies, function(needed) index %in% needed, logical(1L)))
  })

  done <- rep(FALSE, length(steps))
  run_order <- integer()

  while (length(run_order) < length(steps)) {
    ready <- which(!done & waiting_on == 0L)

    if (length(ready) == 0L) {
      stuck <- step_names[!done]
      cli::cli_alert_danger(
        "These steps depend on each other in a circle, so none can run first: {.val {stuck}}. Check their inputs."
      )
      stop("The suite has a circular dependency.", call. = FALSE)
    }

    next_step <- ready[[1L]]
    run_order <- c(run_order, next_step)
    done[[next_step]] <- TRUE
    waiting_on[dependents[[next_step]]] <- waiting_on[dependents[[next_step]]] - 1L
  }

  run_order
}


#' @keywords internal
#' @noRd
report_suite_run <- function(report, report_file) {
  cli::cli_h2("Summary")

  count_of <- function(outcome) sum(report$outcome == outcome)
  names_of <- function(outcome) {
    found <- report$dv[report$outcome == outcome]
    if (length(found) > 10L) c(utils::head(found, 10L), paste("and", length(found) - 10L, "more")) else found
  }

  cli::cli_alert_success("{count_of('built')} DV{?s} built and labelled.")

  if (count_of("not written") > 0L) {
    kept <- sum(report$outcome == "not written" & grepl("already in the data", report$detail, fixed = TRUE))
    cli::cli_alert_warning(
      "{count_of('not written')} not written yet{if (kept > 0) paste0(' (', kept, ' already in the data, left as they are)') else ''}: {names_of('not written')}."
    )
  }

  if (count_of("skipped") > 0L) {
    cli::cli_alert_warning("{count_of('skipped')} skipped for missing inputs: {names_of('skipped')}.")
  }

  if (count_of("failed") > 0L) {
    cli::cli_alert_danger("{count_of('failed')} failed: {names_of('failed')}.")
  }

  if (!is.null(report_file)) {
    data.table::fwrite(report, report_file)
    cli::cli_alert_info("Report of every step written to {.file {report_file}}.")
  } else {
    cli::cli_alert_info("The report of every step is in {.code attr(df, \"dv_suite_report\")}.")
  }

  invisible(NULL)
}
