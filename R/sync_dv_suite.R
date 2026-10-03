#' Bring the DV suite up to date with the plan
#'
#' Run after signing off more rows in the plan. Every step still at
#' `derive = NULL` whose row is now signed off is rewritten as a working call
#' to its verb, and DVs new to the plan get a step at the end of their topic
#' file. Steps with a `derive` function, written by hand or by an earlier
#' sync, are never touched. Creates the suite with [write_dv_suite()] if there
#' is none yet.
#'
#' A copy of each file it changes is saved first in a `_backup` folder inside
#' the suite. Steps in the suite that the plan no longer builds are listed,
#' not deleted.
#'
#' @param plan A plan from [read_dv_plan()].
#' @param suite_dir The suite folder.
#' @param data_names Column names of the input data, so names are spelt as
#'   the data spells them.
#' @param name_case How new DVs are spelt; see [write_dv_suite()].
#' @param topics Optional topic names to sync; every topic when `NULL`.
#'
#' @return A data frame with one row per step looked at: `topic`, `dv`, and
#'   `action` (`"filled in"`, `"added"`, `"written by hand"`, `"still to write"`
#'   or `"not in the plan"`), invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' sync_dv_suite(read_dv_plan("outputs/R9/dv_plan.csv"), "dv_suite/R9", names(was_data))
#' }
sync_dv_suite <- function(plan, suite_dir = "dv_suite", data_names = NULL,
                          name_case = "auto", topics = NULL) {
  check_suite_dir(suite_dir)

  if (!file.exists(file.path(suite_dir, "settings.R"))) {
    cli::cli_alert_info("No suite in {.file {suite_dir}} yet, so it is written from scratch.")
    write_dv_suite(plan, suite_dir, data_names = data_names, name_case = name_case, topics = topics)
    return(invisible(data.frame(topic = character(), dv = character(), action = character())))
  }

  cli::cli_h1("Syncing the DV suite in {.file {suite_dir}} with the plan")

  prepared <- prepare_suite_plan(plan, data_names, name_case, topics, strict_duplicates = TRUE)
  existing <- suite_step_index(suite_dir)
  backup_dir <- file.path(suite_dir, "_backup", format(Sys.time(), "%Y-%m-%d_%H%M%S"))
  actions <- list()
  code_mismatches <- character()

  for (topic in unique(prepared$plan$topic)) {
    steps <- topic_steps(prepared$plan[prepared$plan$topic == topic, , drop = FALSE],
                         prepared, reviewed_only = TRUE, data_names = data_names)
    position <- match(tolower(vapply(steps, `[[`, character(1L), "dv")), tolower(existing$dv))

    to_fill <- which(!is.na(position) & !existing$written[position] &
                       vapply(steps, `[[`, logical(1L), "written"))
    to_add <- which(is.na(position))

    replace_suite_steps(existing[position[to_fill], , drop = FALSE],
                        lapply(steps[to_fill], `[[`, "lines"), backup_dir)

    if (length(to_add) > 0L) {
      add_suite_steps(suite_dir, topic, prepared$plan[prepared$plan$topic == topic, , drop = FALSE],
                      steps[to_add], backup_dir)
    }

    action <- ifelse(
      is.na(position), "added",
      ifelse(seq_along(steps) %in% to_fill, "filled in",
             ifelse(existing$written[position], "written by hand", "still to write"))
    )
    actions[[topic]] <- data.frame(
      topic = topic,
      dv = vapply(steps, `[[`, character(1L), "dv"),
      action = action,
      stringsAsFactors = FALSE
    )

    report_topic_sync(topic, action)

    kept <- which(!is.na(position) & !(seq_along(steps) %in% to_fill) & existing$written[position] %in% TRUE)
    plan_codes <- vapply(steps[kept], `[[`, numeric(1L), "missing_code")
    suite_codes <- existing$missing_code[position[kept]]
    differs <- !(is.na(plan_codes) & is.na(suite_codes)) &
      (is.na(plan_codes) | is.na(suite_codes) | plan_codes != suite_codes)
    code_mismatches <- c(code_mismatches, vapply(steps[kept][differs], `[[`, character(1L), "dv"))

    existing <- suite_step_index(suite_dir)
  }

  actions <- do.call(rbind, c(list(data.frame(topic = character(), dv = character(), action = character())), actions))
  actions <- rbind(actions, steps_not_in_plan(existing, actions, unique(prepared$plan$topic)))

  report_sync(actions, backup_dir, suite_dir)

  if (length(code_mismatches) > 0L) {
    cli::cli_alert_warning(
      "{length(code_mismatches)} step{?s} already written {?has/have} a different {.field missing_code} from the plan, and sync does not change written steps: {.val {utils::head(code_mismatches, 10)}}{if (length(code_mismatches) > 10) ', ...' else ''}. {cli::qty(length(code_mismatches))}Add or remove {.code missing_code = -9} in {?its/their} {.code dv_step()} by hand."
    )
  }

  invisible(actions)
}


#' Every step defined in a suite's topic files, found by parsing, not running, them
#'
#' One row per `steps$name <- dv_step(...)`: the file, its topic, the lines the
#' step and the comment block above it take up, and whether `derive` is set.
#' @keywords internal
#' @noRd
suite_step_index <- function(suite_dir) {
  paths <- list.files(suite_dir, pattern = "[.]R$", full.names = TRUE)
  paths <- paths[basename(paths) != "settings.R"]

  rows <- lapply(paths, function(path) {
    lines <- readLines(path, encoding = "UTF-8", warn = FALSE)
    expressions <- tryCatch(
      parse(text = lines, keep.source = TRUE),
      error = function(problem) {
        cli::cli_alert_danger("{.file {path}} could not be read: {conditionMessage(problem)}")
        stop("A suite file has a syntax error; fix it before syncing.", call. = FALSE)
      }
    )
    sources <- attr(expressions, "srcref")

    found <- lapply(seq_along(expressions), function(index) {
      dv <- assigned_step_name(expressions[[index]])

      if (is.null(dv)) {
        return(NULL)
      }

      first_line <- sources[[index]][[1L]]

      while (first_line > 1L && grepl("^\\s*#", lines[[first_line - 1L]])) {
        first_line <- first_line - 1L
      }

      data.frame(
        dv = dv,
        file = path,
        topic = sub("[.]R$", "", basename(path)),
        first_line = first_line,
        last_line = sources[[index]][[3L]],
        written = step_has_derive(expressions[[index]][[3L]]),
        missing_code = step_missing_code(expressions[[index]][[3L]]),
        stringsAsFactors = FALSE
      )
    })

    do.call(rbind, found)
  })

  index <- do.call(rbind, rows)

  if (is.null(index)) {
    index <- data.frame(dv = character(), file = character(), topic = character(),
                        first_line = integer(), last_line = integer(), written = logical(),
                        missing_code = numeric(), stringsAsFactors = FALSE)
  }

  index
}


#' The DV name in `steps$name <- ...` or `steps[["name"]] <- ...`, else NULL
#' @keywords internal
#' @noRd
assigned_step_name <- function(expression) {
  if (!is.call(expression) || !identical(expression[[1L]], as.name("<-")) ||
      !is.call(expression[[2L]])) {
    return(NULL)
  }

  target <- expression[[2L]]

  if (!identical(target[[2L]], as.name("steps")) || length(target) != 3L) {
    return(NULL)
  }

  if (identical(target[[1L]], as.name("$")) && (is.symbol(target[[3L]]) || is.character(target[[3L]]))) {
    return(as.character(target[[3L]]))
  }

  if (identical(target[[1L]], as.name("[[")) && is.character(target[[3L]])) {
    return(target[[3L]])
  }

  NULL
}


#' Whether a dv_step() call gives derive as anything but NULL
#' @keywords internal
#' @noRd
step_has_derive <- function(step_call) {
  if (!is.call(step_call)) {
    return(TRUE)
  }

  matched <- tryCatch(match.call(dv_step, step_call), error = function(problem) NULL)

  if (is.null(matched)) {
    return(TRUE)
  }

  "derive" %in% names(matched) && !is.null(matched[["derive"]])
}


#' The missing_code a dv_step() call gives, NA when none or not a plain number
#' @keywords internal
#' @noRd
step_missing_code <- function(step_call) {
  matched <- if (is.call(step_call)) tryCatch(match.call(dv_step, step_call), error = function(problem) NULL)
  code <- if (is.null(matched)) NULL else matched[["missing_code"]]

  if (is.null(code)) {
    return(NA_real_)
  }

  value <- tryCatch(
    {
      check_literal_expression(code)
      eval(code, envir = baseenv())
    },
    error = function(problem) NA_real_
  )

  if (is.numeric(value) && length(value) == 1L) as.numeric(value) else NA_real_
}


#' Replace steps, and the comment blocks above them, with new lines
#'
#' Each file is changed from the bottom up, so earlier line numbers stay right.
#' @keywords internal
#' @noRd
replace_suite_steps <- function(step_rows, new_lines, backup_dir) {
  for (path in unique(step_rows$file)) {
    in_file <- which(step_rows$file == path)
    in_file <- in_file[order(step_rows$first_line[in_file], decreasing = TRUE)]
    lines <- readLines(path, encoding = "UTF-8", warn = FALSE)
    backup_suite_file(path, backup_dir)

    for (index in in_file) {
      first_line <- step_rows$first_line[[index]]
      last_line <- step_rows$last_line[[index]]
      # The written lines open with a blank line, which the file already has.
      replacement <- new_lines[[index]][cumsum(nzchar(new_lines[[index]])) > 0L]

      lines <- c(
        lines[seq_len(first_line - 1L)],
        replacement,
        lines[seq.int(last_line + 1L, length.out = length(lines) - last_line)]
      )
    }

    write_lines_utf8(lines, path)
  }

  invisible(NULL)
}


#' Add steps to the end of a topic file, creating it if needed, with test stubs for the unwritten
#' @keywords internal
#' @noRd
add_suite_steps <- function(suite_dir, topic, topic_rows, steps, backup_dir) {
  path <- file.path(suite_dir, paste0(topic, ".R"))
  step_lines <- unlist(lapply(steps, `[[`, "lines"), use.names = FALSE)

  if (file.exists(path)) {
    backup_suite_file(path, backup_dir)
    write_lines_utf8(c(readLines(path, encoding = "UTF-8", warn = FALSE), step_lines), path)
  } else {
    write_lines_utf8(c(topic_header_lines(topic, topic_rows, suite_dir), step_lines), path)
  }

  stubs <- Filter(function(step) !step$written, steps)

  if (length(stubs) == 0L) {
    return(invisible(path))
  }

  test_path <- file.path(suite_dir, "tests", paste0("test-", topic, ".R"))

  if (file.exists(test_path)) {
    backup_suite_file(test_path, backup_dir)
    write_lines_utf8(c(readLines(test_path, encoding = "UTF-8", warn = FALSE),
                       test_stub_lines(topic, stubs, header = FALSE)), test_path)
  } else {
    dir.create(dirname(test_path), recursive = TRUE, showWarnings = FALSE)
    write_lines_utf8(test_stub_lines(topic, stubs), test_path)
  }

  invisible(path)
}


#' Copy a file into the backup folder, once per sync
#' @keywords internal
#' @noRd
backup_suite_file <- function(path, backup_dir) {
  target <- file.path(backup_dir, basename(path))

  if (!file.exists(target)) {
    dir.create(backup_dir, recursive = TRUE, showWarnings = FALSE)
    file.copy(path, target)
  }

  invisible(target)
}


#' @keywords internal
#' @noRd
write_lines_utf8 <- function(lines, path) {
  # Forced first: lines often reads the file that opening it for writing empties.
  force(lines)
  connection <- file(path, open = "w", encoding = "UTF-8")
  on.exit(close(connection))
  writeLines(lines, connection)
  invisible(path)
}


#' Steps in the synced topics' files that the plan no longer builds
#' @keywords internal
#' @noRd
steps_not_in_plan <- function(existing, actions, topics) {
  in_topics <- existing[existing$topic %in% topics, , drop = FALSE]
  orphans <- in_topics[!tolower(in_topics$dv) %in% tolower(actions$dv), , drop = FALSE]

  data.frame(topic = orphans$topic, dv = orphans$dv, action = rep("not in the plan", nrow(orphans)),
             stringsAsFactors = FALSE)
}


#' @keywords internal
#' @noRd
report_topic_sync <- function(topic, action) {
  count_of <- function(what) sum(action == what)

  cli::cli_alert_success(
    "{.file {topic}.R}: {count_of('filled in')} filled in from signed-off rows, {count_of('added')} added, {count_of('written by hand')} already written and left alone, {count_of('still to write')} still to write."
  )

  invisible(NULL)
}


#' @keywords internal
#' @noRd
report_sync <- function(actions, backup_dir, suite_dir) {
  changed <- sum(actions$action %in% c("filled in", "added"))

  if (changed == 0L) {
    cli::cli_alert_info("Nothing to change: the suite already matches the signed-off rows.")
  } else if (dir.exists(backup_dir)) {
    cli::cli_alert_info("The files as they were before are in {.file {backup_dir}}.")
  }

  orphans <- actions$dv[actions$action == "not in the plan"]

  if (length(orphans) > 0L) {
    cli::cli_alert_warning(
      "{length(orphans)} step{?s} in the suite {?is/are} not built by the plan any more (now duplicate or not_a_derivation, or gone from the spec), so {?was/were} left as {?it is/they are}: {.val {utils::head(orphans, 10)}}{if (length(orphans) > 10) ', ...' else ''}. {cli::qty(length(orphans))}Delete {?it/them} if no longer wanted."
    )
  }

  still_to_write <- sum(actions$action == "still to write")

  if (still_to_write > 0L) {
    cli::cli_alert_info(
      "{still_to_write} step{?s} still {?has/have} {.code derive = NULL}: sign off {?its/their} row{?s} and sync again, or write {?it/them} by hand."
    )
  }

  if (changed > 0L) {
    cli::cli_alert_info("Next: {.code check_dv_suite(\"{suite_dir}\")}, then run the suite.")
  }

  invisible(NULL)
}
