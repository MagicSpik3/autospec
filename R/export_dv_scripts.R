#' Export the DVs as plain R scripts for the team that applies them
#'
#' The last step on our side. Writes the finished DVs into a folder (usually a
#' checkout of the scripts repo) as plain R that needs no wealthdv:
#'
#' * `dvs/<topic>.R` - one file per topic, one self-contained block per DV.
#'   Each block shows the same steps in SPSS syntax as comments, for readers
#'   who know SPSS.
#' * `run_dvs.R` - reads the data, runs the chosen files in the chosen order,
#'   prints what each added, and saves the result as `.rds` and `.sav`.
#' * `dv_config.xlsx` - where users give their data file, where to save, and
#'   which topic files to run in what order. The order is filled in so each
#'   file runs after the files it uses DVs from, which are listed beside it.
#'   Exporting again keeps what is already in it.
#' * `README.md` - how to run it.
#' * `<folder>.Rproj` - an RStudio project, if the folder has none yet.
#'
#' Before anything is written, the suite is checked and its example tests run,
#' then the scripts are built on `data` and compared DV by DV with the suite
#' run on the same data. Any difference stops the export, so what the other
#' team receives is known to match the suite. Steps not written yet are left
#' out and listed in the README.
#'
#' @param suite_dir The suite folder.
#' @param scripts_dir The folder to export to; created if missing.
#' @param data The data the suite runs on, used to prove the scripts match.
#' @param title A title for the scripts, such as
#'   `"Wealth and Assets Survey Round 9"`.
#'
#' @return The paths written, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' export_dv_scripts("dv_suite/R9", "D:/git/was-dvs", was_data, "Wealth and Assets Survey Round 9")
#' }
export_dv_scripts <- function(suite_dir = "dv_suite", scripts_dir, data, title = "Derived variables") {
  if (missing(scripts_dir) || !is.character(scripts_dir) || length(scripts_dir) != 1L ||
      is.na(scripts_dir) || !nzchar(scripts_dir)) {
    cli::cli_alert_danger("{.arg scripts_dir} must be the folder to export to.")
    stop("`scripts_dir` must be a single folder path.", call. = FALSE)
  }

  if (!is.data.frame(data)) {
    cli::cli_alert_danger("{.arg data} must be the data frame the suite runs on; it proves the scripts match.")
    stop("`data` must be a data frame.", call. = FALSE)
  }

  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    cli::cli_alert_danger("The openxlsx package is needed to write the config. Run setup_wealthdv.R to install it.")
    stop("openxlsx is not installed.", call. = FALSE)
  }

  problems <- check_dv_suite(suite_dir, data_names = names(data))

  # An input missing from this data only leaves that DV unchecked; anything else stops the export.
  blocking <- problems$severity == "error" & !startsWith(problems$problem, "input not in the data")

  if (any(blocking)) {
    cli::cli_alert_danger("The checks found errors, so nothing was exported. Fix them and export again.")
    stop("The suite has errors.", call. = FALSE)
  }

  results <- run_suite_tests(suite_dir, problems)

  if (!is.null(results)) {
    outcomes <- as.data.frame(results)
    failing <- unique(outcomes$test[outcomes$failed > 0L | outcomes$error])

    if (length(failing) > 0L) {
      cli::cli_alert_danger("{length(failing)} example test{?s} failed, so nothing was exported: {.val {failing}}.")
      stop("Example tests failed.", call. = FALSE)
    }
  }

  cli::cli_h1("Exporting the DVs as plain R to {.file {scripts_dir}}")

  suite <- suppressMessages(read_dv_suite(suite_dir))
  written <- vapply(suite$steps, function(step) is.function(step$derive), logical(1L))
  unwritten <- names(suite$steps)[!written]
  steps <- suite$steps[written]

  if (length(steps) == 0L) {
    cli::cli_alert_danger("No steps are written yet, so there is nothing to export.")
    stop("No written steps.", call. = FALSE)
  }

  steps <- steps[order_suite_steps(steps)]

  blocks <- lapply(steps, plain_step_code, settings = suite$settings)
  names(blocks) <- names(steps)

  cli::cli_alert_info("Checking the plain R gives the same DVs as the suite on {nrow(data)} rows.")
  unbuildable <- check_scripts_match(data, suite, blocks)

  # A DV the suite cannot build on this data would stop the file it sits in, so
  # it is left out rather than shipped untested.
  steps <- steps[!names(steps) %in% unbuildable]
  blocks <- blocks[names(steps)]
  files <- topic_files(steps)
  spss <- lapply(steps, spss_step_code)

  dir.create(file.path(scripts_dir, "dvs"), recursive = TRUE, showWarnings = FALSE)
  file.remove(list.files(file.path(scripts_dir, "dvs"), pattern = "[.]R$", full.names = TRUE))

  uses <- attr(files, "uses")

  for (index in seq_along(files)) {
    lines <- plain_topic_lines(names(files)[[index]], files[[index]], blocks, spss, title, uses[[index]])
    writeLines(lines, file.path(scripts_dir, "dvs", names(files)[[index]]))
  }

  writeLines(run_script_lines(title), file.path(scripts_dir, "run_dvs.R"))

  topic_table <- data.frame(
    Order = seq_along(files),
    File = names(files),
    DVs = lengths(files),
    `Uses DVs from` = vapply(uses, paste, character(1L), collapse = ", "),
    stringsAsFactors = FALSE,
    check.names = FALSE,
    row.names = NULL
  )
  config_file <- file.path(scripts_dir, "dv_config.xlsx")
  run_config <- write_run_config(config_file, topic_table, title)

  project_file <- write_rstudio_project(scripts_dir)
  writeLines(scripts_readme_lines(title, files, unwritten, unbuildable, project_file), file.path(scripts_dir, "README.md"))

  for (name in names(files)) {
    cli::cli_alert_success("{.file dvs/{name}}: {length(files[[name]])} DV{?s}.")
  }
  cli::cli_alert_success("{.file run_dvs.R}, {.file README.md} and {.file dv_config.xlsx}{if (run_config$kept) ' (keeping its settings, choices and order)' else ''}.")
  report_run_order(run_config$topics)

  if (length(unwritten) > 0L) {
    cli::cli_alert_warning("{length(unwritten)} DV{?s} not written yet {?is/are} left out; the README lists them.")
  }

  if (length(unbuildable) > 0L) {
    cli::cli_alert_warning("{length(unbuildable)} DV{?s} whose input is missing from this data {?is/are} left out too; the README lists them.")
  }

  invisible(c(file.path(scripts_dir, "dvs", names(files)), file.path(scripts_dir, "run_dvs.R"),
              config_file, file.path(scripts_dir, "README.md")))
}


#' Group ordered steps by topic, one file per topic, in an order that works
#'
#' Each file must only need DVs from files before it, so that running the
#' files in order works. Steps arrive in dependency order, so a topic's
#' position is where it first appears. The files each file uses DVs from are
#' attached as the attribute `"uses"`.
#' @keywords internal
#' @noRd
topic_files <- function(steps) {
  topics <- vapply(steps, `[[`, character(1L), "topic")
  all_topics <- unique(topics)

  # Which topics each topic needs DVs from
  needs_from <- lapply(all_topics, function(topic) {
    inputs <- unlist(lapply(steps[topics == topic], `[[`, "inputs"), use.names = FALSE)
    setdiff(unique(topics[match(intersect(inputs, names(steps)), names(steps))]), topic)
  })
  names(needs_from) <- all_topics

  topic_order <- character()

  while (length(topic_order) < length(all_topics)) {
    waiting <- setdiff(all_topics, topic_order)
    ready <- waiting[vapply(waiting, function(topic) all(needs_from[[topic]] %in% topic_order), logical(1L))]

    if (length(ready) == 0L) {
      cli::cli_alert_danger(
        "Topics {.val {waiting}} need DVs from each other in both directions, so they cannot be written as separate files run in order. Move a step from one topic file to the other."
      )
      stop("Topics depend on each other in both directions.", call. = FALSE)
    }

    topic_order <- c(topic_order, ready[[1L]])
  }

  files <- split(steps, factor(topics, levels = topic_order))
  names(files) <- paste0(topic_order, ".R")

  uses <- lapply(topic_order, function(topic) {
    used <- intersect(topic_order, needs_from[[topic]])
    if (length(used) == 0L) character() else paste0(used, ".R")
  })
  names(uses) <- names(files)
  attr(files, "uses") <- uses

  files
}


#' Say the order the files will run in, and any file ordered before one it uses DVs from
#' @keywords internal
#' @noRd
report_run_order <- function(topic_table) {
  ordered <- topic_table[order(topic_table$Order), , drop = FALSE]
  cli::cli_alert_info("Run order in {.file dv_config.xlsx}: {paste(ordered$File, collapse = ' > ')}.")

  problems <- run_order_problems(topic_table)

  if (length(problems) > 0L) {
    cli::cli_alert_warning("The Order kept from {.file dv_config.xlsx} runs a file before one it uses DVs from, so {.file run_dvs.R} will stop until the Order is changed:")
    cli::cli_bullets(stats::setNames(problems, rep("*", length(problems))))
  }

  invisible(problems)
}


#' Files ordered to run before a file they use DVs from, as sentences
#' @keywords internal
#' @noRd
run_order_problems <- function(topic_table) {
  problems <- character()

  for (index in seq_len(nrow(topic_table))) {
    used <- trimws(strsplit(topic_table$`Uses DVs from`[[index]], ",", fixed = TRUE)[[1L]])
    used <- used[nzchar(used)]
    later <- used[topic_table$Order[match(used, topic_table$File)] >= topic_table$Order[[index]]]
    later <- later[!is.na(later)]

    if (length(later) > 0L) {
      problems <- c(problems, paste0(
        topic_table$File[[index]], " (Order ", topic_table$Order[[index]], ") uses DVs from ",
        paste(later, collapse = ", "), ", which must have a lower Order"
      ))
    }
  }

  problems
}


#' The lines of one topic file
#' @keywords internal
#' @noRd
plain_topic_lines <- function(file_name, steps, blocks, spss, title, uses = character()) {
  built_here <- character()
  needs <- character()
  block_lines <- list()

  for (step in steps) {
    code <- blocks[[step$dv]]
    needs <- c(needs, setdiff(code$needs, built_here))
    built_here <- c(built_here, step$dv)

    block_lines[[length(block_lines) + 1L]] <- c(
      "",
      step_comment_lines(step),
      paste0("# Needs: ", paste(code$needs, collapse = ", ")),
      "# In SPSS:",
      paste0("#   ", spss[[step$dv]]),
      code$lines,
      if (!is.na(step$label)) paste0("attr(", col_ref(step$dv), ", \"label\") <- ", encodeString(step$label, quote = "\"")),
      ""
    )
  }

  needs <- unique(needs)
  topic <- steps[[1L]]$topic

  c(
    paste0("# ", topic_display_name(topic), " DVs: ", title),
    paste0("# Written from the DV specification on ", format(Sys.Date()), "."),
    "# Each block below builds one DV and reads and writes only the data frame `df`.",
    "# run_dvs.R runs the files in the Order set on the Topics sheet of dv_config.xlsx.",
    paste0("# This file uses DVs from: ", if (length(uses) > 0L) paste(uses, collapse = ", ") else "no other file", "."),
    "# Above each block's R is SPSS syntax doing the same. It assumes -8 and -9 are not",
    "# declared MISSING VALUES and no WEIGHT is on; #names are scratch variables.",
    "",
    "# The DVs this file builds; run_dvs.R reports on each of them",
    vector_lines("dvs_in_file", built_here),
    "",
    "# Columns this file needs, from the data or from files run before it",
    vector_lines("needed", needs),
    "",
    "# Use this file's spelling of any of these columns the data spells in another case;",
    "# run_dvs.R puts the data's spelling back afterwards",
    "for (name in c(needed, dvs_in_file)) {",
    "  found <- which(tolower(names(df)) == tolower(name))",
    "  if (length(found) == 1) names(df)[found] <- name",
    "}",
    "",
    "missing <- setdiff(needed, names(df))",
    paste0(
      "if (length(missing) > 0) stop(\"", file_name, " needs columns that are not in the data: \", paste(missing, collapse = \", \")",
      if (length(uses) > 0L) {
        paste0(", \". They may come from ", paste(uses, collapse = ", "), ": set ",
               if (length(uses) == 1L) "it to run, with a lower Order" else "them to run, with lower Orders", ".\"")
      } else "",
      ")"
    ),
    unlist(block_lines, use.names = FALSE)
  )
}


#' name <- c("a", "b", ...), a few names per line
#' @keywords internal
#' @noRd
vector_lines <- function(name, values) {
  if (length(values) <= 4L) {
    return(paste0(name, " <- ", literal(values)))
  }

  quoted <- encodeString(values, quote = "\"")
  rows <- split(quoted, ceiling(seq_along(quoted) / 4L))

  c(
    paste0(name, " <- c("),
    paste0("  ", vapply(rows, paste, character(1L), collapse = ", "), c(rep(",", length(rows) - 1L), "")),
    ")"
  )
}


#' The comment lines written above a step in its suite file: spec and source
#' @keywords internal
#' @noRd
step_comment_lines <- function(step) {
  heading <- paste0("# ", step$dv, if (!is.na(step$label)) paste0(": ", step$label))

  if (is.null(step$file) || !file.exists(step$file)) {
    return(heading)
  }

  lines <- readLines(step$file, warn = FALSE)
  start <- which(startsWith(lines, paste0("steps$", deparse_name(step$dv), " <- dv_step(")))

  if (length(start) != 1L) {
    return(heading)
  }

  comments <- character()
  index <- start - 1L

  while (index >= 1L && startsWith(lines[[index]], "#")) {
    comments <- c(lines[[index]], comments)
    index <- index - 1L
  }

  comments <- comments[!grepl("^# .* -{4,}$", comments)]
  comments <- comments[!grepl("^# (Not written yet|Plan notes|Already in the data|mark it not_a_derivation)", comments)]

  # The workbook is named without its folder, so our paths stay out of their code
  comments <- sub("^# (.*[.]xlsx)(,.*)?$", "# Spec: \\1\\2", comments)
  sub("^# Spec: .*[/\\\\]([^/\\\\]+[.]xlsx)", "# Spec: \\1", c(heading, comments))
}


#' Topic as users see it: property_wealth -> Property wealth
#' @keywords internal
#' @noRd
topic_display_name <- function(topic) {
  text <- gsub("_", " ", topic, fixed = TRUE)
  paste0(toupper(substr(text, 1L, 1L)), substring(text, 2L))
}


#' Run each DV's plain R on the data and compare it with the suite
#'
#' Steps the suite itself cannot build on this data (an input missing) are
#' left unchecked and listed, since the plain R would stop on them too.
#' @keywords internal
#' @noRd
check_scripts_match <- function(data, suite, blocks) {
  expected <- suppressMessages(run_dv_suite(data, suite, dvs = names(blocks)))
  report <- attr(expected, "dv_suite_report")
  failed <- report$dv[report$outcome == "failed"]

  if (length(failed) > 0L) {
    cli::cli_alert_danger("The suite fails on this data for {.val {failed}}: {report$detail[report$outcome == 'failed'][[1L]]}")
    stop("The suite fails on the data given.", call. = FALSE)
  }

  unchecked <- report$dv[report$outcome != "built"]

  # Compare in the scripts' spelling. Each topic file renames only the columns it
  # lists as needed or built, so rename just those, to catch any it misses.
  script_names <- unique(c(unlist(lapply(blocks, `[[`, "needs"), use.names = FALSE), names(blocks)))
  renaming <- case_clashes(names(data), script_names)
  expected <- rename_columns(expected, renaming$name, renaming$known)
  script_env <- new.env(parent = globalenv())
  script_env$df <- rename_columns(data, renaming$name, renaming$known)
  differing <- character()

  for (dv in setdiff(names(blocks), unchecked)) {
    outcome <- tryCatch(
      eval(parse(text = blocks[[dv]]$lines, keep.source = FALSE), envir = script_env),
      error = function(problem) problem
    )

    if (inherits(outcome, "error")) {
      cli::cli_alert_danger("The plain R for {dv} fails: {conditionMessage(outcome)}")
      stop("The plain R for a DV fails.", call. = FALSE)
    }

    if (!dv %in% names(script_env$df) || !same_values(script_env$df[[dv]], expected[[dv]])) {
      differing <- c(differing, dv)
    }
  }

  if (length(differing) > 0L) {
    cli::cli_alert_danger(
      "{length(differing)} DV{?s} from the plain R differ{?s/} from the suite, so nothing was exported: {.val {utils::head(differing, 10)}}{if (length(differing) > 10) ', ...' else ''}"
    )
    stop("The plain R does not match the suite.", call. = FALSE)
  }

  cli::cli_alert_success("{length(blocks) - length(unchecked)} DV{?s} match the suite.")

  if (length(unchecked) > 0L) {
    cli::cli_alert_warning(
      "The suite cannot build {length(unchecked)} DV{?s} on this data because an input is missing: {.val {utils::head(unchecked, 10)}}{if (length(unchecked) > 10) ', ...' else ''}"
    )
  }

  unchecked
}


#' @keywords internal
#' @noRd
same_values <- function(ours, theirs) {
  ours <- suppressWarnings(as.numeric(ours))
  theirs <- suppressWarnings(as.numeric(theirs))

  length(ours) == length(theirs) && all(
    (is.na(ours) & is.na(theirs)) |
      (!is.na(ours) & !is.na(theirs) & abs(ours - theirs) <= pmax(1e-8, abs(theirs) * 1e-9))
  )
}


#' An RStudio project in the scripts folder, so opening it sets the working directory
#'
#' A project already there is kept as it is.
#' @return The project's file name.
#' @keywords internal
#' @noRd
write_rstudio_project <- function(scripts_dir) {
  existing <- list.files(scripts_dir, pattern = "[.]Rproj$")

  if (length(existing) > 0L) {
    return(existing[[1L]])
  }

  project_file <- paste0(basename(normalizePath(scripts_dir, winslash = "/")), ".Rproj")
  writeLines(c(
    "Version: 1.0",
    "",
    "RestoreWorkspace: No",
    "SaveWorkspace: No",
    "AlwaysSaveHistory: Default",
    "",
    "EnableCodeIndexing: Yes",
    "UseSpacesForTab: Yes",
    "NumSpacesForTab: 2",
    "Encoding: UTF-8",
    "",
    "RnwWeave: Sweave",
    "LaTeX: pdfLaTeX"
  ), file.path(scripts_dir, project_file))

  project_file
}


#' The script users run
#' @keywords internal
#' @noRd
run_script_lines <- function(title) {
  c(
    paste0("# Adds the derived variables (DVs) to your data: ", title, "."),
    "#",
    "# 1. Open dv_config.xlsx. On the Settings sheet give your data file and where to",
    "#    save the result. On the Topics sheet set Run? to Yes for each file you want,",
    "#    and number them in the Order column to choose the order they run in.",
    "# 2. Run this script: in RStudio, click Source.",
    "#",
    "# The files in dvs/ build the DVs, one topic per file. Open them to see or change",
    "# how a DV is made. A file that uses DVs from another (see Uses DVs from on the",
    "# Topics sheet) must run after it.",
    "",
    "library(readxl) # reads dv_config.xlsx",
    "library(haven)  # reads and writes SPSS files",
    "",
    "settings <- read_excel(\"dv_config.xlsx\", sheet = \"Settings\")",
    "data_file <- settings$Value[settings$Setting == \"Data file\"]",
    "save_as <- settings$Value[settings$Setting == \"Save as\"]",
    "topics <- read_excel(\"dv_config.xlsx\", sheet = \"Topics\")",
    "chosen <- topics[topics$`Run?` %in% \"Yes\", ]",
    "",
    "if (is.na(data_file) || !file.exists(data_file)) stop(\"Fill in Data file on the Settings sheet with a file that exists: \", data_file)",
    "if (is.na(save_as) || save_as == \"\") stop(\"Fill in Save as on the Settings sheet\")",
    "if (nrow(chosen) == 0) stop(\"Set Run? to Yes for at least one file on the Topics sheet\")",
    "save_as <- tools::file_path_sans_ext(save_as)",
    "",
    "# Put the chosen files in the Order given on the Topics sheet",
    "run_order <- suppressWarnings(as.numeric(chosen$Order))",
    "if (anyNA(run_order)) stop(\"Give each file set to run a number in the Order column: \", paste(chosen$File[is.na(run_order)], collapse = \", \"))",
    "if (anyDuplicated(run_order)) stop(\"Files set to run share an Order number; give each its own: \", paste(chosen$File[run_order %in% run_order[duplicated(run_order)]], collapse = \", \"))",
    "chosen <- chosen[order(run_order), ]",
    "files_to_run <- chosen$File",
    "",
    "# Each file must run after the files it uses DVs from",
    "for (i in seq_along(files_to_run)) {",
    "  uses <- trimws(strsplit(as.character(chosen$`Uses DVs from`[i]), \",\")[[1]])",
    "  runs_later <- intersect(uses, files_to_run[-seq_len(i)])",
    "  if (length(runs_later) > 0) stop(files_to_run[i], \" uses DVs from \", paste(runs_later, collapse = \", \"), \", so on the Topics sheet give \", paste(runs_later, collapse = \", \"), \" a lower Order than \", files_to_run[i])",
    "}",
    "cat(\"Running, in this order:\", paste(files_to_run, collapse = \" > \"), \"\\n\")",
    "",
    "# Read the data: .sav, .csv or .rds",
    "cat(\"Reading\", data_file, \"\\n\")",
    "df <- switch(tolower(tools::file_ext(data_file)),",
    "  sav = read_sav(data_file),",
    "  csv = read.csv(data_file),",
    "  rds = readRDS(data_file),",
    "  stop(\"Data file must be a .sav, .csv or .rds file\")",
    ")",
    "cat(nrow(df), \"rows and\", ncol(df), \"columns\\n\")",
    "",
    "# Run each chosen file and report what it changed",
    "report <- data.frame()",
    "",
    "for (file in files_to_run) {",
    "  cat(\"\\n==\", file, \"==\\n\")",
    "  before <- df",
    "  outcome <- try(source(file.path(\"dvs\", file)))",
    "",
    "  if (inherits(outcome, \"try-error\")) {",
    "    cat(\"FAILED, so none of its DVs were added. See the error above.\\n\")",
    "    report <- rbind(report, data.frame(File = file, DV = NA, Outcome = \"failed\", Missing = NA, Min = NA, Max = NA))",
    "    df <- before",
    "    next",
    "  }",
    "",
    "  # Put back your data's spelling of any column the file matched in another case",
    "  spelt_before <- match(tolower(names(df)), tolower(names(before)))",
    "  names(df)[!is.na(spelt_before)] <- names(before)[spelt_before[!is.na(spelt_before)]]",
    "",
    "  # Each file lists the DVs it builds, so a DV whose value was already right is reported too",
    "  for (dv in dvs_in_file) {",
    "    dv <- names(df)[match(tolower(dv), tolower(names(df)))]",
    "    values <- suppressWarnings(as.numeric(df[[dv]]))",
    "    outcome_text <- if (!dv %in% names(before)) {",
    "      \"added\"",
    "    } else if (identical(as.numeric(before[[dv]]), values)) {",
    "      \"unchanged\"",
    "    } else {",
    "      \"replaced\"",
    "    }",
    "    missing_count <- sum(is.na(values))",
    "    lowest <- if (all(is.na(values))) NA else min(values, na.rm = TRUE)",
    "    highest <- if (all(is.na(values))) NA else max(values, na.rm = TRUE)",
    "    cat(sprintf(\"  %-32s %-9s missing %6d   min %14s   max %14s\\n\", dv, outcome_text, missing_count, format(lowest), format(highest)))",
    "    report <- rbind(report, data.frame(File = file, DV = dv, Outcome = outcome_text, Missing = missing_count, Min = lowest, Max = highest))",
    "  }",
    "",
    "  cat(\"  \", length(dvs_in_file), \"DVs\\n\")",
    "}",
    "",
    "# Save the data with the DVs added, and the report",
    "saveRDS(df, paste0(save_as, \".rds\"))",
    "write_sav(df, paste0(save_as, \".sav\"))",
    "write.csv(report, paste0(save_as, \"_dv_report.csv\"), row.names = FALSE)",
    "cat(\"\\nSaved\", paste0(save_as, \".rds\"), \"and\", paste0(save_as, \".sav\"), \"\\n\")",
    "cat(\"Report of every DV in\", paste0(save_as, \"_dv_report.csv\"), \"\\n\")"
  )
}


#' Write the config users fill in, keeping what an earlier one held
#'
#' @return A list: `kept`, whether settings or choices were kept from an
#'   earlier config, and `topics`, the Topics sheet as written.
#' @keywords internal
#' @noRd
write_run_config <- function(path, topic_table, title) {
  previous <- if (file.exists(path)) {
    sheets <- readxl::excel_sheets(path)
    read_sheet <- function(sheet) {
      if (!sheet %in% sheets) return(NULL)
      as.data.frame(readxl::read_excel(path, sheet = sheet, col_types = "text"), stringsAsFactors = FALSE)
    }
    list(settings = read_sheet("Settings"), topics = read_sheet("Topics"))
  }

  kept_value <- function(table, key_column, keys, value_column, default) {
    if (is.null(table) || !all(c(key_column, value_column) %in% names(table))) {
      return(rep(default, length(keys)))
    }

    # Files were once numbered, as 1_income.R; match them to income.R.
    unnumbered <- function(files) sub("^[0-9]+_", "", files)
    values <- table[[value_column]][match(unnumbered(keys), unnumbered(table[[key_column]]))]
    ifelse(is.na(values) | !nzchar(values), default, values)
  }

  settings <- data.frame(
    Setting = c("Data file", "Save as"),
    Value = NA_character_,
    Help = c(
      "The data to add the DVs to: a .sav, .csv or .rds file, for example E:/data/imputed_data.sav",
      "Where to save the result, without an extension: it is saved as .rds and .sav, for example E:/data/data_with_dvs"
    ),
    stringsAsFactors = FALSE
  )
  settings$Value <- kept_value(previous$settings, "Setting", settings$Setting, "Value", "")

  topic_table[["Run?"]] <- kept_value(previous$topics, "File", topic_table$File, "Run?", "Yes")
  topic_table$Order <- kept_order(
    suppressWarnings(as.numeric(kept_value(previous$topics, "File", topic_table$File, "Order", NA))),
    topic_table$Order
  )
  topic_table <- topic_table[order(topic_table$Order), c("File", "Run?", "Order", "DVs", "Uses DVs from")]
  rownames(topic_table) <- NULL

  instructions <- c(
    paste0("Adding the derived variables (DVs) to your data: ", title),
    "",
    "1. On the Settings sheet, fill in your data file and where to save the result.",
    "2. On the Topics sheet, set Run? to Yes for each file of DVs you want, and number",
    "   them in the Order column to choose the order they run in (1 runs first).",
    "   A file must run after the files listed in its Uses DVs from column; run_dvs.R",
    "   stops and says so if it would not.",
    "3. Save and close this file, then run run_dvs.R.",
    "",
    "Your data is saved with the DVs added, as .rds and .sav, with a report listing every DV",
    "added or replaced, how many values are missing, and its lowest and highest value."
  )

  workbook <- openxlsx::createWorkbook()
  header_style <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#DDEBF7", border = "bottom")
  entry_style <- openxlsx::createStyle(fgFill = "#FFF2CC")

  openxlsx::addWorksheet(workbook, "Instructions")
  openxlsx::writeData(workbook, "Instructions", data.frame(text = instructions), colNames = FALSE)
  openxlsx::addStyle(workbook, "Instructions", openxlsx::createStyle(textDecoration = "bold", fontSize = 13), rows = 1, cols = 1)
  openxlsx::setColWidths(workbook, "Instructions", cols = 1, widths = 100)

  openxlsx::addWorksheet(workbook, "Settings")
  openxlsx::writeData(workbook, "Settings", settings, headerStyle = header_style)
  openxlsx::setColWidths(workbook, "Settings", cols = 1:3, widths = c(12, 70, 100))
  openxlsx::addStyle(workbook, "Settings", entry_style, rows = 2:3, cols = 2)

  topic_rows <- seq_len(nrow(topic_table)) + 1L

  openxlsx::addWorksheet(workbook, "Topics")
  openxlsx::writeData(workbook, "Topics", topic_table, headerStyle = header_style)
  openxlsx::setColWidths(workbook, "Topics", cols = 1:5, widths = c(32, 8, 8, 8, 60))
  openxlsx::addStyle(workbook, "Topics", entry_style, rows = topic_rows, cols = 2:3, gridExpand = TRUE)
  openxlsx::addWorksheet(workbook, "Lists", visible = FALSE)
  openxlsx::writeData(workbook, "Lists", data.frame(Answers = c("Yes", "No")))
  openxlsx::dataValidation(workbook, "Topics", col = 2, rows = topic_rows,
                           type = "list", value = "'Lists'!$A$2:$A$3")
  openxlsx::dataValidation(workbook, "Topics", col = 3, rows = topic_rows,
                           type = "whole", operator = "greaterThanOrEqual", value = 1)

  openxlsx::saveWorkbook(workbook, path, overwrite = TRUE)

  list(kept = !is.null(previous), topics = topic_table)
}


#' The run order: the numbers kept from the last config, with new files after them
#'
#' Files with no number kept, such as topics new since the last export, go
#' last in the order worked out from the DVs they use.
#' @keywords internal
#' @noRd
kept_order <- function(kept, default) {
  if (all(is.na(kept))) {
    return(default)
  }

  new_files <- is.na(kept)
  kept[new_files] <- max(kept, na.rm = TRUE) + rank(default[new_files])
  kept
}


#' @keywords internal
#' @noRd
scripts_readme_lines <- function(title, files, unwritten, unbuildable = character(),
                                 project_file = "the .Rproj file") {
  c(
    paste0("# ", title, ": derived variables"),
    "",
    "R scripts that add the derived variables (DVs) to the survey data.",
    "",
    "## To run",
    "",
    paste0("1. Double-click `", project_file, "` to open the project in RStudio. This sets the"),
    "   working directory, so the scripts find `dv_config.xlsx` and `dvs/` on their own.",
    "2. Open `dv_config.xlsx`. On **Settings**, fill in your data file (`.sav`, `.csv`",
    "   or `.rds`) and where to save the result. On **Topics**, set **Run?** to Yes for",
    "   each file of DVs you want, and number them in **Order** to choose the order",
    "   they run in (1 runs first).",
    "3. Open `run_dvs.R` in RStudio and click **Source**.",
    "",
    "The data is saved with the DVs added, as `.rds` and `.sav`, with a report listing",
    "every DV: **added** (new to your data), **replaced** (it was there with different",
    "values) or **unchanged** (it was there and already correct). You need R with the",
    "`haven` and `readxl` packages.",
    "",
    "## The files",
    "",
    "| File | DVs |",
    "|---|---|",
    paste0("| `dvs/", names(files), "` | ", lengths(files), " |"),
    "",
    "Each file holds one block per DV: the spec it came from, the columns it needs,",
    "and the R that builds it. Files run in the **Order** set on the **Topics** sheet.",
    "A file that uses DVs from another, listed under **Uses DVs from**, must run after",
    "it; `run_dvs.R` checks this before it starts and says what to change. The Order",
    "is filled in to work when the files are exported, and your numbers are kept when",
    "they are exported again. To change how a DV is made, edit its block; nothing else",
    "depends on the code inside a block, only on the column it makes.",
    "",
    "Above the R in each block, under `# In SPSS:`, is SPSS syntax that does the same,",
    "for reading alongside the R. It assumes -8 and -9 are not declared as missing",
    "values and no `WEIGHT` is on; names starting `#` are scratch variables. If you",
    "change a block's R, its SPSS is not updated.",
    "",
    if (length(unwritten) > 0L) c(
      "## Not yet built",
      "",
      "These DVs are in the specification but not yet written:",
      "",
      paste0("- ", sort(unwritten)),
      ""
    ),
    if (length(unbuildable) > 0L) c(
      "## Left out: a column they need is missing",
      "",
      "These DVs are written, but a column they read was not in the data they were",
      "exported from, so they are not here. Ask the DV team if you need them:",
      "",
      paste0("- ", sort(unbuildable)),
      ""
    ),
    paste0("Exported on ", format(Sys.Date()), ".")
  )
}
