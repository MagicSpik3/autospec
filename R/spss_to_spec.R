#' Convert an SPSS syntax file into a spec-like data frame
#'
#' This is a first-pass converter intended for legacy SPSS code. It reads a
#' syntax file, extracts derivation-producing commands, and produces one row per
#' output variable with a `level` (`person` or `household`), original file
#' metadata, and the SPSS line numbers that generated the row.
#'
#' The converter is deliberately conservative: it focuses on commands that
#' create or transform variables (`COMPUTE`, `RECODE`, `DO IF`, `AGGREGATE`) and
#' ignores reporting and control statements such as `SORT`, `LIST`, and `TEMPORARY`.
#'
#' @param sps_path Path to the `.sps` file to convert.
#' @param output_path Optional path to write the generated CSV.
#' @param quiet Suppress progress messages.
#'
#' @returns A data frame with columns:
#' \describe{
#'   \item{file_name}{Original file name.}
#'   \item{file_location}{Original file directory.}
#'   \item{level}{`person` or `household`.}
#'   \item{variable}{Output variable name.}
#'   \item{label}{Short human-readable label from nearby comments.}
#'   \item{derivation}{SPSS derivation text.}
#'   \item{notes}{Comments and context captured around the command.}
#'   \item{spss line number(s)}{SPSS line numbers relevant to the row.}
#'   \item{spss command}{Command type.}
#' }
#'
#' @examples
#' \dontrun{
#'  spec <- spss_to_spec("test_SPSS/2_example.sps")
#'  write.csv(spec, "test_SPSS/2_example_spec.csv", row.names = FALSE)
#' }
#' @export
spss_to_spec <- function(sps_path, output_path = NULL, quiet = FALSE) {
  if (!file.exists(sps_path)) {
    stop("SPSS file not found: ", sps_path, call. = FALSE)
  }

  lines <- readLines(sps_path, warn = FALSE, encoding = "windows-1252")
  rows <- parse_spss_lines(lines)

  if (nrow(rows) == 0L) {
    rows <- data.frame(
      source_file = basename(sps_path),
      source_location = normalizePath(dirname(sps_path), winslash = "/"),
      level = character(),
      variable = character(),
      label = character(),
      derivation = character(),
      notes = character(),
      "spss line number(s)" = character(),
      "spss command" = character(),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  } else {
    rows$source_file <- basename(sps_path)
    rows$source_location <- normalizePath(dirname(sps_path), winslash = "/")
  }

  if ("spss.line.number.s." %in% names(rows)) {
    names(rows)[names(rows) == "spss.line.number.s."] <- "spss line number(s)"
  }
  if ("spss.command" %in% names(rows)) {
    names(rows)[names(rows) == "spss.command"] <- "spss command"
  }

  rows <- rows[, c(
    "source_file",
    "source_location",
    "level",
    "variable",
    "label",
    "derivation",
    "notes",
    "spss line number(s)",
    "spss command"
  ), drop = FALSE]

  if (!is.null(output_path)) {
    utils::write.csv(rows, output_path, row.names = FALSE)
  }

  if (!quiet) {
    message("Converted ", nrow(rows), " derivation row(s) from ", basename(sps_path), ".")
  }

  rows
}

parse_spss_lines <- function(lines) {
  rows <- list()
  index <- 1L
  pending_notes <- character()
  i <- 1L

  while (i <= length(lines)) {
    line <- lines[[i]]
    trimmed <- trimws(line)

    if (trimmed == "") {
      i <- i + 1L
      next
    }

    if (grepl("^\\*", line)) {
      note <- clean_comment_line(line)
      if (nzchar(note)) {
        pending_notes <- c(pending_notes, note)
      }
      i <- i + 1L
      next
    }

    command <- detect_spss_command(line)
    if (is.null(command)) {
      i <- i + 1L
      next
    }

    block <- collect_block(lines, i, command)
    block_lines <- block$lines
    block_end <- block$end
    comments <- if (length(pending_notes) > 0L) unique(pending_notes) else character()
    pending_notes <- character()

    parsed_rows <- parse_command_block(command, block_lines, comments, block$start:block$end)
    if (length(parsed_rows) > 0L) {
      rows <- c(rows, parsed_rows)
    }

    i <- block_end + 1L
  }

  if (length(rows) == 0L) {
    out <- data.frame(
      level = character(),
      variable = character(),
      label = character(),
      derivation = character(),
      notes = character(),
      "spss line number(s)" = character(),
      "spss command" = character(),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    if ("spss.line.number.s." %in% names(out)) {
      names(out)[names(out) == "spss.line.number.s."] <- "spss line number(s)"
    }
    if ("spss.command" %in% names(out)) {
      names(out)[names(out) == "spss.command"] <- "spss command"
    }
    return(out)
  }

  out <- do.call(rbind, lapply(rows, function(row) {
    data.frame(
      level = row[["level"]],
      variable = row[["variable"]],
      label = row[["label"]],
      derivation = row[["derivation"]],
      notes = row[["notes"]],
      "spss line number(s)" = row[["spss line number(s)"]],
      "spss command" = row[["spss command"]],
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }))

  if ("spss.line.number.s." %in% names(out)) {
    names(out)[names(out) == "spss.line.number.s."] <- "spss line number(s)"
  }
  if ("spss.command" %in% names(out)) {
    names(out)[names(out) == "spss.command"] <- "spss command"
  }

  out
}

collect_block <- function(lines, start_index, command) {
  block_lines <- lines[start_index]
  end_index <- start_index

  if (identical(command, "do_if")) {
    depth <- 0L
    while (end_index < length(lines)) {
      candidate <- lines[end_index + 1L]
      if (grepl("^\\s*DO\\s+IF\\b", toupper(candidate), perl = TRUE)) {
        depth <- depth + 1L
      }
      if (grepl("^\\s*END\\s+IF\\b", toupper(candidate), perl = TRUE)) {
        if (depth == 0L) {
          block_lines <- c(block_lines, candidate)
          end_index <- end_index + 1L
          break
        }
        depth <- depth - 1L
      }
      block_lines <- c(block_lines, candidate)
      end_index <- end_index + 1L
    }
    if (end_index == start_index) {
      end_index <- start_index
    }
    return(list(lines = block_lines, start = start_index, end = end_index))
  }

  if (identical(command, "aggregate")) {
    while (end_index < length(lines)) {
      candidate <- lines[end_index + 1L]
      if (grepl("^\\s*EXE\\.?\\s*$", candidate, perl = TRUE) ||
          grepl("^\\s*(COMPUTE|RECODE|DO\\s+IF|IF\\s+\\(|AGGREGATE|SELECT\\s+IF|SORT\\s+CASES|LIST|TEMPORARY|TEMP\\b)", toupper(candidate), perl = TRUE)) {
        break
      }
      block_lines <- c(block_lines, candidate)
      end_index <- end_index + 1L
    }
    return(list(lines = block_lines, start = start_index, end = end_index))
  }

  if (identical(command, "compute") || identical(command, "recode")) {
    return(list(lines = block_lines, start = start_index, end = start_index))
  }

  list(lines = block_lines, start = start_index, end = start_index)
}

detect_spss_command <- function(line) {
  trimmed <- trimws(line)
  if (trimmed == "") {
    return(NULL)
  }
  upper <- toupper(trimmed)

  if (grepl("^DO\\s+IF\\b", upper, perl = TRUE)) return("do_if")
  if (grepl("^COMPUTE\\b", upper, perl = TRUE)) return("compute")
  if (grepl("^RECODE\\b", upper, perl = TRUE)) return("recode")
  if (grepl("^AGGREGATE\\b", upper, perl = TRUE)) return("aggregate")
  if (grepl("^SELECT\\s+IF\\b", upper, perl = TRUE)) return("select_if")
  if (grepl("^SORT\\s+CASES\\b", upper, perl = TRUE)) return("sort")
  if (grepl("^LIST\\b", upper, perl = TRUE)) return("list")
  if (grepl("^TEMPORARY\\b|^TEMP\\b", upper, perl = TRUE)) return("temporary")
  if (grepl("^FRE\\s+VAR\\b", upper, perl = TRUE)) return("fre_var")
  if (grepl("^VARIABLE\\s+LABELS\\b", upper, perl = TRUE)) return("variable_labels")
  if (grepl("^FORMATS\\b", upper, perl = TRUE)) return("formats")
  NULL
}

parse_command_block <- function(command, block_lines, comments, line_numbers) {
  out <- list()

  if (identical(command, "compute")) {
    text <- paste(block_lines, collapse = " ")
    match <- regexec("COMPUTE\\s+([A-Za-z0-9_.]+)\\s*=\\s*([^.]*)\\.?", text, perl = TRUE)
    result <- regmatches(text, match)[[1L]]
    if (length(result) >= 3L) {
      out[[length(out) + 1L]] <- row_template(
        variable = result[2L],
        derivation = paste0(result[2L], " = ", trimws(result[3L])),
        notes = comments,
        command = "COMPUTE",
        level = determine_level(result[2L], text),
        line_number = paste(line_numbers, collapse = ", ")
      )
    }
    return(out)
  }

  if (identical(command, "recode")) {
    text <- paste(block_lines, collapse = " ")
    match <- regexec("RECODE\\s+([A-Za-z0-9_.]+)\\s*(.*)\\s+INTO\\s+([A-Za-z0-9_.]+)", text, perl = TRUE)
    result <- regmatches(text, match)[[1L]]
    if (length(result) >= 4L) {
      out[[length(out) + 1L]] <- row_template(
        variable = result[4L],
        derivation = paste0(result[4L], " = RECODE(", trimws(result[2L]), result[3L], ")"),
        notes = comments,
        command = "RECODE",
        level = determine_level(result[4L], text),
        line_number = paste(line_numbers, collapse = ", ")
      )
    }
    return(out)
  }

  if (identical(command, "aggregate")) {
    text <- paste(block_lines, collapse = " ")
    group_by <- if (grepl("BREAK\\s*=\\s*([A-Za-z0-9_.]+)", text, perl = TRUE)) {
      sub(".*BREAK\\s*=\\s*([A-Za-z0-9_.]+).*", "\\1", text, perl = TRUE)
    } else {
      NA_character_
    }

    captures <- gregexpr("/\\s*([A-Za-z0-9_.]+)\\s*=\\s*([^/]+)", text, perl = TRUE)
    matches <- regmatches(text, captures)[[1L]]

    if (length(matches) > 0L) {
      for (match in matches) {
        part <- regexec("/\\s*([A-Za-z0-9_.]+)\\s*=\\s*(.+)", match, perl = TRUE)
        parts <- regmatches(match, part)[[1L]]
        if (length(parts) >= 3L) {
          variable <- parts[2L]
          derivation <- trimws(parts[3L])
          out[[length(out) + 1L]] <- row_template(
            variable = variable,
            derivation = if (is.na(group_by)) trimws(derivation) else paste0(variable, " = ", derivation, " grouped by ", group_by),
            notes = c(comments, if (!is.na(group_by)) paste0("Household aggregation grouped by ", group_by) else "Household aggregation"),
            command = "AGGREGATE",
            level = "household",
            line_number = paste(line_numbers, collapse = ", ")
          )
        }
      }
      return(out)
    }
  }

  if (identical(command, "do_if")) {
    text <- paste(block_lines, collapse = " ")
    assigned <- unique(unlist(regmatches(text, gregexpr("COMPUTE\\s+([A-Za-z0-9_.]+)", text, perl = TRUE))))
    variable_names <- sub(".*COMPUTE\\s+([A-Za-z0-9_.]+).*", "\\1", assigned, perl = TRUE)
    variable_names <- variable_names[nzchar(variable_names)]

    if (length(variable_names) > 0L) {
      for (variable in variable_names) {
        out[[length(out) + 1L]] <- row_template(
          variable = variable,
          derivation = paste0(variable, " = conditional logic from DO IF block"),
          notes = c(comments, "Conditioned assignment block"),
          command = "DO IF",
          level = determine_level(variable, text),
          line_number = paste(line_numbers, collapse = ", ")
        )
      }
      return(out)
    }
  }

  out
}

row_template <- function(variable, derivation, notes, command, level, line_number) {
  label <- make_label(variable, notes)
  list(
    level = level,
    variable = variable,
    label = label,
    derivation = derivation,
    notes = paste(unique(notes), collapse = "; "),
    "spss line number(s)" = line_number,
    "spss command" = command
  )
}

make_label <- function(variable, notes) {
  if (length(notes) > 0L && nzchar(notes[1])) {
    chunk <- trimws(notes[1])
    chunk <- gsub("^\\*\\s*", "", chunk)
    chunk <- gsub("^[A-Za-z0-9]\\)\\s*", "", chunk)
    chunk <- gsub("\\s+", " ", chunk)
    if (nzchar(chunk)) {
      return(chunk)
    }
  }
  paste("Derived", variable)
}

determine_level <- function(variable, text) {
  if (grepl("(?i)BREAK\\s*=\\s*var_b01|hhserial|var_b01", text, perl = TRUE) ||
      (grepl("(?i)(sum\\(|mean\\(|max\\(|min\\(|aggregate|grouped by)", text, perl = TRUE) &&
       grepl("(?i)(var_b01|hhserial)", text, perl = TRUE))) {
    return("household")
  }
  if (grepl("(?i)(var_b01|hhserial)", variable, perl = TRUE)) {
    return("household")
  }
  "person"
}

clean_comment_line <- function(line) {
  x <- sub("^\\s*\\*\\s*", "", line)
  x <- sub("^\\s*[A-Za-z0-9]\\)\\s*", "", x)
  x <- trimws(x)
  x <- gsub("\\s+", " ", x)
  x
}
