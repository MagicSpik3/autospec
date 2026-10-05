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
#' A simple `DO IF RANGE(x, low, high)` with two `COMPUTE` branches assigning
#' 1 and 0 is converted to a flag derivation. A nearby `Requirement UID`
#' comment is retained; generated spec notes identify the source file and
#' SPSS line numbers for review.
#'
#' @param sps_path Path to the `.sps` file to convert.
#' @param output_path Optional path to write a CSV spec that can be read by
#'   [make_catalogue()]. Person and household derivations are written as
#'   separate blocks; SPSS source locations and notes are retained in Notes.
#' @param quiet Suppress progress messages.
#'
#' @returns A data frame with columns:
#' \describe{
#'   \item{source_file}{Original file name.}
#'   \item{source_location}{Original file directory.}
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
#'  spec <- spss_to_spec(
#'    "test_SPSS/2_example.sps",
#'    output_path = "specs/2_example.csv"
#'  )
#'  catalogue <- make_catalogue("specs/2_example.csv")
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
      uid = character(),
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
    "uid",
    "label",
    "derivation",
    "notes",
    "spss line number(s)",
    "spss command"
  ), drop = FALSE]

  if (!is.null(output_path)) {
    utils::write.table(
      spss_spec_csv(rows),
      output_path,
      sep = ",",
      quote = TRUE,
      row.names = FALSE,
      col.names = FALSE,
      na = ""
    )
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

    variable_range <- regmatches(
      trimmed,
      regexec(
        "^SELECT(?:\\s+IF)?\\s+([A-Za-z0-9_.]+)\\s+TO\\s+([A-Za-z0-9_.]+)\\b",
        trimmed,
        ignore.case = TRUE,
        perl = TRUE
      )
    )[[1L]]
    if (length(variable_range) >= 3L) {
      pending_notes <- c(
        pending_notes,
        paste0(
          "Unexpanded SPSS variable range ",
          variable_range[[2L]],
          " TO ",
          variable_range[[3L]],
          ": dataset variable order is required to identify the intervening variables"
        )
      )
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
      uid = character(),
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
      uid = row[["uid"]],
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

spss_spec_csv <- function(rows) {
  person <- rows[rows$level == "person", , drop = FALSE]
  household <- rows[rows$level == "household", , drop = FALSE]
  two_blocks <- nrow(person) > 0L && nrow(household) > 0L
  has_person <- nrow(person) > 0L || nrow(household) == 0L
  row_count <- max(nrow(person), nrow(household))
  column_count <- if (two_blocks) 10L else 7L

  out <- as.data.frame(
    matrix("", nrow = row_count + 2L, ncol = column_count),
    stringsAsFactors = FALSE
  )
  block_headers <- c("Variable Name", "Label", "Derivation", "Notes", "Requirement UID")
  names(out) <- if (two_blocks) {
    c(block_headers, block_headers)
  } else {
    c(block_headers, rep("", 2L))
  }

  out[1L, 1L] <- if (has_person) "Person" else "Household"
  if (two_blocks) {
    out[1L, 5L] <- "Household"
  }
  out[2L, ] <- names(out)

  fill_block <- function(block, columns) {
    if (nrow(block) == 0L) {
      return(invisible(NULL))
    }

    row_numbers <- seq_len(nrow(block)) + 2L
    block_notes <- vapply(seq_len(nrow(block)), function(index) {
      note <- block$notes[[index]]
      if (is.na(note)) note <- ""
      provenance <- paste0(
        "SPSS ", block$source_file[[index]], " lines ",
        block$`spss line number(s)`[[index]], " (",
        block$`spss command`[[index]], ")"
      )
      paste(c(note[nzchar(note)], provenance), collapse = "; ")
    }, character(1L))

    out[row_numbers, columns] <<- data.frame(
      variable = block$variable,
      label = block$label,
      derivation = block$derivation,
      notes = block_notes,
      uid = block$uid,
      stringsAsFactors = FALSE
    )
    invisible(NULL)
  }

  if (two_blocks) {
    fill_block(person, 1L:5L)
    fill_block(household, 6L:10L)
  } else if (nrow(person) > 0L) {
    fill_block(person, 1L:5L)
  } else if (nrow(household) > 0L) {
    fill_block(household, 1L:5L)
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

    simple_flag <- parse_simple_range_flag(block_lines)
    if (!is.null(simple_flag)) {
      out[[length(out) + 1L]] <- row_template(
        variable = simple_flag$variable,
        derivation = simple_flag$derivation,
        notes = comments,
        command = "DO IF",
        level = determine_level(simple_flag$variable, text),
        line_number = paste(line_numbers, collapse = ", ")
      )
      return(out)
    }

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
  uid <- extract_spss_uid(notes)
  notes <- notes[!grepl("^(?:Requirement\\s+)?UID\\s*[:=]", notes, ignore.case = TRUE, perl = TRUE)]
  label <- make_label(variable, notes)
  list(
    level = level,
    variable = variable,
    uid = uid,
    label = label,
    derivation = derivation,
    notes = paste(unique(notes), collapse = "; "),
    "spss line number(s)" = line_number,
    "spss command" = command
  )
}

parse_simple_range_flag <- function(block_lines) {
  opening <- regmatches(
    block_lines[[1L]],
    regexec(
      "^\\s*DO\\s+IF\\s+RANGE\\s*\\(\\s*([A-Za-z0-9_.]+)\\s*,\\s*(-?[0-9]+(?:\\.[0-9]+)?)\\s*,\\s*(-?[0-9]+(?:\\.[0-9]+)?)\\s*\\)\\s*\\.?\\s*$",
      block_lines[[1L]],
      ignore.case = TRUE,
      perl = TRUE
    )
  )[[1L]]

  if (length(opening) < 4L) {
    return(NULL)
  }

  else_line <- which(grepl("^\\s*ELSE\\s*\\.?\\s*$", block_lines, ignore.case = TRUE, perl = TRUE))
  end_line <- which(grepl("^\\s*END\\s+IF\\s*\\.?\\s*$", block_lines, ignore.case = TRUE, perl = TRUE))
  compute_lines <- grep("^\\s*COMPUTE\\b", block_lines, ignore.case = TRUE, perl = TRUE)

  if (length(else_line) != 1L || length(end_line) != 1L ||
      length(compute_lines) != 2L ||
      !(compute_lines[[1L]] < else_line && else_line < compute_lines[[2L]] &&
        compute_lines[[2L]] < end_line)) {
    return(NULL)
  }

  assignments <- lapply(compute_lines, function(line_number) {
    matched <- regmatches(
      block_lines[[line_number]],
      regexec(
        "^\\s*COMPUTE\\s+([A-Za-z0-9_.]+)\\s*=\\s*(-?[0-9]+(?:\\.[0-9]+)?)\\s*\\.?\\s*$",
        block_lines[[line_number]],
        ignore.case = TRUE,
        perl = TRUE
      )
    )[[1L]]
    if (length(matched) < 3L) NULL else matched
  })

  if (any(vapply(assignments, is.null, logical(1L))) ||
      !identical(tolower(assignments[[1L]][[2L]]), tolower(assignments[[2L]][[2L]])) ||
      !identical(as.numeric(assignments[[1L]][[3L]]), 1) ||
      !identical(as.numeric(assignments[[2L]][[3L]]), 0)) {
    return(NULL)
  }

  list(
    variable = assignments[[1L]][[2L]],
    derivation = paste0(
      "IF ", opening[[2L]], " >= ", opening[[3L]], " AND ",
      opening[[2L]], " <= ", opening[[4L]],
      " THEN ", assignments[[1L]][[2L]], " = 1 ELSE ",
      assignments[[2L]][[2L]], " = 0"
    )
  )
}

extract_spss_uid <- function(notes) {
  uid_lines <- grep(
    "^(?:Requirement\\s+)?UID\\s*[:=]",
    notes,
    value = TRUE,
    ignore.case = TRUE,
    perl = TRUE
  )
  if (length(uid_lines) == 0L) {
    return(NA_character_)
  }

  matched <- regmatches(
    uid_lines[[1L]],
    regexec(
      "^(?:Requirement\\s+)?UID\\s*[:=]\\s*([A-Za-z0-9_.-]+)\\s*$",
      uid_lines[[1L]],
      ignore.case = TRUE,
      perl = TRUE
    )
  )[[1L]]
  if (length(matched) < 2L) {
    stop("Invalid SPSS requirement UID comment: ", uid_lines[[1L]], call. = FALSE)
  }
  matched[[2L]]
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
