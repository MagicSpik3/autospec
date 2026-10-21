#' Spec issues: what is wrong with, or missing from, the spec itself
#'
#' The plan keeps two kinds of message apart. `spec_issues` holds defects in
#' the spec, for the spec owner to fix: omissions such as a missing ELSE,
#' errors such as an unknown variable, and typos the parser repaired. `notes`
#' holds prompts for the reviewer about how the spec was drafted into code.
#' @name spec_issues
#' @keywords internal
#' @noRd
NULL


#' The drafting messages that are spec issues, by code
#'
#' Each pattern matches the start of a message written by the parser,
#' match_verb() or build_dv_plan(); test-spec_issues.R pins each one.
#' @keywords internal
#' @noRd
spec_issue_patterns <- function() {
  c(
    no_else = "^no else branch",
    missing_value = "^spec gives no value",
    no_derivation = "^no derivation text$",
    no_dv_name = "^no variable name against this derivation$",
    branch_without_name = "^value with no variable name inside a branch$",
    unknown_variable = "^unknown variable ",
    duplicate_dv = "^also defined on .* with different text",
    unparsed = "^could not parse",
    typo = "^dash character used as minus$",
    typo = "^IF condition wrapped in brackets$",
    typo = "^OTHERWISE written in brackets$",
    typo = "^unclosed bracket before IF removed$",
    typo = "^unmatched closing bracket removed$",
    typo = "^numbered range written in square brackets$"
  )
}


#' The spec issue code of each message, or NA for a message that is not one
#' @keywords internal
#' @noRd
spec_issue_codes <- function(messages) {
  patterns <- spec_issue_patterns()

  vapply(messages, function(message) {
    hit <- which(vapply(patterns, grepl, logical(1L), x = message, ignore.case = TRUE, perl = TRUE))
    if (length(hit) == 0L) NA_character_ else names(patterns)[[hit[[1L]]]]
  }, character(1L), USE.NAMES = FALSE)
}


#' Whether a derivation has an IF with no ELSE before the DV has any value
#'
#' A DV is a column, so every row needs a value. An IF with no ELSE leaves some
#' rows without one, unless an earlier statement already set the DV. Text that
#' cannot be parsed is reported as unparsed instead, so is not counted here.
#' @keywords internal
#' @noRd
missing_else <- function(text) {
  if (!is.character(text) || length(text) != 1L || is.na(text)) {
    return(FALSE)
  }

  analysis <- tryCatch(analyse_derivation(text), error = function(problem) NULL)

  if (is.null(analysis) || !identical(analysis$kind, "parsed")) {
    return(FALSE)
  }

  set <- FALSE

  for (statement in analysis$statements) {
    if (identical(statement$type, "assign")) {
      set <- TRUE
    } else if (identical(statement$type, "if")) {
      if (!is.null(statement$otherwise)) {
        set <- TRUE
      } else if (!set) {
        return(TRUE)
      }
    }
  }

  FALSE
}


#' Move the spec issues out of a plan's notes into its spec_issues column
#'
#' Each issue is written `[code] message`, separated by `; `. A missing ELSE
#' is found from the spec text, so rows no verb fits are flagged too. Where the
#' default ELSE is used, `implied_else` records it: on a row no verb fits, for
#' whoever writes it, and on a row whose drafted verb takes `otherwise`. A row
#' with a spec issue is never `auto`.
#'
#' @param names_checked Whether the data's names were known. Without them, an
#'   input that is not a DV may still be in the data, so only a near miss of a
#'   DV name ("did you mean ...") is an unknown variable issue.
#' @param default_else The value a DV takes where its spec gives no ELSE.
#' @keywords internal
#' @noRd
add_spec_issues <- function(plan, names_checked = TRUE, default_else = 0) {
  else_value <- format_else_value(parse_default_else(default_else))
  notes <- as.character(plan$notes)
  instructions <- as.character(plan$instructions)
  verbs <- as.character(plan$verb)
  args <- as.character(plan$args)
  issues <- rep(NA_character_, nrow(plan))
  kept <- rep(NA_character_, nrow(plan))
  implied <- rep(NA_character_, nrow(plan))

  for (index in seq_len(nrow(plan))) {
    parts <- if (is_blank(notes[[index]])) character() else strsplit(notes[[index]], "; ", fixed = TRUE)[[1L]]
    codes <- spec_issue_codes(parts)

    if (!names_checked) {
      codes[codes %in% "unknown_variable" & !grepl("did you mean", parts, fixed = TRUE)] <- NA_character_
    }

    is_issue <- !is.na(codes)
    found <- if (any(is_issue)) paste0("[", codes[is_issue], "] ", parts[is_issue]) else character()

    if (!"no_else" %in% codes && missing_else(instructions[[index]])) {
      found <- c(paste0("[no_else] no ELSE branch: ", else_value, " where no branch applies (default_else in the config)"), found)
    }

    takes_default <- is_blank(verbs[[index]]) || grepl("otherwise = ", args[[index]], fixed = TRUE)

    if (any(startsWith(found, "[no_else]")) && takes_default) {
      implied[[index]] <- else_value
    }

    rest <- parts[!is_issue]
    issues[[index]] <- if (length(found) > 0L) paste(found, collapse = "; ") else NA_character_
    kept[[index]] <- if (length(rest) > 0L) paste(rest, collapse = "; ") else NA_character_
  }

  plan[["notes"]] <- kept
  plan[["spec_issues"]] <- issues
  plan[["implied_else"]] <- implied
  plan[["status"]][!is.na(issues) & plan$status %in% "auto"] <- "needs_review"

  plan
}


#' Whether each plan row has a spec issue
#' @keywords internal
#' @noRd
has_spec_issues <- function(plan) {
  if (!"spec_issues" %in% names(plan)) {
    return(rep(FALSE, nrow(plan)))
  }

  !is_blank(plan$spec_issues)
}


#' One row per spec issue, with where it is in the spec, for the spec owner
#' @keywords internal
#' @noRd
spec_issue_table <- function(plan) {
  empty <- data.frame(
    uid = character(), dv = character(), file_name = character(), sheet_name = character(),
    excel_row = integer(), code = character(), issue = character(), instructions = character(),
    stringsAsFactors = FALSE
  )

  rows <- lapply(which(has_spec_issues(plan)), function(index) {
    parts <- strsplit(plan$spec_issues[[index]], "; ", fixed = TRUE)[[1L]]
    parts <- parts[grepl("^\\[[^]]+\\]", parts)]

    if (length(parts) == 0L) {
      return(NULL)
    }

    data.frame(
      uid = as.character(plan$uid[[index]]),
      dv = as.character(plan$dv[[index]]),
      file_name = as.character(plan$file_name[[index]]),
      sheet_name = as.character(plan$sheet_name[[index]]),
      excel_row = as.integer(plan$excel_row[[index]]),
      code = sub("^\\[([^]]+)\\].*$", "\\1", parts),
      issue = sub("^\\[[^]]+\\]\\s*", "", parts),
      instructions = as.character(plan$instructions[[index]]),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, c(list(empty), rows))
}


#' Write the spec issues for the spec owner, or remove an old list when there are none
#' @keywords internal
#' @noRd
write_spec_issues_file <- function(plan, path) {
  table <- spec_issue_table(plan)

  if (nrow(table) == 0L) {
    if (file.exists(path)) {
      file.remove(path)
    }

    cli::cli_alert_success("No spec issues found.")
    return(invisible(NULL))
  }

  data.table::fwrite(table, path)
  row_count <- length(unique(paste(table$sheet_name, table$excel_row, table$dv)))

  cli::cli_alert_warning(
    "{nrow(table)} spec issue{?s} on {row_count} row{?s}: omissions, errors or typos in the spec. {.file {basename(path)}} lists them for the spec owner."
  )

  invisible(path)
}
