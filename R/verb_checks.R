#' Stop a verb with a message a researcher can act on
#' @keywords internal
#' @noRd
verb_stop <- function(verb, message) {
  text <- paste0(verb, "(): ", message)

  # Inside run_dv_suite() the orchestrator prints the failure itself.
  if (!isTRUE(getOption("autospec.in_suite"))) {
    cli::cli_alert_danger(text)
  }

  stop(text, call. = FALSE)
}


#' Print the one line a verb reports about what it built
#' @keywords internal
#' @noRd
report_verb <- function(new_col, text) {
  if (!isTRUE(getOption("autospec.quiet"))) {
    cli::cli_alert_success("{new_col}: {text}")
  }

  invisible(NULL)
}


#' Format a count for console messages
#' @keywords internal
#' @noRd
format_count <- function(count) {
  format(count, big.mark = ",", scientific = FALSE, trim = TRUE)
}


#' Describe a condition argument in words for console messages
#' @keywords internal
#' @noRd
describe_condition <- function(condition_code) {
  text <- paste(trimws(deparse(condition_code)), collapse = " ")

  if (!is.language(condition_code) || nchar(text) > 80L) "the condition" else text
}


#' Sentinel codes: -8 banded or refused, -9 missing
#' @keywords internal
#' @noRd
sentinel_codes <- function() {
  c(-8, -9)
}


#' @keywords internal
#' @noRd
check_verb_basics <- function(df, new_col, verb) {
  if (!is.data.frame(df)) {
    verb_stop(verb, "`df` must be a data frame.")
  }

  if (!is.character(new_col) || length(new_col) != 1L || is.na(new_col) ||
      !nzchar(new_col)) {
    verb_stop(verb, "`new_col` must be a single column name.")
  }

  invisible(NULL)
}


#' @keywords internal
#' @noRd
check_columns_exist <- function(df, columns, verb) {
  if (!is.character(columns) || length(columns) == 0L || anyNA(columns)) {
    verb_stop(verb, "column names must be given as a character vector.")
  }

  missing_columns <- setdiff(columns, names(df))

  if (length(missing_columns) > 0L) {
    verb_stop(verb, paste0(
      "column(s) not found in the data: ",
      paste(missing_columns, collapse = ", ")
    ))
  }

  invisible(NULL)
}


#' Labelled SPSS numerics count as numeric
#' @keywords internal
#' @noRd
check_columns_numeric <- function(df, columns, verb) {
  not_numeric <- columns[!vapply(
    columns,
    FUN = function(column) is.numeric(df[[column]]) || is.logical(df[[column]]),
    FUN.VALUE = logical(1L)
  )]

  if (length(not_numeric) > 0L) {
    verb_stop(verb, paste0(
      "column(s) must be numeric: ",
      paste(not_numeric, collapse = ", ")
    ))
  }

  invisible(NULL)
}


#' Stop if -8/-9 would reach arithmetic, optionally only in some rows
#' @keywords internal
#' @noRd
check_no_sentinels <- function(df, columns, verb, rows = NULL) {
  codes <- sentinel_codes()

  counts <- vapply(
    columns,
    FUN = function(column) {
      values <- df[[column]]

      if (!is.null(rows)) {
        values <- values[rows %in% TRUE]
      }

      sum(values %in% codes)
    },
    FUN.VALUE = integer(1L)
  )

  if (any(counts > 0L)) {
    affected <- counts[counts > 0L]

    verb_stop(verb, paste0(
      "sentinel codes (", paste(codes, collapse = ", "), ") found in ",
      paste0(names(affected), " (", affected, " rows)", collapse = ", "),
      ". Clear them first with dv_clear_sentinels()."
    ))
  }

  invisible(NULL)
}


#' Numeric values of columns with -8/-9 handled as the `sentinels` setting says
#'
#' "stop" refuses to run, "missing" treats them as NA, "zero" as 0. The list
#' carries the number replaced as the attribute "replaced".
#' @keywords internal
#' @noRd
sentinel_safe_values <- function(df, columns, verb, sentinels, rows = NULL) {
  check_sentinels_setting(sentinels, verb, allow_keep = FALSE)

  if (sentinels == "stop") {
    check_no_sentinels(df, columns, verb, rows = rows)
  }

  replacement <- if (sentinels == "zero") 0 else NA_real_
  used_rows <- if (is.null(rows)) TRUE else rows %in% TRUE

  values <- lapply(columns, function(column) {
    column_values <- as.numeric(df[[column]])
    column_values[column_values %in% sentinel_codes()] <- replacement
    column_values
  })

  replaced <- sum(vapply(columns, function(column) {
    sum(df[[column]] %in% sentinel_codes() & used_rows)
  }, integer(1L)))

  attr(values, "replaced") <- replaced
  values
}


#' Check a sentinels setting; "keep" is only for verbs that copy or look up values
#' @keywords internal
#' @noRd
check_sentinels_setting <- function(sentinels, verb, allow_keep) {
  allowed <- c(if (allow_keep) "keep", "stop", "missing", "zero")

  if (!is.character(sentinels) || length(sentinels) != 1L || !sentinels %in% allowed) {
    verb_stop(verb, paste0(
      "`sentinels` must be ", paste0("\"", allowed, "\"", collapse = ", "),
      if (!allow_keep) " (\"keep\" only works for verbs that copy values)",
      ". Check the setting in settings.R."
    ))
  }

  invisible(NULL)
}


#' Apply the sentinels setting to values a verb has copied or looked up
#'
#' `sentinel_rows` marks the rows whose input held -8 or -9. "keep" leaves the
#' result as it is; "zero" and "missing" put 0 or `NA` there; "stop" refuses.
#' @return A list: `values` and `replaced`, the number of rows changed.
#' @keywords internal
#' @noRd
apply_sentinels <- function(values, sentinel_rows, sentinels, verb, source_text) {
  check_sentinels_setting(sentinels, verb, allow_keep = TRUE)
  count <- sum(sentinel_rows)

  if (sentinels == "stop" && count > 0L) {
    verb_stop(verb, paste0(
      "sentinel codes (-8, -9) found in ", source_text, " (", count,
      " rows). Choose what to do with them with the sentinels setting."
    ))
  }

  if (sentinels %in% c("zero", "missing")) {
    values <- as.numeric(values)
    values[sentinel_rows] <- if (sentinels == "zero") 0 else NA_real_
  }

  list(values = values, replaced = if (sentinels %in% c("zero", "missing")) count else 0L)
}


#' The part of a verb report saying how many -8/-9 values were replaced
#' @keywords internal
#' @noRd
sentinel_note <- function(values, sentinels) {
  replaced <- attr(values, "replaced")

  if (is.null(replaced) || replaced == 0L) {
    return("")
  }

  paste0("; ", format_count(replaced), " -8/-9 values treated as ",
         if (sentinels == "zero") "0" else "missing")
}


#' @keywords internal
#' @noRd
check_condition <- function(df, condition, verb) {
  if (!is.logical(condition)) {
    verb_stop(verb, "`condition` must be a logical vector, such as `df$x > 0`.")
  }

  if (length(condition) != nrow(df)) {
    verb_stop(verb, paste0(
      "`condition` has ", length(condition), " values but the data has ",
      nrow(df), " rows. Check every column it uses exists."
    ))
  }

  invisible(NULL)
}


#' @keywords internal
#' @noRd
check_single_number <- function(value, name, verb, allow_na = FALSE) {
  is_na <- length(value) == 1L && is.na(value)

  valid <- length(value) == 1L &&
    ((is.numeric(value) && !is.na(value)) || (allow_na && is_na))

  if (!valid) {
    verb_stop(verb, paste0(
      "`", name, "` must be a single number",
      if (allow_na) " or NA" else "", "."
    ))
  }

  invisible(NULL)
}


#' @keywords internal
#' @noRd
check_true_or_false <- function(value, name, verb) {
  if (!is.logical(value) || length(value) != 1L || is.na(value)) {
    verb_stop(verb, paste0("`", name, "` must be TRUE or FALSE."))
  }

  invisible(NULL)
}


#' @keywords internal
#' @noRd
check_household_id <- function(df, by, verb) {
  if (!is.character(by) || length(by) != 1L || is.na(by)) {
    verb_stop(verb, "`by` must be the name of the household identifier column.")
  }

  check_columns_exist(df, by, verb)

  if (anyNA(df[[by]])) {
    verb_stop(verb, paste0(
      "the household identifier `", by, "` is missing on ",
      sum(is.na(df[[by]])), " rows."
    ))
  }

  invisible(NULL)
}
