#' The value a DV takes where its spec gives no ELSE
#'
#' A DV is a column, so every row needs a value. Where the spec has an IF with
#' no ELSE, the DV takes `default_else` from the config: 0 (the default, as the
#' published DVs are), -9 (the legacy code for missing) or NA. The plan records
#' the value in `implied_else` on each row where it was used.
#' @name default_else
#' @keywords internal
#' @noRd
NULL


#' Read a default_else setting: 0, -9 or NA, written as a number or as text
#' @keywords internal
#' @noRd
parse_default_else <- function(value) {
  if (length(value) == 1L && (is.numeric(value) || is.character(value) || is.logical(value))) {
    if (is.na(value) || toupper(trimws(as.character(value))) == "NA") {
      return(NA_real_)
    }

    number <- suppressWarnings(as.numeric(trimws(as.character(value))))

    if (!is.na(number) && number %in% c(0, -9)) {
      return(number)
    }
  }

  cli::cli_alert_danger(
    "{.field default_else} must be 0, -9 or NA: the value a DV takes where its spec gives no ELSE."
  )
  stop("The default_else setting must be 0, -9 or NA.", call. = FALSE)
}


#' A default_else value as the plan writes it: "0", "-9" or "NA"
#' @keywords internal
#' @noRd
format_else_value <- function(value) {
  if (is.na(value)) "NA" else format_number(value)
}
