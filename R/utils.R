#' Excel column letters
#'
#' Generates the Excel column reference for each of the first `n` columns,
#' for example `A`, `B`, ... `Z`, `AA`, `AB`.
#'
#' @param n Number of columns to name.
#'
#' @return A character vector of length `n`.
#'
#' @keywords internal
#' @noRd
excel_letters <- function(n) {
  vapply(
    seq_len(n),
    FUN = function(position) {
      reference <- character()

      while (position > 0L) {
        remainder <- (position - 1L) %% 26L
        reference <- c(LETTERS[[remainder + 1L]], reference)
        position <- (position - 1L) %/% 26L
      }

      paste(reference, collapse = "")
    },
    FUN.VALUE = character(1L),
    USE.NAMES = FALSE
  )
}


#' Test whether spreadsheet cells are blank
#'
#' A cell counts as blank when it is missing or contains only whitespace.
#' Spreadsheet cells frequently contain a single space rather than a true
#' empty value, so trimming is required before testing.
#'
#' @param x A character vector.
#'
#' @return A logical vector the same length as `x`.
#'
#' @keywords internal
#' @noRd
is_blank <- function(x) {
  is.na(x) | !nzchar(trimws(x))
}


#' Compare header labels
#'
#' Normalises a spreadsheet header label so that differences in case,
#' padding and internal whitespace do not prevent a match.
#'
#' @param x A character vector of header labels.
#'
#' @return A character vector of normalised labels.
#'
#' @keywords internal
#' @noRd
normalise_header <- function(x) {
  x <- ifelse(is.na(x), "", x)

  tolower(gsub("\\s+", " ", trimws(x)))
}
