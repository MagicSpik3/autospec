#' Group pseudocode items with their continuations
#'
#' Assigns a group number to each item, where an item that continues the
#' preceding one — because it begins with `OTHERWISE`, `ELSE` or `ELSE IF` —
#' shares the group of the item it continues.
#'
#' An item that looks like a continuation but has nothing before it starts a
#' group of its own, so leading continuations are left as they were found.
#'
#' @param x A character vector of pseudocode items.
#' @param continuation_pattern A regular expression identifying continuations.
#' @param is_continuation An optional logical vector overriding the pattern
#'   match, used when the spreadsheet supplies further evidence — a genuine
#'   continuation row carries no variable name of its own.
#'
#' @return An integer vector of group numbers, the same length as `x`.
#'
#' @keywords internal
#' @noRd
continuation_group <- function(
    x,
    continuation_pattern = "^\\s*(OTHERWISE|ELSE(?:\\s+IF)?)\\b",
    is_continuation = NULL
) {
  if (length(x) == 0L) {
    return(integer())
  }

  if (is.null(is_continuation)) {
    is_continuation <- !is.na(x) &
      grepl(
        continuation_pattern,
        x,
        ignore.case = TRUE,
        perl = TRUE
      )
  }

  # The first item can never continue anything.
  is_continuation[[1L]] <- FALSE

  cumsum(!is_continuation)
}
