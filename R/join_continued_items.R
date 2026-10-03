#' @title join_continued_items
#'
#' @description
#'
#' Join pseudocode items continued across multiple rows
#'
#' Combines pseudocode items when a later item begins with a recognised
#' continuation keyword, such as `OTHERWISE`.
#'
#' @param x A character vector containing pseudocode items.
#' @param continuation_pattern A regular expression identifying items that
#' continue the preceding item.
#' @param separator A character string inserted between joined items.
#' @param drop_na Whether to remove missing items before joining.
#'
#' @return A character vector containing reconstructed pseudocode items.
#'
#' @keywords internal
#' @noRd
#'
#' @examples
#' x <- c(
#'   "IF x = 2 THEN\n    y = 1",
#'   "OTHERWISE\n    y = 0"
#' )
#'
#' join_continued_items(x)
#'
#' @keywords internal
#' @noRd
join_continued_items <- function(
    x,
    continuation_pattern = "^\\s*(OTHERWISE|ELSE(?:\\s+IF)?)\\b",
    separator = "\n",
    drop_na = TRUE
) {
  if (!is.character(x)) {
    cli::cli_abort("{.arg x} must be a character vector.")
  }

  if (!is.character(continuation_pattern) ||
      length(continuation_pattern) != 1L ||
      is.na(continuation_pattern)) {
    cli::cli_abort(
      "{.arg continuation_pattern} must be a single non-missing string."
    )
  }

  if (!is.character(separator) ||
      length(separator) != 1L ||
      is.na(separator)) {
    cli::cli_abort(
      "{.arg separator} must be a single non-missing string."
    )
  }

  if (!is.logical(drop_na) ||
      length(drop_na) != 1L ||
      is.na(drop_na)) {
    cli::cli_abort(
      "{.arg drop_na} must be a single non-missing logical value."
    )
  }

  if (drop_na) {
    x <- x[!is.na(x)]
  }

  if (length(x) == 0L) {
    return(character())
  }

  is_continuation <- !is.na(x) &
    grepl(
      continuation_pattern,
      x,
      ignore.case = TRUE,
      perl = TRUE
    )

  # A continuation cannot attach to a missing item, which is only reachable
  # when missing items have been retained.
  is_continuation <- is_continuation &
    c(FALSE, !is.na(utils::head(x, -1L)))

  groups <- continuation_group(x, is_continuation = is_continuation)

  # split() orders its result by the sorted character form of the grouping
  # values, so the levels are set explicitly to preserve the input order.
  items <- split(x, factor(groups, levels = unique(groups)))

  unname(vapply(
    items,
    FUN = function(group) {
      # Joining a single item would coerce a missing value to the string
      # "NA", so lone items are returned untouched.
      if (length(group) == 1L) {
        return(group)
      }

      paste(group, collapse = separator)
    },
    FUN.VALUE = character(1L)
  ))
}
