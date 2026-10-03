#' Validate numbered ranges
#'
#' @param ranges A data frame returned by `find_number_ranges()`.
#' @param value The original pseudocode, used in an error message.
#'
#' @return `ranges`, invisibly.
#'
#' @keywords internal
#' @noRd
validate_number_ranges <- function(ranges, value) {
  descending <- ranges$lower > ranges$upper

  if (any(descending)) {
    invalid <- ranges$match[which(descending)[[1L]]]

    stop(
      "Invalid descending numbered range ",
      sQuote(invalid),
      " in derivation: ",
      sQuote(value),
      call. = FALSE
    )
  }

  invisible(ranges)
}


#' Replace a particular numbered range with an index
#'
#' @param x A character value.
#' @param range_text The literal range to replace, such as `"(1-3)"`.
#' @param index The replacement index.
#'
#' @return A character value.
#'
#' @keywords internal
#' @noRd
replace_number_range <- function(x, range_text, index) {
  gsub(
    pattern = range_text,
    replacement = as.character(index),
    x = x,
    fixed = TRUE
  )
}


#' Expand a ranged comparison
#'
#' Converts an expression such as `A(1-3) = 0` to:
#'
#' `A1 = 0 AND A2 = 0 AND A3 = 0`
#'
#' @param variable A variable containing one numbered range.
#' @param operator A comparison operator.
#' @param value The value on the right-hand side.
#'
#' @return An expanded comparison.
#'
#' @keywords internal
#' @noRd
expand_ranged_comparison <- function(variable, operator, value) {
  ranges <- find_number_ranges(variable)

  if (nrow(ranges) != 1L) {
    stop(
      "A ranged comparison must contain exactly one numbered range: ",
      sQuote(variable),
      call. = FALSE
    )
  }

  validate_number_ranges(ranges, variable)

  lower <- ranges$lower[[1L]]
  upper <- ranges$upper[[1L]]
  range_text <- ranges$match[[1L]]

  comparisons <- vapply(
    seq.int(lower, upper),
    FUN = function(index) {
      expanded_variable <- replace_number_range(
        variable,
        range_text,
        index
      )

      paste(expanded_variable, operator, value)
    },
    FUN.VALUE = character(1L),
    USE.NAMES = FALSE
  )

  paste0(
    "(",
    paste(comparisons, collapse = " AND "),
    ")"
  )
}
