#' Expand numbered ranges in an IF condition
#'
#' @param x A single pseudocode IF statement.
#'
#' @return The IF statement with ranged comparisons expanded.
#'
#' @keywords internal
#' @noRd
expand_numbered_conditions <- function(x) {
  comparison_pattern <- paste0(
    # Variable prefix
    "([A-Za-z][A-Za-z0-9_.]*",
    # Numbered range
    "\\([0-9]+\\s*-\\s*[0-9]+\\)",
    # Optional variable suffix
    "[A-Za-z0-9_.]*)",
    # Comparison operator
    "\\s*(=|==|!=|<=|>=|<|>)\\s*",
    # Simple comparison value
    "([^\\s,.)]+)"
  )

  repeat {
    location <- regexpr(
      pattern = comparison_pattern,
      text = x,
      perl = TRUE
    )

    if (location[[1L]] == -1L) {
      break
    }

    matched_comparison <- regmatches(x, location)

    parts <- regexec(
      pattern = comparison_pattern,
      text = matched_comparison,
      perl = TRUE
    )

    captures <- regmatches(matched_comparison, parts)[[1L]]

    variable <- captures[[2L]]
    operator <- captures[[3L]]
    value <- captures[[4L]]

    expanded <- expand_ranged_comparison(
      variable = variable,
      operator = operator,
      value = value
    )

    regmatches(x, location) <- expanded
  }

  x
}
