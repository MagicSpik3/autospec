#' Locate numbered ranges in pseudocode
#'
#' @param x A single character value.
#'
#' @return A data frame containing the matched range text and bounds.
#'
#' @keywords internal
#' @noRd
find_number_ranges <- function(x) {
  range_pattern <- "\\(([0-9]+)\\s*-\\s*([0-9]+)\\)"

  locations <- gregexpr(
    pattern = range_pattern,
    text = x,
    perl = TRUE
  )

  matches <- regmatches(x, locations)[[1L]]

  if (identical(matches, character(0))) {
    return(data.frame(
      match = character(),
      lower = integer(),
      upper = integer()
    ))
  }

  data.frame(
    match = matches,
    lower = as.integer(
      sub(range_pattern, "\\1", matches, perl = TRUE)
    ),
    upper = as.integer(
      sub(range_pattern, "\\2", matches, perl = TRUE)
    ),
    stringsAsFactors = FALSE
  )
}
