#' Replace sentinel codes with a value
#'
#' The one place -8/-9 are neutralised before arithmetic (decision D1).
#'
#' @param df A data frame.
#' @param value_col Column to clean.
#' @param new_col Column to create. May be `value_col` to clean in place.
#' @param codes Codes to replace.
#' @param replacement Value to put in their place: a number or `NA`.
#'
#' @return `df` with `new_col` added.
#' @export
#'
#' @examples
#' dv_clear_sentinels(data.frame(x = c(10, -9, -8)), "x", "x_clean")
dv_clear_sentinels <- function(df, value_col, new_col, codes = c(-8, -9),
                               replacement = 0) {
  verb <- "dv_clear_sentinels"
  check_verb_basics(df, new_col, verb)
  check_columns_exist(df, value_col, verb)
  check_columns_numeric(df, value_col, verb)

  if (!is.numeric(codes) || length(codes) == 0L || anyNA(codes)) {
    verb_stop(verb, "`codes` must be one or more numbers.")
  }

  check_single_number(replacement, "replacement", verb, allow_na = TRUE)

  values <- as.numeric(df[[value_col]])
  cleared <- values %in% codes
  values[cleared] <- as.numeric(replacement)

  df[[new_col]] <- values

  report_verb(new_col, paste0(
    value_col, " with ", format_count(sum(cleared)), " rows of ",
    paste(codes, collapse = "/"), " set to ", replacement
  ))

  df
}
