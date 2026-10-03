#' Subtract one column from another
#'
#' Stops if either column holds -8 or -9, unless `sentinels` says otherwise.
#'
#' @param df A data frame.
#' @param minuend_col Column to subtract from.
#' @param subtrahend_col Column to subtract.
#' @param new_col Column to create.
#' @param sentinels What to do with -8/-9 codes (decision D1): `"stop"`,
#'   `"missing"` to treat them as `NA`, or `"zero"` to treat them as 0.
#'
#' @return `df` with `new_col` added.
#' @export
#'
#' @examples
#' df <- data.frame(value = c(200000, 150000), debt = c(50000, 0))
#' dv_difference(df, "value", "debt", "equity")
dv_difference <- function(df, minuend_col, subtrahend_col, new_col, sentinels = "stop") {
  verb <- "dv_difference"
  check_verb_basics(df, new_col, verb)
  check_columns_exist(df, c(minuend_col, subtrahend_col), verb)
  check_columns_numeric(df, c(minuend_col, subtrahend_col), verb)

  values <- sentinel_safe_values(df, c(minuend_col, subtrahend_col), verb, sentinels)
  df[[new_col]] <- values[[1L]] - values[[2L]]

  report_verb(new_col, paste0(minuend_col, " minus ", subtrahend_col, sentinel_note(values, sentinels)))

  df
}
