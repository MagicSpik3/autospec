#' Flag rows where a value is above a threshold
#'
#' 1 above the threshold, 0 otherwise, missing where the value is missing.
#' Stops if the column holds -8 or -9, unless `sentinels` says otherwise.
#'
#' @param df A data frame.
#' @param value_col Column to test.
#' @param new_col Column to create.
#' @param threshold Value that must be exceeded.
#' @param sentinels What to do with -8/-9 codes (decision D1): `"stop"`,
#'   `"missing"` to treat them as `NA`, or `"zero"` to treat them as 0.
#'
#' @return `df` with `new_col` added.
#' @export
#'
#' @examples
#' dv_flag_positive(data.frame(value = c(0, 250, NA)), "value", "has_value")
dv_flag_positive <- function(df, value_col, new_col, threshold = 0, sentinels = "stop") {
  verb <- "dv_flag_positive"
  check_verb_basics(df, new_col, verb)
  check_columns_exist(df, value_col, verb)
  check_columns_numeric(df, value_col, verb)
  check_single_number(threshold, "threshold", verb)

  values <- sentinel_safe_values(df, value_col, verb, sentinels)
  flag <- data.table::fifelse(values[[1L]] > threshold, 1, 0)
  df[[new_col]] <- flag

  report_verb(new_col, paste0(
    "1 where ", value_col, " > ", threshold, " (", format_count(sum(flag %in% 1)),
    " rows), 0 elsewhere", sentinel_note(values, sentinels)
  ))

  df
}
