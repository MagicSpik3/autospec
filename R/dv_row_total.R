#' Add columns together within each row
#'
#' Stops if any column holds -8 or -9, unless `sentinels` says otherwise.
#'
#' @param df A data frame.
#' @param value_cols Columns to add.
#' @param new_col Column to create.
#' @param na_as_zero Whether a missing value counts as zero (decision D3).
#'   When `FALSE`, one missing value makes the total missing.
#' @param sentinels What to do with -8/-9 codes (decision D1): `"stop"`,
#'   `"missing"` to treat them as `NA`, or `"zero"` to treat them as 0.
#'
#' @return `df` with `new_col` added.
#' @export
#'
#' @examples
#' df <- data.frame(a = c(1, NA), b = c(2, 3))
#' dv_row_total(df, c("a", "b"), "total")
dv_row_total <- function(df, value_cols, new_col, na_as_zero = TRUE, sentinels = "stop") {
  verb <- "dv_row_total"
  check_verb_basics(df, new_col, verb)
  check_columns_exist(df, value_cols, verb)
  check_columns_numeric(df, value_cols, verb)
  check_true_or_false(na_as_zero, "na_as_zero", verb)

  columns <- sentinel_safe_values(df, value_cols, verb, sentinels)
  note <- sentinel_note(columns, sentinels)

  if (na_as_zero) {
    columns <- lapply(columns, function(values) {
      values[is.na(values)] <- 0
      values
    })
  }

  df[[new_col]] <- Reduce(`+`, columns)

  report_verb(new_col, paste0(
    "row total of ", length(value_cols), " column", if (length(value_cols) > 1L) "s",
    " (", paste(utils::head(value_cols, 4L), collapse = ", "),
    if (length(value_cols) > 4L) ", ...", ")",
    if (na_as_zero) ", missing counted as zero" else ", missing if any column is missing",
    note
  ))

  df
}
