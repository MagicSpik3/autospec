#' Copy a value where a condition holds
#'
#' `otherwise` where the condition is `FALSE`, missing where it is missing.
#' Stops if a copied value is -8 or -9, unless `sentinels` says otherwise; rows
#' not copied are not checked.
#'
#' @param df A data frame.
#' @param condition A logical vector with one value per row.
#' @param value_col Column to copy where the condition holds.
#' @param new_col Column to create.
#' @param otherwise Value where the condition is `FALSE`: a number, or `NA`
#'   where the spec gives no else branch (decision D5).
#' @param sentinels What to do with -8/-9 codes (decision D1): `"stop"`,
#'   `"missing"` to treat them as `NA`, or `"zero"` to treat them as 0.
#'
#' @return `df` with `new_col` added.
#' @export
#'
#' @examples
#' df <- data.frame(DLType1R9_i = c(2, 3), DSLamt1R9_i = c(9000, 4000))
#' dv_keep_if(df, df$DLType1R9_i == 2, "DSLamt1R9_i", "SLNS1R9")
dv_keep_if <- function(df, condition, value_col, new_col, otherwise = 0, sentinels = "stop") {
  verb <- "dv_keep_if"
  condition_text <- describe_condition(substitute(condition))
  check_verb_basics(df, new_col, verb)
  check_condition(df, condition, verb)
  check_columns_exist(df, value_col, verb)
  check_columns_numeric(df, value_col, verb)
  check_single_number(otherwise, "otherwise", verb, allow_na = TRUE)

  values <- sentinel_safe_values(df, value_col, verb, sentinels, rows = condition)

  df[[new_col]] <- data.table::fifelse(
    condition,
    values[[1L]],
    as.numeric(otherwise)
  )

  report_verb(new_col, paste0(
    value_col, " where ", condition_text, " (", format_count(sum(condition %in% TRUE)),
    " rows), ", otherwise, " elsewhere", sentinel_note(values, sentinels)
  ))

  df
}
