#' Divide one column by another where the denominator is above zero
#'
#' `otherwise` where the denominator is zero or negative, missing where either
#' value is missing. Stops if either column holds -8 or -9, unless `sentinels`
#' says otherwise.
#'
#' @param df A data frame.
#' @param numerator_col Numerator column.
#' @param denominator_col Denominator column.
#' @param new_col Column to create.
#' @param otherwise Value where the denominator is not above zero.
#' @param sentinels What to do with -8/-9 codes (decision D1): `"stop"`,
#'   `"missing"` to treat them as `NA`, or `"zero"` to treat them as 0.
#'
#' @return `df` with `new_col` added.
#' @export
#'
#' @examples
#' df <- data.frame(debt = c(5000, 100), income = c(20000, 0))
#' dv_ratio(df, "debt", "income", "debt_to_income")
dv_ratio <- function(df, numerator_col, denominator_col, new_col, otherwise = 0,
                     sentinels = "stop") {
  verb <- "dv_ratio"
  check_verb_basics(df, new_col, verb)
  check_columns_exist(df, c(numerator_col, denominator_col), verb)
  check_columns_numeric(df, c(numerator_col, denominator_col), verb)
  check_single_number(otherwise, "otherwise", verb, allow_na = TRUE)

  values <- sentinel_safe_values(df, c(numerator_col, denominator_col), verb, sentinels)
  numerator <- values[[1L]]
  denominator <- values[[2L]]

  ratio <- data.table::fifelse(denominator > 0, numerator / denominator,
                               as.numeric(otherwise))
  ratio[is.na(numerator) | is.na(denominator)] <- NA_real_

  df[[new_col]] <- ratio

  report_verb(new_col, paste0(
    numerator_col, " / ", denominator_col, ", ", otherwise, " where ",
    denominator_col, " is not above zero (", format_count(sum(denominator <= 0, na.rm = TRUE)),
    " rows)", sentinel_note(values, sentinels)
  ))

  df
}
