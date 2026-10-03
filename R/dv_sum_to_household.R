#' Add a person-level value across the household
#'
#' The household total is written back onto every person's row. Stops if the
#' column holds -8 or -9, unless `sentinels` says otherwise.
#'
#' @param df A person-level data frame.
#' @param value_col Column to add up.
#' @param new_col Column to create.
#' @param by Household identifier column. Must not be missing.
#' @param na_as_zero Whether a missing value counts as zero (decision D3).
#'   When `FALSE`, one missing person makes the household total missing.
#' @param sentinels What to do with -8/-9 codes (decision D1): `"stop"`,
#'   `"missing"` to treat them as `NA`, or `"zero"` to treat them as 0.
#'
#' @return `df` with `new_col` added.
#' @export
#'
#' @examples
#' df <- data.frame(hhserial = c(1, 1, 2), savings = c(100, 50, NA))
#' dv_sum_to_household(df, "savings", "savings_hh")
dv_sum_to_household <- function(df, value_col, new_col, by = "hhserial",
                                na_as_zero = TRUE, sentinels = "stop") {
  verb <- "dv_sum_to_household"
  check_verb_basics(df, new_col, verb)
  check_columns_exist(df, value_col, verb)
  check_columns_numeric(df, value_col, verb)
  check_household_id(df, by, verb)
  check_true_or_false(na_as_zero, "na_as_zero", verb)

  cleaned <- sentinel_safe_values(df, value_col, verb, sentinels)

  if (nrow(df) == 0L) {
    df[[new_col]] <- numeric()
    return(df)
  }

  values <- cleaned[[1L]]

  if (na_as_zero) {
    values[is.na(values)] <- 0
  }

  household <- as.character(df[[by]])
  totals <- rowsum(values, household, reorder = FALSE, na.rm = FALSE)

  df[[new_col]] <- unname(totals[match(household, rownames(totals)), 1L])

  report_verb(new_col, paste0(
    "household total of ", value_col, " across ", format_count(nrow(totals)),
    " households",
    if (na_as_zero) ", missing counted as zero" else ", missing if anyone is missing",
    sentinel_note(cleaned, sentinels)
  ))

  df
}
