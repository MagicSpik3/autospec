#' Discount an amount expected at a target age to its present value
#'
#' `amount / (1 + rate) ^ (target_age - age)`. Rows at or past the target age
#' are set to `NA` with a warning. Stops if either column holds -8 or -9, unless
#' `sentinels` says otherwise.
#'
#' @param df A data frame.
#' @param amount_col Amount expected at the target age.
#' @param age_col Current age column.
#' @param new_col Column to create.
#' @param rate Discount rate, from the pension rates lookup.
#' @param target_age Age the amount is expected at.
#' @param sentinels What to do with -8/-9 codes (decision D1): `"stop"`,
#'   `"missing"` to treat them as `NA`, or `"zero"` to treat them as 0.
#'
#' @return `df` with `new_col` added.
#' @export
#'
#' @examples
#' df <- data.frame(pot = c(10000, 10000), age = c(56, 70))
#' dv_discount_to_present_value(df, "pot", "age", "pot_pv", rate = 0.03)
dv_discount_to_present_value <- function(df, amount_col, age_col, new_col, rate,
                                         target_age = 66, sentinels = "stop") {
  verb <- "dv_discount_to_present_value"
  check_verb_basics(df, new_col, verb)
  check_columns_exist(df, c(amount_col, age_col), verb)
  check_columns_numeric(df, c(amount_col, age_col), verb)

  if (missing(rate) || is.null(rate)) {
    verb_stop(verb, "`rate` must be supplied; set it in settings.R from the pension rates lookup.")
  }

  check_single_number(rate, "rate", verb)
  check_single_number(target_age, "target_age", verb)

  values <- sentinel_safe_values(df, c(amount_col, age_col), verb, sentinels)
  amounts <- values[[1L]]
  ages <- values[[2L]]

  years <- target_age - ages
  present_value <- amounts / (1 + rate)^years

  at_or_past <- !is.na(years) & years <= 0

  if (any(at_or_past)) {
    warning(
      verb, "(): ", sum(at_or_past), " rows are at or past age ", target_age,
      " and were set to NA.",
      call. = FALSE
    )
    present_value[at_or_past] <- NA_real_
  }

  df[[new_col]] <- present_value

  report_verb(new_col, paste0(
    amount_col, " discounted at ", rate, " a year from ", age_col, " to age ", target_age,
    sentinel_note(values, sentinels)
  ))

  df
}
