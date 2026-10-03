#' Convert an amount per period to an annual or monthly amount
#'
#' With `period_codes` and `multipliers`, uses the spec's own period table: each
#' amount is multiplied by the multiplier of its period code, and an amount of 0
#' stays 0. The specs use more than one period table (decision D4), so the plan
#' copies the table from the spec. Without them, uses the period multipliers in
#' `was.utils::annualise()`, which gives -8 for a zero, negative or 97 period,
#' -9 for an unrecognised code, and rounds to 2 decimal places.
#'
#' @param df A data frame.
#' @param amount_col Amount column.
#' @param period_col Period code column.
#' @param new_col Column to create.
#' @param target `"annual"`, or `"monthly"` for one twelfth of the annual amount.
#'   Only used with the was.utils table; the spec's multipliers already give the
#'   amount the spec wants.
#' @param period_codes Period codes from the spec, such as `c(1, 2, 3)`.
#' @param multipliers What the spec multiplies the amount by for each code, such
#'   as `c(52, 12, 1)`.
#' @param sentinels What to do where the amount, or the period of a non-zero
#'   amount, is -8/-9 (decision D1): `"keep"` keeps the code, `"zero"` puts 0,
#'   `"missing"` puts `NA`, `"stop"` refuses.
#'
#' @return `df` with `new_col` added.
#' @export
#'
#' @examples
#' df <- data.frame(amount = c(100, 1000, -9), period = c(1, 2, 3))
#' dv_annualise(df, "amount", "period", "annual_amount",
#'              period_codes = c(1, 2, 3), multipliers = c(52, 12, 1), sentinels = "zero")
dv_annualise <- function(df, amount_col, period_col, new_col, target = "annual",
                         period_codes = NULL, multipliers = NULL, sentinels = "keep") {
  verb <- "dv_annualise"
  check_verb_basics(df, new_col, verb)
  check_columns_exist(df, c(amount_col, period_col), verb)
  check_columns_numeric(df, c(amount_col, period_col), verb)
  check_sentinels_setting(sentinels, verb, allow_keep = TRUE)

  if (!is.character(target) || length(target) != 1L ||
      !target %in% c("annual", "monthly")) {
    verb_stop(verb, "`target` must be \"annual\" or \"monthly\".")
  }

  if (is.null(period_codes) != is.null(multipliers)) {
    verb_stop(verb, "give both `period_codes` and `multipliers`, or neither.")
  }

  amounts <- as.numeric(df[[amount_col]])
  periods <- as.numeric(df[[period_col]])

  if (!is.null(period_codes)) {
    if (!is.numeric(period_codes) || !is.numeric(multipliers) ||
        length(period_codes) != length(multipliers) || length(period_codes) == 0L ||
        anyNA(period_codes) || anyNA(multipliers) || anyDuplicated(period_codes) > 0L) {
      verb_stop(verb, "`period_codes` and `multipliers` must be numbers of the same length, one per period code.")
    }

    converted <- amounts * multipliers[match(periods, period_codes)]
    converted[amounts %in% 0] <- 0

    unrecognised <- !is.na(amounts) & amounts > 0 & !is.na(periods) &
      !periods %in% sentinel_codes() & !periods %in% period_codes

    if (any(unrecognised)) {
      warning(
        verb, "(): ", sum(unrecognised), " rows have a period code not in the spec's table (",
        paste(format_number(period_codes), collapse = ", "), ") and were set to NA.",
        call. = FALSE
      )
    }

    table_text <- "the spec's period codes"
  } else {
    if (!requireNamespace("was.utils", quietly = TRUE)) {
      verb_stop(verb, paste(
        "the was.utils package is needed for its period multipliers.",
        "Run setup_autospec.R, which installs it, or give the spec's period_codes and multipliers."
      ))
    }

    converted <- was.utils::annualise(values = amounts, period = periods)

    unrecognised <- sum(
      !is.na(amounts) & amounts >= 0 & !is.na(periods) & periods > 0 & periods != 97 &
        is.na(was.utils::calc_annual_multiplier(periods))
    )

    if (unrecognised > 0L) {
      warning(
        verb, "(): ", unrecognised, " rows have a period code that was.utils does not ",
        "recognise and were set to -9. Check which period table the spec uses.",
        call. = FALSE
      )
    }

    if (target == "monthly") {
      positive <- !is.na(converted) & converted >= 0
      converted[positive] <- round(converted[positive] / 12, 2L)
    }

    table_text <- "the was.utils period codes"
  }

  amount_sentinel <- amounts %in% sentinel_codes()
  period_sentinel <- periods %in% sentinel_codes() & !amounts %in% 0
  sentinel_rows <- amount_sentinel | period_sentinel

  if (!is.null(period_codes)) {
    converted[amount_sentinel] <- amounts[amount_sentinel]
    converted[period_sentinel & !amount_sentinel] <- periods[period_sentinel & !amount_sentinel]
  }

  cleaned <- apply_sentinels(converted, sentinel_rows, sentinels, verb, paste(amount_col, "or", period_col))
  df[[new_col]] <- cleaned$values

  report_verb(new_col, paste0(
    amount_col, " converted using ", table_text, " in ", period_col,
    sentinel_note(structure(list(), replaced = cleaned$replaced), sentinels)
  ))

  df
}
