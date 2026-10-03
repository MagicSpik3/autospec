#' Flag rows where any of several columns holds one of some values
#'
#' Covers SPSS `ANY(value, a, b, c)`. Missing only where every column is
#' missing.
#'
#' @param df A data frame.
#' @param value_cols Columns to check.
#' @param match_values Values that set the flag.
#' @param new_col Column to create.
#'
#' @return `df` with `new_col` added.
#' @export
#'
#' @examples
#' df <- data.frame(DLBeh1R9_i = c(2, 1), DLBeh2R9_i = c(1, 1))
#' dv_flag_any_of(df, c("DLBeh1R9_i", "DLBeh2R9_i"), 2, "DVHasLnArR9")
dv_flag_any_of <- function(df, value_cols, match_values, new_col) {
  verb <- "dv_flag_any_of"
  check_verb_basics(df, new_col, verb)
  check_columns_exist(df, value_cols, verb)

  if (!is.numeric(match_values) || length(match_values) == 0L || anyNA(match_values)) {
    verb_stop(verb, "`match_values` must be one or more numbers.")
  }

  matched <- Reduce(`|`, lapply(value_cols, function(column) df[[column]] %in% match_values))
  all_missing <- Reduce(`&`, lapply(value_cols, function(column) is.na(df[[column]])))

  flag <- as.numeric(matched)
  flag[all_missing] <- NA_real_

  df[[new_col]] <- flag

  report_verb(new_col, paste0(
    "1 where any of ", paste(utils::head(value_cols, 4L), collapse = ", "),
    if (length(value_cols) > 4L) ", ...", " is ",
    paste(match_values, collapse = " or "), " (", format_count(sum(flag %in% 1)),
    " rows), 0 elsewhere"
  ))

  df
}
