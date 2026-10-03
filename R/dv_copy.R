#' Copy a column into a DV
#'
#' @param df A data frame.
#' @param source_col Column to copy.
#' @param new_col Column to create.
#' @param sentinels What to do with -8/-9 codes (decision D1): `"keep"` copies
#'   them as they are, `"zero"` puts 0, `"missing"` puts `NA`, `"stop"` refuses.
#'
#' @return `df` with `new_col` added.
#' @export
#'
#' @examples
#' dv_copy(data.frame(FCISAvR9_i = c(100, -9)), "FCISAvR9_i", "DVCISAvR9", sentinels = "zero")
dv_copy <- function(df, source_col, new_col, sentinels = "keep") {
  verb <- "dv_copy"
  check_verb_basics(df, new_col, verb)
  check_columns_exist(df, source_col, verb)
  check_sentinels_setting(sentinels, verb, allow_keep = TRUE)

  values <- df[[source_col]]

  if (sentinels != "keep") {
    check_columns_numeric(df, source_col, verb)
  }

  cleaned <- apply_sentinels(values, values %in% sentinel_codes(), sentinels, verb, source_col)
  df[[new_col]] <- cleaned$values

  report_verb(new_col, paste0(
    "copied from ", source_col,
    sentinel_note(structure(list(), replaced = cleaned$replaced), sentinels)
  ))

  df
}
