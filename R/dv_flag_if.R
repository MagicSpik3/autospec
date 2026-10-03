#' Flag rows where a condition holds
#'
#' 1 where the condition is `TRUE`, `otherwise` where it is `FALSE`, and
#' missing where it is missing.
#'
#' @param df A data frame.
#' @param condition A logical vector with one value per row, such as
#'   `df$CommiR9 %in% c(4, 5)`.
#' @param new_col Column to create.
#' @param otherwise Value where the condition is `FALSE`: a number, or `NA`
#'   where the spec gives no else branch (decision D5).
#'
#' @return `df` with `new_col` added.
#' @export
#'
#' @examples
#' df <- data.frame(CommiR9 = c(4, 1, 5))
#' dv_flag_if(df, df$CommiR9 %in% c(4, 5), "BillCredKeepNoR9")
dv_flag_if <- function(df, condition, new_col, otherwise = 0) {
  verb <- "dv_flag_if"
  condition_text <- describe_condition(substitute(condition))
  check_verb_basics(df, new_col, verb)
  check_condition(df, condition, verb)
  check_single_number(otherwise, "otherwise", verb, allow_na = TRUE)

  df[[new_col]] <- data.table::fifelse(condition, 1, as.numeric(otherwise))

  report_verb(new_col, paste0(
    "1 where ", condition_text, " (", format_count(sum(condition %in% TRUE)),
    " rows), ", otherwise, " elsewhere"
  ))

  df
}
