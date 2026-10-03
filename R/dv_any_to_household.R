#' Flag a household where anyone has a person-level flag
#'
#' Written back onto every person's row. Works like SPSS `MAX` when
#' aggregating: missing people are ignored, and a household where everyone is
#' missing gets `NA`. Anything other than 0, 1 or missing stops.
#'
#' @param df A person-level data frame.
#' @param flag_col Person-level 0/1 flag.
#' @param new_col Column to create.
#' @param by Household identifier column. Must not be missing.
#'
#' @return `df` with `new_col` added as integer 0/1.
#' @export
#'
#' @examples
#' df <- data.frame(hhserial = c(1, 1, 2), owns = c(0, 1, 0))
#' dv_any_to_household(df, "owns", "owns_hh")
dv_any_to_household <- function(df, flag_col, new_col, by = "hhserial") {
  verb <- "dv_any_to_household"
  check_verb_basics(df, new_col, verb)
  check_columns_exist(df, flag_col, verb)
  check_household_id(df, by, verb)

  flag <- df[[flag_col]]
  not_flag <- !is.na(flag) & !(flag %in% c(0, 1))

  if (any(not_flag)) {
    verb_stop(verb, paste0(
      "`", flag_col, "` must hold only 0, 1 or missing; ", sum(not_flag),
      " rows hold something else (",
      paste(utils::head(unique(flag[not_flag]), 5L), collapse = ", "), ")."
    ))
  }

  if (nrow(df) == 0L) {
    df[[new_col]] <- integer()
    return(df)
  }

  household <- as.character(df[[by]])
  hits <- rowsum(as.numeric(flag %in% 1), household, reorder = FALSE)
  answered <- rowsum(as.numeric(!is.na(flag)), household, reorder = FALSE)

  household_flag <- ifelse(answered[, 1L] > 0, as.integer(hits[, 1L] > 0), NA_integer_)
  df[[new_col]] <- unname(household_flag[match(household, rownames(hits))])

  report_verb(new_col, paste0(
    "1 for the ", format_count(sum(household_flag %in% 1L)), " of ",
    format_count(nrow(hits)), " households where anyone has ", flag_col, " = 1",
    if (anyNA(household_flag)) paste0("; ", format_count(sum(is.na(household_flag))), " all missing") else ""
  ))

  df
}
