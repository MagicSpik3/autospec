#' @title default_exclude_sheets
#'
#' @description
#'
#' Sheets that appear in the derived variable specifications but do not contain
#' derivations. They are change logs, imputation lists, planning sheets and
#' value-label lookups, and are skipped when the catalogue is built.
#'
#' Sheet names are matched exactly as they appear in the workbook, including
#' the leading space in `" Final accounting structure"`. Some names carry the
#' round number, such as `R9_finalised_changelog`; those are included only when
#' `round` is given.
#'
#' @param round The round number, or `NULL` for only the names that do not
#'   carry one.
#'
#' @return A character vector of sheet names to exclude.
#'
#' @export
#'
#' @examples
#' default_exclude_sheets(round = 9)
default_exclude_sheets <- function(round = NULL) {
  templates <- exclude_sheet_templates()
  has_round <- grepl("{round}", templates, fixed = TRUE)

  if (is.null(round)) {
    return(templates[!has_round])
  }

  fill_round(templates, round)
}


#' Excluded sheet names, with {round} where the round number goes
#' @keywords internal
#' @noRd
exclude_sheet_templates <- function() {
  c(
    "Major Changes",
    "Imputation_list",
    "Imputation Change_log",
    "DV Change Log",
    "LA Change_log",
    "Change_log",
    "Round {round} Imp Spec",
    "R{round}_finalised_changelog",
    " Final accounting structure",
    "R{round}_Total_Household_Income",
    "DVPinPTyp38",
    "DVPinPTyp18"
  )
}
