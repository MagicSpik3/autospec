#' The DV delivery columns the plan tracks, in the order each step follows the one before
#' @keywords internal
#' @noRd
tracking_columns <- function() {
  c("DV Written", "Unit Test Stub", "Unit Test Populated", "Unit Test Passed")
}


#' Each tracking value as lower-case text, blank as NA; NAs when the plan lacks the column
#' @keywords internal
#' @noRd
tracking_value <- function(plan, column) {
  if (!column %in% names(plan)) {
    return(rep(NA_character_, nrow(plan)))
  }

  value <- tolower(trimws(as.character(plan[[column]])))
  value[value %in% ""] <- NA_character_
  value
}


#' The rows of a plan that break the tracking rules
#'
#' A tracking value is yes, no or blank. A step can be yes only when the step
#' before it is yes.
#'
#' @param plan A plan data frame.
#' @return A data frame with one row per problem: `dv`, `sheet_name`, `column`
#'   and `problem`.
#' @keywords internal
#' @noRd
tracking_problems <- function(plan) {
  columns <- tracking_columns()
  values <- lapply(columns, function(column) tracking_value(plan, column))
  names(values) <- columns

  problems <- list()

  for (index in seq_along(columns)) {
    column <- columns[[index]]
    value <- values[[column]]

    not_yes_no <- !is.na(value) & !value %in% c("yes", "no")
    problems[[length(problems) + 1L]] <- problem_rows(
      plan, not_yes_no, column, "must be yes or no, or left blank"
    )

    if (index > 1L) {
      before <- columns[[index - 1L]]
      broken <- value %in% "yes" & !values[[before]] %in% "yes"
      problems[[length(problems) + 1L]] <- problem_rows(
        plan, broken, column, paste0("is yes but ", before, " is not yes")
      )
    }
  }

  do.call(rbind, c(list(empty_tracking_problems()), problems))
}


#' @keywords internal
#' @noRd
problem_rows <- function(plan, rows, column, problem) {
  index <- which(rows)

  data.frame(
    dv = as.character(plan$dv[index]),
    sheet_name = as.character(plan$sheet_name[index]),
    column = rep(column, length(index)),
    problem = rep(problem, length(index)),
    stringsAsFactors = FALSE
  )
}


#' @keywords internal
#' @noRd
empty_tracking_problems <- function() {
  data.frame(dv = character(), sheet_name = character(), column = character(),
             problem = character(), stringsAsFactors = FALSE)
}


#' Copy tracking values from an earlier plan onto the same DVs on the same sheets
#'
#' Kept whatever the spec text now says: they record work already done.
#' @keywords internal
#' @noRd
carry_over_tracking <- function(plan, previous_plan) {
  previous <- as.data.frame(previous_plan, stringsAsFactors = FALSE)

  if (!all(tracking_columns() %in% names(previous))) {
    return(plan)
  }

  new_key <- paste(tolower(plan$dv), plan$sheet_name, sep = "\r")
  old_key <- paste(tolower(clean_dv_name(previous$dv)), previous$sheet_name, sep = "\r")
  position <- match(new_key, old_key)

  for (column in tracking_columns()) {
    plan[[column]] <- previous[[column]][position]
  }

  plan
}
