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
#' A tracking value is yes, no or blank. A step can be yes only when the steps
#' it needs are yes (`tracking_needs()`).
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
  needs <- tracking_needs()

  problems <- list()

  for (column in columns) {
    value <- values[[column]]

    not_yes_no <- !is.na(value) & !value %in% c("yes", "no")
    problems[[length(problems) + 1L]] <- problem_rows(
      plan, not_yes_no, column, "must be yes or no, or left blank"
    )

    for (needed in needs[[column]]) {
      broken <- value %in% "yes" & !values[[needed]] %in% "yes"
      problems[[length(problems) + 1L]] <- problem_rows(
        plan, broken, column, paste0("is yes but ", needed, " is not yes")
      )
    }
  }

  do.call(rbind, c(list(empty_tracking_problems()), problems))
}


#' The tracking columns each one needs to be yes first
#'
#' A test stub is written with its step, before the DV is, so it needs nothing.
#' @keywords internal
#' @noRd
tracking_needs <- function() {
  list(
    "DV Written" = character(),
    "Unit Test Stub" = character(),
    "Unit Test Populated" = "Unit Test Stub",
    "Unit Test Passed" = c("Unit Test Populated", "DV Written")
  )
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


#' Record what the suite shows on the plan rows for the same DVs
#'
#' Matched by UID, or by DV name in any case where the UID does not match. The
#' plan is a record of the suite, so values typed in it are replaced, and rows
#' with no suite step are left blank.
#'
#' @param plan A plan data frame.
#' @param tracking A data frame with `uid`, `dv` and tracking columns, from
#'   suite_tracking() and unit_test_results().
#' @return The plan with its tracking columns set.
#' @keywords internal
#' @noRd
record_tracking <- function(plan, tracking) {
  position <- match_uid_or_dv(plan$uid, plan$dv, tracking$uid, tracking$dv)

  for (column in tracking_columns()) {
    plan[[column]] <- if (column %in% names(tracking)) {
      as.character(tracking[[column]])[position]
    } else {
      rep(NA_character_, nrow(plan))
    }
  }

  plan
}


#' Where each UID and DV is in a table: by UID, or by DV name in any case where the UID does not match
#' @keywords internal
#' @noRd
match_uid_or_dv <- function(uid, dv, table_uid, table_dv) {
  uid <- ifelse(is_blank(uid), NA_character_, as.character(uid))
  by_uid <- match(uid, table_uid, incomparables = NA)
  by_dv <- match(tolower(dv), tolower(table_dv), incomparables = NA)

  ifelse(is.na(by_uid), by_dv, by_uid)
}


#' The first step each plan row is missing before it is finished, or NA when finished
#'
#' Checked in the order the work is done, so the reason given is the next thing to do.
#' @keywords internal
#' @noRd
tracking_gap <- function(plan) {
  gaps <- c(
    "DV Written" = "not written",
    "Unit Test Stub" = "no unit test",
    "Unit Test Populated" = "unit test not populated",
    "Unit Test Passed" = "unit test not passed"
  )
  gap <- rep(NA_character_, nrow(plan))

  for (column in names(gaps)) {
    missing <- is.na(gap) & !tracking_value(plan, column) %in% "yes"
    gap[missing] <- gaps[[column]]
  }

  gap
}


#' The DVs ready to deliver: written, with a unit test that is filled in and passed
#' @keywords internal
#' @noRd
finished_dvs <- function(plan) {
  as.character(plan$dv[is.na(tracking_gap(plan))])
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
