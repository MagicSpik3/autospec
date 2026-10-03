#' @title expand_variable_names
#'
#' @description
#'
#' Expands variable names carrying a numbered range into the individual
#' variables they stand for: `DCSC(1-5)R9` becomes `DCSC1R9` to `DCSC5R9`.
#'
#' Built on [find_number_ranges()], so it follows the same rules as
#' [find_number_ranges()]. A range written twice in one name is expanded in
#' parallel; two different ranges, as in `M(1-12)_interest(1-3)`, are expanded
#' in every combination. A descending range such as `(5-1)` is left as written
#' rather than stopping, because this is used to read specs, not to validate
#' them.
#'
#' @param names A character vector of variable names.
#'
#' @return A character vector of expanded names, in order, without duplicates.
#'
#' @keywords internal
#' @noRd
#'
#' @examples
#' expand_variable_names(c("DCSC(1-3)R9", "TOTCSCR9_SUM"))
expand_variable_names <- function(names) {
  if (!is.character(names)) {
    cli::cli_alert_danger("{.arg names} must be a character vector.")
    stop("`names` must be a character vector.", call. = FALSE)
  }

  names <- names[!is.na(names)]

  expanded <- lapply(names, function(name) {
    ranges <- find_number_ranges(name)

    if (nrow(ranges) == 0L || any(ranges$lower > ranges$upper)) {
      return(name)
    }

    results <- name

    for (range_text in unique(ranges$match)) {
      bounds <- ranges[ranges$match == range_text, , drop = FALSE][1L, ]

      results <- unlist(
        lapply(results, function(partial) {
          vapply(
            seq.int(bounds$lower, bounds$upper),
            FUN = function(index) replace_number_range(partial, range_text, index),
            FUN.VALUE = character(1L)
          )
        }),
        use.names = FALSE
      )
    }

    results
  })

  unique(unlist(expanded, use.names = FALSE))
}


#' @title resolve_variable_names
#'
#' @description
#'
#' Matches the variable names used in derivations against the names known to
#' exist, so that inputs are recorded in their real spelling and misspellings
#' are caught.
#'
#' SPSS variable names are not case-sensitive, and the specifications take
#' full advantage: `DVBLDValR9`, `DVBldValR9` and `DVblDValR9` all appear for
#' one variable. R is case-sensitive, so each name is resolved in three steps:
#'
#' 1. An exact match.
#' 2. A match ignoring case. Where several spellings exist, the first one known
#'    is used.
#' 3. No match. The closest known name within two edits is offered as a
#'    suggestion, which catches slips such as `DVFLfSVR9` for `DVFLfFSVR9`.
#'
#' @param names A character vector of names to resolve.
#' @param known_names A character vector of names known to exist — typically
#'   the DVs in the catalogue together with the column names of the input data.
#'
#' @return A data frame with one row per element of `names` and the columns
#'   `name`, `resolved` (the known spelling, or `name` when unknown), `how`
#'   (`"exact"`, `"case"` or `"unknown"`) and `suggestion` (a near-miss for an
#'   unknown name, otherwise `NA`).
#'
#' @importFrom utils adist
#'
#' @keywords internal
#' @noRd
#'
#' @examples
#' resolve_variable_names(
#'   c("dvhsevalr9", "DVFLfSVR9"),
#'   known_names = c("DVHseValR9", "DVFLfFSVR9")
#' )
resolve_variable_names <- function(names, known_names) {
  if (!is.character(names) || !is.character(known_names)) {
    cli::cli_alert_danger("{.arg names} and {.arg known_names} must be character vectors.")
    stop("`names` and `known_names` must be character vectors.", call. = FALSE)
  }

  known_names <- unique(known_names[!is.na(known_names)])
  known_lower <- tolower(known_names)

  resolved <- names
  how <- rep("unknown", length(names))
  suggestion <- rep(NA_character_, length(names))

  exact <- names %in% known_names
  how[exact] <- "exact"

  case_position <- match(tolower(names), known_lower)
  by_case <- !exact & !is.na(case_position)
  resolved[by_case] <- known_names[case_position[by_case]]
  how[by_case] <- "case"

  unknown <- which(how == "unknown" & !is.na(names))

  if (length(unknown) > 0L && length(known_names) > 0L) {
    distances <- utils::adist(tolower(names[unknown]), known_lower)

    best <- apply(distances, 1L, which.min)
    best_distance <- distances[cbind(seq_along(unknown), best)]

    close <- best_distance <= 2L
    suggestion[unknown[close]] <- known_names[best[close]]
  }

  data.frame(
    name = names,
    resolved = resolved,
    how = how,
    suggestion = suggestion,
    stringsAsFactors = FALSE
  )
}


#' Compare two names as the same variable
#' @keywords internal
#' @noRd
same_variable <- function(first, second) {
  clean <- function(value) tolower(sub("[.[:space:]]+$", "", value))

  !is.na(first) & !is.na(second) & clean(first) == clean(second)
}


#' Remove a trailing full stop, and spaces before a numbered range, from a DV name
#'
#' `DVDBInc (1-6)R9.` becomes `DVDBInc(1-6)R9`.
#' @keywords internal
#' @noRd
clean_dv_name <- function(name) {
  name <- sub("[.[:space:]]+$", "", trimws(name))
  gsub("\\s+(?=\\()", "", name, perl = TRUE)
}
