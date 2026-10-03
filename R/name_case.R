#' The ways new DV names can be spelt
#' @keywords internal
#' @noRd
name_case_options <- function() {
  c("auto", "lower", "upper", "spec")
}


#' @keywords internal
#' @noRd
check_name_case <- function(name_case) {
  if (!is.character(name_case) || length(name_case) != 1L || !name_case %in% name_case_options()) {
    cli::cli_alert_danger("{.arg name_case} must be one of {.val {name_case_options()}}.")
    stop("`name_case` is not recognised.", call. = FALSE)
  }

  invisible(NULL)
}


#' The naming style of the data: "lower", "upper", or "spec" when it mixes cases
#'
#' A style counts when at least 95% of the names with letters in follow it, so
#' a handful of odd system variables do not decide it.
#' @keywords internal
#' @noRd
detect_name_case <- function(data_names) {
  lettered <- data_names[!is.na(data_names) & grepl("[[:alpha:]]", data_names)]

  if (length(lettered) == 0L) {
    return("spec")
  }

  if (mean(lettered == tolower(lettered)) >= 0.95) {
    "lower"
  } else if (mean(lettered == toupper(lettered)) >= 0.95) {
    "upper"
  } else {
    "spec"
  }
}


#' Turn "auto" into the style new DV names will follow
#' @keywords internal
#' @noRd
resolve_name_case <- function(name_case, data_names) {
  if (name_case != "auto") {
    return(name_case)
  }

  if (is.null(data_names)) "spec" else detect_name_case(data_names)
}


#' A function that spells variable names for the suite
#'
#' A name the data has, in any case, takes the data's spelling. Any other name
#' follows `name_case`; with "spec", every mention of a DV takes the spelling of
#' its plan row, so one DV is not spelt two ways.
#' @keywords internal
#' @noRd
name_speller <- function(data_names, name_case, dv_names) {
  data_names <- unique(data_names[!is.na(data_names)])
  dv_names <- unique(dv_names[!is.na(dv_names)])

  function(names) {
    if (length(names) == 0L) {
      return(character())
    }

    in_data <- match(tolower(names), tolower(data_names))
    in_dvs <- match(tolower(names), tolower(dv_names))

    styled <- switch(
      name_case,
      lower = tolower(names),
      upper = toupper(names),
      spec = ifelse(is.na(in_dvs), names, dv_names[in_dvs])
    )

    ifelse(is.na(in_data), styled, data_names[in_data])
  }
}


#' Names that match a known name only when case is ignored
#'
#' @return A data frame with `name` and `known` (the known spelling).
#' @keywords internal
#' @noRd
case_clashes <- function(names, known_names) {
  names <- unique(names[!is.na(names)])
  known_names <- unique(known_names[!is.na(known_names)])
  position <- match(tolower(names), tolower(known_names))
  clash <- !is.na(position) & !(names %in% known_names)

  data.frame(
    name = names[clash],
    known = known_names[position[clash]],
    stringsAsFactors = FALSE
  )
}


#' Groups of names that differ only in case, as "a / A" text
#' @keywords internal
#' @noRd
case_duplicates <- function(names) {
  names <- unique(names[!is.na(names)])
  lower <- tolower(names)
  repeated <- unique(lower[duplicated(lower)])

  vapply(repeated, function(key) paste(names[lower == key], collapse = " / "), character(1L), USE.NAMES = FALSE)
}


#' Stop when the data has columns differing only in case
#' @keywords internal
#' @noRd
stop_if_case_duplicates <- function(data_names) {
  duplicates <- case_duplicates(data_names)

  if (length(duplicates) > 0L) {
    cli::cli_alert_danger(
      "The data has {length(duplicates)} set{?s} of columns whose names differ only in case: {.val {utils::head(duplicates, 10)}}. SPSS cannot hold both, and the suite cannot tell which to use; keep one of each."
    )
    stop("The data has column names that differ only in case.", call. = FALSE)
  }

  invisible(NULL)
}


#' Pair the data's spelling with the suite's for names that differ only in case
#'
#' The suite renames those columns to its spelling for the run and back after,
#' so a suite written for data in one case runs on data in another. Stops when
#' the data, or the suite itself, spells one name two ways.
#' @keywords internal
#' @noRd
case_renaming <- function(steps, data_names, settings_names = character()) {
  stop_if_case_duplicates(data_names)

  suite_names <- unique(c(
    names(steps),
    unlist(lapply(steps, `[[`, "inputs"), use.names = FALSE),
    settings_names
  ))
  spelt_twice <- case_duplicates(suite_names)

  if (length(spelt_twice) > 0L) {
    cli::cli_alert_danger(
      "The suite spells {length(spelt_twice)} variable{?s} more than one way: {.val {utils::head(spelt_twice, 10)}}. Use one spelling in every step's name and inputs."
    )
    stop("The suite spells a variable more than one way.", call. = FALSE)
  }

  clashes <- case_clashes(data_names, suite_names)

  if (nrow(clashes) > 0L) {
    shown <- utils::head(paste0(clashes$name, " (", clashes$known, " in the suite)"), 5)
    cli::cli_alert_info(
      "{nrow(clashes)} column{?s} in the data {?is/are} spelt in a different case from the suite, such as {shown}; {cli::qty(nrow(clashes))}{?it is/they are} matched ignoring case and keep the data's spelling."
    )
  }

  list(data = clashes$name, suite = clashes$known)
}


#' Rename columns, leaving the rest as they are
#' @keywords internal
#' @noRd
rename_columns <- function(df, from, to) {
  if (length(from) == 0L) {
    return(df)
  }

  position <- match(from, names(df))
  names(df)[position[!is.na(position)]] <- to[!is.na(position)]
  df
}
