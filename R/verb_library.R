#' The verb library: one row per function
#'
#' Read from `inst/extdata/verb_library.csv`. A "verb" is one of the functions
#' the DV suite calls to build a DV.
#'
#' @return A data frame: `verb`, `what_it_does` and `spec_wording_it_matches`.
#' @keywords internal
#' @noRd
#'
#' @examples
#' verb_library()
verb_library <- function() {
  read_package_csv("verb_library.csv")
}


#' What you give each verb: one row per argument
#'
#' Read from `inst/extdata/verb_arguments.csv`. Every verb also takes `df`, the
#' data, and `new_col`, the DV being built; those are not listed.
#'
#' * `what_to_give`: one column name, several column names, a number, a
#'   condition, and so on.
#' * `where_it_is_set`: `plan args` (the plan's `args` column, per DV),
#'   `plan condition` (the plan's `condition` column), or `settings.R` (the
#'   suite's settings file, the same for every DV).
#' * `settings_name`: for arguments set in `settings.R`, the name of the
#'   setting they read.
#' * `must_be_given`: `yes` or `no`.
#' * `if_not_given`: the value used when it is not given.
#'
#' @return A data frame with one row per argument.
#' @keywords internal
#' @noRd
#'
#' @examples
#' arguments <- verb_arguments()
#' arguments[arguments$verb == "dv_keep_if", ]
verb_arguments <- function() {
  read_package_csv("verb_arguments.csv")
}


#' Print the verb library to the console
#'
#' For each verb: what it does, the spec wording it matches, and what you give
#' it.
#'
#' @param verbs Optional verb names to show.
#'
#' @return The verb library, invisibly.
#' @export
#'
#' @examples
#' show_verbs("dv_keep_if")
show_verbs <- function(verbs = NULL) {
  library_table <- verb_library()
  arguments <- verb_arguments()

  if (!is.null(verbs)) {
    unknown <- setdiff(verbs, library_table$verb)

    if (length(unknown) > 0L) {
      cli::cli_alert_danger("Not a verb: {.val {unknown}}. Verbs: {.val {library_table$verb}}.")
      stop("Unknown verb.", call. = FALSE)
    }

    library_table <- library_table[library_table$verb %in% verbs, , drop = FALSE]
  }

  for (index in seq_len(nrow(library_table))) {
    verb <- library_table$verb[[index]]
    verb_rows <- arguments[arguments$verb == verb, , drop = FALSE]

    cli::cli_h2(verb)
    cli::cli_text(library_table$what_it_does[[index]])
    cli::cli_text("{.emph Spec wording it matches:} {.code {library_table$spec_wording_it_matches[[index]]}}")
    cli::cli_text("{.emph You give it:}")

    cli::cli_bullets(stats::setNames(
      paste0(
        "{.strong ", verb_rows$argument, "}: ", verb_rows$what_to_give,
        ", set in ", verb_rows$where_it_is_set,
        ifelse(verb_rows$where_it_is_set == "settings.R", paste0(" as ", verb_rows$settings_name), ""),
        ifelse(verb_rows$must_be_given == "yes", "", paste0(" (if not given: ", verb_rows$if_not_given, ")")),
        ". ", verb_rows$what_it_is_for
      ),
      rep("*", nrow(verb_rows))
    ))
  }

  invisible(library_table)
}


#' @keywords internal
#' @noRd
read_package_csv <- function(file_name) {
  path <- system.file("extdata", file_name, package = "wealthdv")

  if (!nzchar(path)) {
    cli::cli_alert_danger("{.file {file_name}} is missing from the installed package. Run setup_wealthdv.R again.")
    stop("Cannot find inst/extdata/", file_name, call. = FALSE)
  }

  utils::read.csv(path, colClasses = "character", na.strings = character(),
                  stringsAsFactors = FALSE, check.names = FALSE)
}


#' @keywords internal
#' @noRd
registered_verbs <- function() {
  verb_library()$verb
}


#' Arguments of a verb in the order the function takes them
#' @keywords internal
#' @noRd
verb_formals <- function(verb) {
  formals(get(verb, envir = asNamespace("wealthdv"), mode = "function"))
}


#' Default of an argument as R code, "" when it has none
#' @keywords internal
#' @noRd
formal_default <- function(verb, argument) {
  arg_list <- verb_formals(verb)

  if (identical(arg_list[[argument]], quote(expr = ))) {
    return("")
  }

  paste(deparse(arg_list[[argument]]), collapse = "")
}
