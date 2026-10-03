#' @title analyse_derivation
#'
#' @description
#'
#' Reads a derivation and reports what it is, what it reads and what it writes.
#' This is what fills the catalogue's `inputs` and `outputs` columns, and the
#' first step of [match_verb()].
#'
#' Before parsing, a derivation is checked against the handful of things in the
#' spec columns that are not derivations at all: value labels
#' (`1.0: 'One week'`), notes that a variable is imputed or derived elsewhere,
#' a band midpoint described in words, and R code pasted into the spec.
#'
#' @param text A single derivation.
#' @param dv The name of the variable the derivation defines.
#' @param level The level of the block the derivation came from: `"person"`,
#'   `"household"`, `"input"` or `"unknown"`.
#' @param known_names Variable names known to exist. Inputs are reported in
#'   their known spelling where one matches ignoring case.
#'
#' @return A list with elements:
#'
#' * `kind` — `"parsed"`, `"unparsed"`, `"value_labels"`, `"upstream"`,
#'   `"midpoint"` or `"r_code"`.
#' * `statements` — the parse tree when `kind` is `"parsed"`.
#' * `parse_error` — the parse error when `kind` is `"unparsed"`, else `NA`.
#' * `notes` — repairs made while parsing and problems spotted in the text.
#' * `inputs` — variables the derivation reads, excluding the ones it writes.
#' * `outputs` — variables the derivation writes.
#'
#' @export
#'
#' @examples
#' analyse_derivation(
#'   "IF DVHseValR9 > 0 THEN PropDVHseValR9 = 1 (otherwise PropDVHseValR9 = 0)",
#'   dv = "PropDVHseValR9"
#' )$inputs
analyse_derivation <- function(text, dv = NA_character_, level = NA_character_,
                               known_names = character()) {
  if (!is.character(text) || length(text) != 1L || is.na(text)) {
    cli::cli_alert_danger("{.arg text} must be a single non-missing string.")
    stop("`text` must be a single non-missing string.", call. = FALSE)
  }

  dv <- clean_dv_name(dv)

  analysis <- function(kind, statements = list(), parse_error = NA_character_,
                       notes = character(), inputs = character(),
                       outputs = character()) {
    list(
      kind = kind,
      statements = statements,
      parse_error = parse_error,
      notes = unique(notes),
      inputs = inputs,
      outputs = outputs
    )
  }

  if (identical(level, "input") ||
      grepl("^\\s*-?\\d+(\\.\\d+)?\\s*:\\s*['\"]", text, perl = TRUE)) {
    return(analysis("value_labels"))
  }

  if (grepl(
    "^\\s*(imputed from derived|annual amounts are derived from|derived in\\b.*\\bspec)",
    text, ignore.case = TRUE, perl = TRUE
  )) {
    return(analysis("upstream"))
  }

  midpoint <- regmatches(
    text,
    regexec(
      "^\\s*mid-?\\s*point taken from\\s+([A-Za-z_][A-Za-z0-9_]*)\\s+banded amount",
      text, ignore.case = TRUE, perl = TRUE
    )
  )[[1L]]

  if (length(midpoint) > 0L) {
    return(analysis("midpoint", inputs = midpoint[[2L]], outputs = dv))
  }

  if (grepl("<-|\\bfunction\\s*\\(|\\bcase_when\\s*\\(", text, perl = TRUE)) {
    return(analysis("r_code"))
  }

  parsed <- parse_derivation(text, known_names)

  if (!parsed$ok) {
    return(analysis("unparsed", parse_error = parsed$error, notes = parsed$notes))
  }

  statements <- parsed$statements
  notes <- parsed$notes

  targets <- tree_targets(statements)
  named_targets <- unique(clean_dv_name(targets[!is.na(targets)]))

  names_read <- unique(tree_names(statements))
  is_written <- vapply(
    names_read,
    FUN = function(name) any(same_variable(name, c(named_targets, dv))),
    FUN.VALUE = logical(1L),
    USE.NAMES = FALSE
  )
  inputs <- names_read[!is_written]

  if (length(inputs) > 0L && length(known_names) > 0L) {
    resolution <- resolve_variable_names(inputs, known_names)
    inputs <- unique(resolution$resolved)
  }

  stray <- named_targets[!same_variable(named_targets, dv)]

  if (!is.na(dv) && length(stray) > 0L) {
    notes <- c(notes, paste0(
      "assigns to ", paste(sort(unique(stray)), collapse = ", "), ", not ", dv
    ))
  }

  later_statements <- statements[-1L]
  loose <- later_statements[vapply(later_statements, node_is, logical(1L), type = "expr")]

  if (length(loose) > 0L) {
    loose_names <- sort(unique(tree_names(loose)))
    notes <- c(notes, paste0(
      "stray term with no operator: ",
      if (length(loose_names) > 0L) paste(loose_names, collapse = ", ") else "a value"
    ))
  }

  outputs <- named_targets

  if (anyNA(targets) && !is.na(dv)) {
    outputs <- unique(c(dv, outputs))
  }

  analysis(
    "parsed",
    statements = statements,
    notes = notes,
    inputs = inputs,
    outputs = outputs
  )
}


#' @title match_verb
#'
#' @description
#'
#' Drafts how a derivation should be built: which verb from the library, with
#' which arguments, and — for conditional verbs — the condition written in R.
#'
#' The match is made on the shape of the parse tree, not on the wording, so
#' `DVCISAvR9_aggr = SUM(DVCISAvR9)`, `aggregate from person level
#' (DVCISAvR9)` and `SUM(DVCISAvR9) - Aggregated from person level` all match
#' [dv_sum_to_household()]. The level of the block decides between shapes
#' that look alike: `SUM(x)` at household level adds people within a
#' household, while `SUM(a, b)` adds columns within a row.
#'
#' Every result is a draft for review. The status says how much review it needs:
#'
#' * `"auto"` — an unambiguous shape. Still to be signed off.
#' * `"needs_review"` — a verb was found but something must be checked: a
#'   drafted condition, a convention decision (see `reasons`), or a derivation
#'   that could not be parsed.
#' * `"hand_written"` — no verb fits; write a bespoke function.
#' * `"not_a_derivation"` — value labels, or a variable derived elsewhere.
#'
#' @param text A single derivation.
#' @param dv The variable the derivation defines.
#' @param level The block level: `"person"`, `"household"`, `"input"` or
#'   `"unknown"`.
#' @param known_names Variable names known to exist.
#'
#' @return A list with elements `verb` (a verb name or `NA`), `args` (a named
#'   list of per-row arguments, not including `df`, `new_col`, `condition` or
#'   run settings), `condition` (R code as text, or `NA`), `status`, `reasons`,
#'   `inputs`, `outputs`, `notes` and `parse_error`.
#'
#' @export
#'
#' @examples
#' match_verb("IF CommiR9 in (4,5) THEN BillCredKeepNoR9 = 1 ELSE BillCredKeepNoR9 = 0",
#'            dv = "BillCredKeepNoR9", level = "person")
match_verb <- function(text, dv, level = NA_character_, known_names = character()) {
  if (!is.character(dv) || length(dv) != 1L || is.na(dv) || !nzchar(trimws(dv))) {
    cli::cli_alert_danger("{.arg dv} must be a single variable name.")
    stop("`dv` must be a single variable name.", call. = FALSE)
  }

  dv <- clean_dv_name(dv)
  analysis <- analyse_derivation(text, dv, level, known_names)
  reasons <- character()

  draft <- function(status, verb = NA_character_, args = list(),
                    condition = NA_character_, why = character()) {
    list(
      verb = verb,
      args = args,
      condition = condition,
      status = status,
      reasons = c(reasons, why),
      inputs = analysis$inputs,
      outputs = analysis$outputs,
      notes = analysis$notes,
      parse_error = analysis$parse_error
    )
  }

  if (analysis$kind == "value_labels") {
    return(draft("not_a_derivation", why = "value labels, not a derivation"))
  }

  if (analysis$kind == "upstream") {
    return(draft("not_a_derivation", why = "derived upstream or in another spec"))
  }

  if (analysis$kind == "midpoint") {
    band_col <- analysis$inputs[[1L]]
    table <- sub("r\\d+_i$", "", tolower(band_col))

    if (table %in% names(band_midpoint_tables())) {
      return(draft("auto", "dv_band_midpoint",
                   args = list(band_col = band_col, table = table)))
    }

    return(draft("needs_review", "dv_band_midpoint",
                 args = list(band_col = band_col, table = table),
                 why = paste("no band table called", table)))
  }

  if (analysis$kind == "r_code") {
    return(draft("hand_written", why = "written as R code"))
  }

  if (analysis$kind == "unparsed") {
    return(draft("needs_review", why = paste("could not parse:", analysis$parse_error)))
  }

  statements <- analysis$statements
  is_household <- identical(level, "household")
  known_lookup <- unique(c(known_names, analysis$inputs))
  dv_ranges <- find_number_ranges(dv)

  rename <- function(name) {
    ranges <- find_number_ranges(name)

    mismatched <- nrow(ranges) > 0L && (
      nrow(dv_ranges) != 1L ||
        any(ranges$lower != dv_ranges$lower[[1L]] | ranges$upper != dv_ranges$upper[[1L]])
    )

    if (mismatched) {
      signal_untranslatable("a numbered range that does not match the DV")
    }

    resolve_variable_names(name, known_lookup)$resolved[[1L]]
  }

  with_condition <- function(verb, condition, args,
                             why = "confirm the drafted condition") {
    drafted <- tryCatch(
      list(text = condition_to_r(condition, rename), why = character()),
      untranslatable = function(problem) {
        list(
          text = NA_character_,
          why = paste0("condition uses ", conditionMessage(problem), ", write it by hand")
        )
      }
    )

    draft("needs_review", verb, args = args, condition = drafted$text,
          why = c(drafted$why, why))
  }

  # --- a single assignment or expression ------------------------------------

  if (length(statements) == 1L && statements[[1L]]$type %in% c("assign", "expr")) {
    value <- statements[[1L]]$value

    is_group_summary <- node_is(value, "call") && value$fn %in% c("SUM", "MAX") &&
      length(value$args) == 1L && node_is(value$args[[1L]], "name") &&
      nrow(find_number_ranges(value$args[[1L]]$name)) == 0L

    if (is_group_summary && is_household) {
      if (length(value$named) > 0L) {
        reasons <- c(reasons, paste0(
          "spec sets ", paste(names(value$named), collapse = ", "), " (decision D3)"
        ))
      }

      if (value$fn == "SUM") {
        return(draft("auto", "dv_sum_to_household",
                     args = list(value_col = value$args[[1L]]$name)))
      }

      return(draft("auto", "dv_any_to_household",
                   args = list(flag_col = value$args[[1L]]$name)))
    }

    if (node_is(value, "name")) {
      return(draft("auto", "dv_copy", args = list(source_col = value$name)))
    }

    parts <- flatten_addition(value)

    if (!is.null(parts) &&
        (length(parts) >= 2L || nrow(find_number_ranges(parts[[1L]])) > 0L)) {
      return(draft("auto", "dv_row_total", args = list(value_cols = parts)))
    }

    if (node_is(value, "bin") && value$op == "-" &&
        node_is(value$left, "name") && node_is(value$right, "name")) {
      return(draft("auto", "dv_difference",
                   args = list(minuend_col = value$left$name,
                               subtrahend_col = value$right$name)))
    }
  }

  # --- copy, then floor at zero ----------------------------------------------

  if (length(statements) == 2L &&
      node_is(statements[[1L]], "assign") && node_is(statements[[1L]]$value, "name") &&
      node_is(statements[[2L]], "if") && length(statements[[2L]]$branches) == 1L &&
      length(statements[[2L]]$otherwise) == 0L) {
    condition <- statements[[2L]]$branches[[1L]]$condition
    assignment <- single_assignment(statements[[2L]]$branches[[1L]]$body)

    floors_at_zero <- node_is(condition, "bin") && condition$op == "<" &&
      is_number_node(condition$right, 0) &&
      !is.null(assignment) && is_number_node(assignment$value, 0)

    if (floors_at_zero) {
      return(draft("needs_review", "dv_clear_sentinels",
                   args = list(value_col = statements[[1L]]$value$name),
                   why = "spec floors every negative at zero (decision D1)"))
    }
  }

  # --- household any-true written as "IF ANY(flag) = 1" -----------------------

  if (length(statements) == 1L && node_is(statements[[1L]], "if") && is_household &&
      length(statements[[1L]]$branches[[1L]]$body) == 0L) {
    condition <- statements[[1L]]$branches[[1L]]$condition

    is_any_flag <- node_is(condition, "bin") && condition$op == "==" &&
      is_number_node(condition$right, 1) &&
      node_is(condition$left, "call") && condition$left$fn %in% c("ANY", "MAX") &&
      length(condition$left$args) == 1L && node_is(condition$left$args[[1L]], "name")

    if (is_any_flag) {
      return(draft("auto", "dv_any_to_household",
                   args = list(flag_col = condition$left$args[[1L]]$name)))
    }
  }

  # --- period conversion -------------------------------------------------------

  chain <- period_chain(statements, dv)

  if (!is.null(chain)) {
    differences <- compare_period_table(chain$codes, chain$factors, chain$target)

    uses_spec_table <- length(chain$codes) > 0L

    why <- if (!uses_spec_table) {
      "the spec's period multipliers could not be read, so the was.utils table is drafted (decision D4)"
    } else if (is.null(differences) || length(differences) == 0L) {
      "uses the spec's own period codes"
    } else {
      paste0("uses the spec's own period codes, which differ from was.utils (decision D4): ",
             paste(differences, collapse = "; "))
    }

    if (length(chain$wrappers) > 0L) {
      wrapped <- tryCatch(
        paste(vapply(chain$wrappers, condition_to_r, character(1L), rename = rename),
              collapse = " and "),
        untranslatable = function(problem) "further conditions"
      )
      why <- c(why, paste("spec only converts when", wrapped))
    }

    if (length(chain$guards) > 0L) {
      why <- c(why, paste("spec", chain$guards))
    }

    annualise_args <- list(amount_col = chain$amount, period_col = chain$period,
                           target = chain$target)

    if (uses_spec_table) {
      first_mention <- !duplicated(chain$codes)
      annualise_args$period_codes <- chain$codes[first_mention]
      annualise_args$multipliers <- chain$factors[first_mention]
    }

    return(draft("needs_review", "dv_annualise", args = annualise_args, why = why))
  }

  # --- a single IF with one branch ----------------------------------------------

  if (length(statements) == 1L && node_is(statements[[1L]], "if") &&
      length(statements[[1L]]$branches) == 1L) {
    condition <- statements[[1L]]$branches[[1L]]$condition
    otherwise <- statements[[1L]]$otherwise

    then_assign <- single_assignment(statements[[1L]]$branches[[1L]]$body)
    else_assign <- if (length(otherwise) > 0L) single_assignment(otherwise) else NULL
    then_value <- if (is.null(then_assign)) NULL else then_assign$value
    else_value <- if (is.null(else_assign)) NULL else else_assign$value

    if (is_number_node(then_value, 1) && is_number_node(else_value, 2)) {
      return(draft("hand_written", why = "codes 1/2 rather than 1/0 (decision D7)"))
    }

    if (is_number_node(then_value, 0) && is_number_node(else_value, 1)) {
      return(with_condition(
        "dv_flag_if",
        list(type = "not", x = condition),
        args = list(otherwise = 0),
        why = "spec sets 0 when the condition holds; drafted as its negation"
      ))
    }

    if (is_number_node(then_value, 1)) {
      if (is_number_node(else_value, 0)) {
        any_of <- match_any_of(condition)

        if (!is.null(any_of)) {
          return(draft("auto", "dv_flag_any_of",
                       args = list(value_cols = any_of$columns,
                                   match_values = any_of$values)))
        }

        if (node_is(condition, "bin") && condition$op == ">" &&
            node_is(condition$left, "name") && is_number_node(condition$right)) {
          return(draft("auto", "dv_flag_positive",
                       args = list(value_col = condition$left$name,
                                   threshold = condition$right$value)))
        }

        return(with_condition("dv_flag_if", condition, args = list(otherwise = 0)))
      }

      if (is.null(otherwise)) {
        return(with_condition(
          "dv_flag_if", condition,
          args = list(otherwise = 0),
          why = "no else branch: drafted as 0 where the condition is false, as the published DVs are (decision D5)"
        ))
      }
    }

    if (!is.null(then_value) && is_number_node(else_value, 0)) {
      is_ratio <- node_is(then_value, "bin") && then_value$op == "/" &&
        node_is(condition, "bin") && condition$op == ">" &&
        is_number_node(condition$right, 0) && node_is(condition$left, "name") &&
        node_is(then_value$right, "name") &&
        tolower(then_value$right$name) == tolower(condition$left$name)

      if (is_ratio) {
        if (node_is(then_value$left, "name")) {
          return(draft("auto", "dv_ratio",
                       args = list(numerator_col = then_value$left$name,
                                   denominator_col = then_value$right$name)))
        }

        return(draft("hand_written", why = "ratio with a calculated numerator"))
      }

      if (node_is(then_value, "name")) {
        return(with_condition("dv_keep_if", condition,
                              args = list(value_col = then_value$name, otherwise = 0)))
      }
    }

    clears_one_code <- is_number_node(then_value, 0) && node_is(else_value, "name") &&
      node_is(condition, "bin") && condition$op == "==" &&
      node_is(condition$left, "name") && is_number_node(condition$right) &&
      condition$right$value < 0 &&
      tolower(condition$left$name) == tolower(else_value$name)

    if (clears_one_code) {
      return(draft("needs_review", "dv_clear_sentinels",
                   args = list(value_col = else_value$name),
                   why = paste0("spec clears only ", format_number(condition$right$value),
                                " (decision D1)")))
    }

    if (node_is(then_value, "name") && is.null(otherwise)) {
      return(with_condition(
        "dv_keep_if", condition,
        args = list(value_col = then_value$name, otherwise = 0),
        why = "no else branch: drafted as 0 where the condition is false, as the published DVs are (decision D5)"
      ))
    }
  }

  # --- guarded ratio: "else if denominator = 0 then 0", no final else -----------

  if (length(statements) == 1L && node_is(statements[[1L]], "if") &&
      length(statements[[1L]]$branches) == 2L &&
      length(statements[[1L]]$otherwise) == 0L) {
    first <- statements[[1L]]$branches[[1L]]
    second <- statements[[1L]]$branches[[2L]]
    first_assign <- single_assignment(first$body)
    second_assign <- single_assignment(second$body)
    condition <- first$condition

    is_guarded_ratio <- !is.null(first_assign) && !is.null(second_assign) &&
      is_number_node(second_assign$value, 0) &&
      node_is(condition, "bin") && condition$op == ">" &&
      is_number_node(condition$right, 0) && node_is(condition$left, "name") &&
      node_is(first_assign$value, "bin") && first_assign$value$op == "/" &&
      node_is(first_assign$value$left, "name") &&
      node_is(first_assign$value$right, "name") &&
      tolower(first_assign$value$right$name) == tolower(condition$left$name)

    if (is_guarded_ratio) {
      return(draft("needs_review", "dv_ratio",
                   args = list(numerator_col = first_assign$value$left$name,
                               denominator_col = first_assign$value$right$name),
                   why = "spec gives no value for a negative denominator"))
    }
  }

  draft("hand_written", why = "no verb matches this shape")
}


#' Recognise "any of these columns holds this value"
#' @keywords internal
#' @noRd
match_any_of <- function(condition) {
  if (node_is(condition, "bin") && condition$op == "==" &&
      is_number_node(condition$right)) {
    left <- condition$left

    if (node_is(left, "call") && left$fn == "ANY" && length(left$args) == 1L &&
        node_is(left$args[[1L]], "name") &&
        nrow(find_number_ranges(left$args[[1L]]$name)) > 0L) {
      return(list(columns = left$args[[1L]]$name, values = condition$right$value))
    }
  }

  if (node_is(condition, "call") && condition$fn == "ANY" && length(condition$args) >= 2L) {
    first <- condition$args[[1L]]
    rest <- condition$args[-1L]

    if (node_is(first, "num") && all(vapply(rest, node_is, logical(1L), type = "name"))) {
      return(list(
        columns = vapply(rest, function(item) item$name, character(1L)),
        values = first$value
      ))
    }
  }

  collect <- function(node) {
    if (node_is(node, "bin") && node$op == "|") {
      left <- collect(node$left)
      right <- collect(node$right)

      if (is.null(left) || is.null(right)) {
        return(NULL)
      }

      return(rbind(left, right))
    }

    if (node_is(node, "bin") && node$op == "==" && node_is(node$left, "name") &&
        is_number_node(node$right)) {
      return(data.frame(name = node$left$name, value = node$right$value,
                        stringsAsFactors = FALSE))
    }

    NULL
  }

  leaves <- collect(condition)

  if (!is.null(leaves) && nrow(leaves) >= 2L && length(unique(leaves$value)) == 1L) {
    return(list(columns = leaves$name, values = leaves$value[[1L]]))
  }

  NULL
}


#' Find a period conversion
#' @keywords internal
#' @noRd
period_chain <- function(statements, dv) {
  found <- period_chain_here(statements, dv)

  if (!is.null(found)) {
    return(found)
  }

  for (statement in statements) {
    if (!node_is(statement, "if")) {
      next
    }

    for (branch in statement$branches) {
      found <- period_chain(branch$body, dv)

      if (!is.null(found)) {
        found$wrappers <- c(list(branch$condition), found$wrappers)
        return(found)
      }
    }

    if (length(statement$otherwise) > 0L) {
      found <- period_chain(statement$otherwise, dv)

      if (!is.null(found)) {
        return(found)
      }
    }
  }

  NULL
}


#' Find a period conversion among sibling statements
#' @keywords internal
#' @noRd
period_chain_here <- function(statements, dv) {
  branches <- list()
  guards <- character()

  for (statement in statements) {
    if (node_is(statement, "assign")) {
      if (length(tree_names(statement$value)) > 0L) {
        return(NULL)
      }

      guards <- c(guards, if (node_is(statement$value, "num")) {
        paste("starts from", format_number(statement$value$value))
      } else {
        "has an initialiser"
      })

      next
    }

    if (!node_is(statement, "if")) {
      return(NULL)
    }

    branches <- c(branches, statement$branches)
  }

  is_period_test <- function(condition) {
    node_is(condition, "bin") && condition$op == "==" &&
      node_is(condition$left, "name") && is_number_node(condition$right)
  }

  period <- NULL
  amount <- NULL
  multipliers <- list()

  for (branch in branches) {
    condition <- branch$condition
    assignment <- single_assignment(branch$body)

    if (is.null(assignment)) {
      return(NULL)
    }

    # A guard changes a variable its own condition reads:
    # IF DHPins < 0 THEN DHPins = 0
    target <- assignment$target

    if (!is.na(target) && !same_variable(target, dv) &&
        any(same_variable(target, tree_names(condition)))) {
      guards <- c(guards, paste("changes", target, "before converting"))
      next
    }

    value <- assignment$value
    value_names <- tree_names(value)

    if (!is_period_test(condition)) {
      if (length(value_names) == 0L) {
        next
      }

      return(NULL)
    }

    # "IF amount = 0 THEN dv = 0", or a sentinel passed straight through
    if (length(unique(tolower(value_names))) != 1L) {
      next
    }

    if (is.null(period) && tolower(condition$left$name) != tolower(value_names[[1L]])) {
      period <- condition$left$name
    }

    if (is.null(period) || tolower(condition$left$name) != tolower(period)) {
      next
    }

    if (is.null(amount)) {
      amount <- value_names[[1L]]
    }

    multipliers[[length(multipliers) + 1L]] <- value
  }

  if (is.null(period) || length(multipliers) < 3L) {
    return(NULL)
  }

  # Monthly when a branch divides the result by 12 or 24: (amount * 52) / 12
  monthly <- any(vapply(
    multipliers,
    FUN = function(value) {
      node_is(value, "bin") && value$op == "/" &&
        is_number_node(value$right) && value$right$value %in% c(12, 24)
    },
    FUN.VALUE = logical(1L)
  ))

  codes <- numeric()
  factors <- numeric()

  for (branch in branches) {
    assignment <- single_assignment(branch$body)
    condition <- branch$condition

    if (is.null(assignment) || !is_period_test(condition) ||
        tolower(condition$left$name) != tolower(period)) {
      next
    }

    factor <- evaluate_multiplier(assignment$value, amount)

    if (!is.null(factor)) {
      codes <- c(codes, condition$right$value)
      factors <- c(factors, factor)
    }
  }

  list(
    period = period,
    amount = amount,
    target = if (monthly) "monthly" else "annual",
    guards = guards,
    wrappers = list(),
    codes = codes,
    factors = factors
  )
}


#' Compare a spec's period table with was.utils
#' @keywords internal
#' @noRd
compare_period_table <- function(codes, factors, target) {
  if (!requireNamespace("was.utils", quietly = TRUE)) {
    return(NULL)
  }

  expected <- was.utils::calc_annual_multiplier(codes)

  if (identical(target, "monthly")) {
    expected <- expected / 12
  }

  describe <- function(value) formatC(value, digits = 4L, format = "g")

  missing_code <- is.na(expected)
  differs <- !missing_code & abs(factors - expected) > 1e-9 * pmax(1, abs(expected))

  # paste0() turns a zero-length vector into one empty string, so each part is
  # built only when there is something to describe.
  not_known <- if (any(missing_code)) {
    paste0("code ", format_number(codes[missing_code]), " is not in was.utils")
  } else {
    character()
  }

  different <- if (any(differs)) {
    paste0("code ", format_number(codes[differs]), " is x",
           describe(factors[differs]), " here, x", describe(expected[differs]),
           " in was.utils")
  } else {
    character()
  }

  c(not_known, different)
}
