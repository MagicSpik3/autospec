#' Plain R for one step of the suite
#'
#' A step whose `derive` is a single call to a verb becomes the same
#' calculation written out in base R, with every setting inlined. A step
#' written by hand is copied as it is, with `settings$...` replaced by the
#' values. Either way the result reads and writes `df` and nothing else.
#'
#' @return A list: `lines` of R code, and `needs`, the columns the code reads.
#' @keywords internal
#' @noRd
plain_step_code <- function(step, settings) {
  code <- plain_step_body(step, settings)

  if (!is.null(step$missing_code)) {
    code$lines <- plain_missing_code_lines(code$lines, setdiff(step$inputs, step$dv), step$dv, step$missing_code)
  }

  code
}


#' A step's code with -8/-9 inputs as missing, the inputs put back, and missing in the DV coded
#' @keywords internal
#' @noRd
plain_missing_code_lines <- function(lines, inputs, dv, missing_code) {
  code_text <- format_number(missing_code)

  c(
    paste0("# -8 and -9 in the inputs count as missing while this DV is worked out; missing in it is written as ", code_text),
    paste0("inputs_as_given <- df[", literal(inputs), "]"),
    paste0("for (column in names(inputs_as_given)) df[[column]][df[[column]] %in% ", literal(sentinel_codes()), "] <- NA"),
    lines,
    "df[names(inputs_as_given)] <- inputs_as_given",
    paste0(col_ref(dv), "[is.na(", col_ref(dv), ")] <- ", code_text)
  )
}


#' The plain R of a step's calculation, before any missing_code handling
#' @keywords internal
#' @noRd
plain_step_body <- function(step, settings) {
  call <- verb_call_of(step$derive)

  if (is.null(call)) {
    return(plain_hand_written(step, settings))
  }

  args <- resolve_verb_args(call, step$derive)
  emit <- plain_emitters()[[call$verb]]

  if (is.null(emit)) {
    stop(step$dv, ": no plain R is known for ", call$verb, "().", call. = FALSE)
  }

  lines <- emit(args, step$dv)
  needs <- unlist(args[names(args) %in% c("source_col", "value_col", "value_cols", "minuend_col",
                                          "subtrahend_col", "flag_col", "numerator_col",
                                          "denominator_col", "amount_col", "age_col", "period_col",
                                          "band_col", "by")], use.names = FALSE)
  needs <- unique(c(step$inputs, needs))

  list(lines = lines, needs = needs)
}


#' The verb call a step's derive makes, if it is nothing but that call
#' @keywords internal
#' @noRd
verb_call_of <- function(derive) {
  code <- body(derive)
  statements <- if (is.call(code) && identical(code[[1L]], as.name("{"))) as.list(code)[-1L] else list(code)

  if (length(statements) != 1L) {
    return(NULL)
  }

  call <- statements[[1L]]

  if (!is.call(call) || !is.symbol(call[[1L]]) || !as.character(call[[1L]]) %in% registered_verbs()) {
    return(NULL)
  }

  verb <- as.character(call[[1L]])
  matched <- match.call(get(verb, envir = asNamespace("wealthdv")), call)

  list(verb = verb, args = as.list(matched)[-1L])
}


#' Evaluate a verb call's arguments, except the condition, which stays as code
#'
#' Arguments the call leaves out take the verb's own default, so plain R is
#' written for what the verb would actually do.
#' @keywords internal
#' @noRd
resolve_verb_args <- function(call, derive) {
  verb_defaults <- formals(get(call$verb, envir = asNamespace("wealthdv")))
  verb_defaults <- verb_defaults[vapply(verb_defaults, function(default) !identical(default, quote(expr = )), logical(1L))]

  supplied <- call$args[setdiff(names(call$args), "df")]
  args <- c(supplied, verb_defaults[setdiff(names(verb_defaults), names(supplied))])
  where <- environment(derive)

  lapply(stats::setNames(names(args), names(args)), function(name) {
    if (name == "condition") {
      paste(trimws(deparse(args[[name]], width.cutoff = 500L)), collapse = " ")
    } else {
      eval(args[[name]], envir = where)
    }
  })
}


#' A hand-written derive, copied out with settings inlined
#' @keywords internal
#' @noRd
plain_hand_written <- function(step, settings) {
  source_lines <- as.character(attr(step$derive, "srcref"))

  if (length(source_lines) == 0L) {
    source_lines <- deparse(step$derive, width.cutoff = 100L)
  }

  # Drop "function(df) {" and the closing "}", then the line that returns df
  source_lines[1L] <- sub("^\\s*function\\s*\\(\\s*df\\s*\\)\\s*\\{?\\s*", "", source_lines[1L])
  source_lines[length(source_lines)] <- sub("\\}\\s*$", "", source_lines[length(source_lines)])
  source_lines <- source_lines[!grepl("^\\s*$", source_lines) | seq_along(source_lines) < length(source_lines)]
  source_lines <- source_lines[!grepl("^\\s*(return\\(df\\)|df)\\s*$", source_lines)]
  source_lines <- dedent(source_lines)

  verbs_used <- intersect(code_function_names(step$derive), registered_verbs())

  if (length(verbs_used) > 0L) {
    stop(
      step$dv, ": hand-written code calls ", paste(verbs_used, collapse = ", "),
      " as well as other code. Write it as one verb call, or in plain R, so it can be exported.",
      call. = FALSE
    )
  }

  needs <- step$inputs

  if (any(grepl("settings\\$by\\b", source_lines, perl = TRUE))) {
    needs <- unique(c(needs, settings_column_names(settings)))
  }

  for (name in names(settings)) {
    pattern <- paste0("settings\\$", name, "\\b")
    source_lines <- gsub(pattern, literal(settings[[name]]), source_lines, perl = TRUE)
  }

  if (any(grepl("settings\\$", source_lines))) {
    stop(step$dv, ": uses a setting that settings.R does not define.", call. = FALSE)
  }

  list(lines = source_lines, needs = needs)
}


#' @keywords internal
#' @noRd
dedent <- function(lines) {
  filled <- lines[nzchar(trimws(lines))]

  if (length(filled) == 0L) {
    return(lines)
  }

  indents <- nchar(sub("^(\\s*).*$", "\\1", filled))
  sub(paste0("^\\s{0,", min(indents), "}"), "", lines)
}


#' R code for a value: 0, NA, "text", c(1, 2), c(`1` = 2500, ...)
#' @keywords internal
#' @noRd
literal <- function(value) {
  if (is.null(value)) {
    return("NULL")
  }

  # Whole pounds, not 3e+05
  old_options <- options(scipen = 100)
  on.exit(options(old_options))

  paste(deparse(value, width.cutoff = 500L), collapse = " ")
}


#' df$name, backticked when the name needs it
#' @keywords internal
#' @noRd
col_ref <- function(name) {
  paste0("df$", if (make.names(name) == name) name else paste0("`", name, "`"))
}


#' What to do with -8/-9 codes held in `variable`
#' @keywords internal
#' @noRd
sentinel_lines <- function(variable, sentinels, dv, source_name = variable) {
  codes <- literal(sentinel_codes())

  switch(
    sentinels,
    keep = character(),
    zero = paste0(variable, "[", variable, " %in% ", codes, "] <- 0"),
    missing = paste0(variable, "[", variable, " %in% ", codes, "] <- NA"),
    stop = paste0(
      "if (any(", variable, " %in% ", codes, ")) stop(\"", dv, ": ", source_name,
      " holds -8/-9 codes; decide what they should become\")"
    ),
    stop("Unknown sentinels setting: ", sentinels, call. = FALSE)
  )
}


#' One function per verb: the plain R that does the same as the verb
#' @keywords internal
#' @noRd
plain_emitters <- function() {
  codes <- literal(sentinel_codes())

  list(
    dv_copy = function(args, dv) {
      new <- col_ref(dv)
      c(
        paste0(new, " <- ", col_ref(args$source_col)),
        if (identical(args$sentinels, "stop")) {
          sentinel_lines(col_ref(args$source_col), "stop", dv, args$source_col)
        } else {
          sentinel_lines(new, args$sentinels, dv, args$source_col)
        }
      )
    },

    dv_clear_sentinels = function(args, dv) {
      new <- col_ref(dv)
      c(
        paste0(new, " <- ", col_ref(args$value_col)),
        paste0(new, "[", new, " %in% ", literal(args$codes), "] <- ", literal(args$replacement))
      )
    },

    dv_row_total = function(args, dv) {
      new <- col_ref(dv)
      c(
        paste0(new, " <- 0"),
        paste0("for (column in ", literal(args$value_cols), ") {"),
        "  part <- df[[column]]",
        paste0("  ", sentinel_lines("part", args$sentinels, dv, "\", column, \"")),
        if (isTRUE(args$na_as_zero)) "  part[is.na(part)] <- 0",
        paste0("  ", new, " <- ", new, " + part"),
        "}"
      )
    },

    dv_difference = function(args, dv) {
      c(
        paste0("first <- ", col_ref(args$minuend_col)),
        paste0("second <- ", col_ref(args$subtrahend_col)),
        sentinel_lines("first", args$sentinels, dv, args$minuend_col),
        sentinel_lines("second", args$sentinels, dv, args$subtrahend_col),
        paste0(col_ref(dv), " <- first - second")
      )
    },

    dv_sum_to_household = function(args, dv) {
      c(
        paste0("amount <- ", col_ref(args$value_col)),
        sentinel_lines("amount", args$sentinels, dv, args$value_col),
        if (isTRUE(args$na_as_zero)) "amount[is.na(amount)] <- 0",
        paste0(col_ref(dv), " <- ave(amount, paste(", col_ref(args$by), "), FUN = sum)")
      )
    },

    dv_any_to_household = function(args, dv) {
      new <- col_ref(dv)
      c(
        paste0("flag <- ", col_ref(args$flag_col)),
        paste0("household <- paste(", col_ref(args$by), ")"),
        paste0(new, " <- ave(as.numeric(flag %in% 1), household, FUN = max)"),
        paste0(new, "[ave(as.numeric(!is.na(flag)), household, FUN = sum) == 0] <- NA")
      )
    },

    dv_flag_positive = function(args, dv) {
      c(
        paste0("value <- ", col_ref(args$value_col)),
        sentinel_lines("value", args$sentinels, dv, args$value_col),
        paste0(col_ref(dv), " <- ifelse(value > ", literal(args$threshold), ", 1, 0)")
      )
    },

    dv_flag_if = function(args, dv) {
      paste0(col_ref(dv), " <- ifelse(", args$condition, ", 1, ", literal(args$otherwise), ")")
    },

    dv_keep_if = function(args, dv) {
      c(
        paste0("value <- ", col_ref(args$value_col)),
        sentinel_lines("value", args$sentinels, dv, args$value_col),
        paste0(col_ref(dv), " <- ifelse(", args$condition, ", value, ", literal(args$otherwise), ")")
      )
    },

    dv_flag_any_of = function(args, dv) {
      new <- col_ref(dv)
      refs <- vapply(args$value_cols, col_ref, character(1L))
      c(
        paste0("matched <- ", paste0(refs, " %in% ", literal(args$match_values), collapse = " | ")),
        paste0("all_missing <- ", paste0("is.na(", refs, ")", collapse = " & ")),
        paste0(new, " <- as.numeric(matched)"),
        paste0(new, "[all_missing] <- NA")
      )
    },

    dv_ratio = function(args, dv) {
      new <- col_ref(dv)
      c(
        paste0("top <- ", col_ref(args$numerator_col)),
        paste0("bottom <- ", col_ref(args$denominator_col)),
        sentinel_lines("top", args$sentinels, dv, args$numerator_col),
        sentinel_lines("bottom", args$sentinels, dv, args$denominator_col),
        paste0(new, " <- ifelse(bottom > 0, top / bottom, ", literal(args$otherwise), ")"),
        paste0(new, "[is.na(top) | is.na(bottom)] <- NA")
      )
    },

    dv_discount_to_present_value = function(args, dv) {
      new <- col_ref(dv)
      c(
        paste0("amount <- ", col_ref(args$amount_col)),
        paste0("age <- ", col_ref(args$age_col)),
        sentinel_lines("amount", args$sentinels, dv, args$amount_col),
        sentinel_lines("age", args$sentinels, dv, args$age_col),
        paste0("years <- ", literal(args$target_age), " - age"),
        paste0(new, " <- amount / (1 + ", literal(args$rate), ")^years"),
        paste0(new, "[!is.na(years) & years <= 0] <- NA")
      )
    },

    dv_band_midpoint = function(args, dv) {
      new <- col_ref(dv)
      table <- band_midpoint_tables()[[args$table]]
      c(
        paste0("midpoints <- ", literal(table)),
        paste0("code <- ", col_ref(args$band_col)),
        paste0(new, " <- unname(midpoints[as.character(code)])"),
        "negative <- !is.na(code) & code < 0",
        paste0(new, "[negative] <- code[negative]"),
        switch(
          args$sentinels,
          keep = character(),
          zero = paste0(new, "[code %in% ", codes, "] <- 0"),
          missing = paste0(new, "[code %in% ", codes, "] <- NA"),
          stop = sentinel_lines("code", "stop", dv, args$band_col)
        )
      )
    },

    dv_annualise = function(args, dv) {
      if (is.null(args$period_codes)) {
        stop(
          dv, ": dv_annualise() without period_codes uses the was.utils period table, which ",
          "cannot be written out. Give period_codes and multipliers in the plan.",
          call. = FALSE
        )
      }

      new <- col_ref(dv)
      table <- stats::setNames(args$multipliers, as.character(args$period_codes))
      c(
        paste0("amount <- ", col_ref(args$amount_col)),
        paste0("period <- ", col_ref(args$period_col)),
        paste0("multiplier <- ", literal(table)),
        paste0(new, " <- amount * unname(multiplier[as.character(period)])"),
        paste0(new, "[amount %in% 0] <- 0"),
        paste0("amount_missing <- amount %in% ", codes),
        paste0("period_missing <- period %in% ", codes, " & !amount_missing & !amount %in% 0"),
        paste0(new, "[amount_missing] <- amount[amount_missing]"),
        paste0(new, "[period_missing] <- period[period_missing]"),
        switch(
          args$sentinels,
          keep = character(),
          zero = paste0(new, "[amount_missing | period_missing] <- 0"),
          missing = paste0(new, "[amount_missing | period_missing] <- NA"),
          stop = paste0(
            "if (any(amount_missing | period_missing)) stop(\"", dv, ": ", args$amount_col, " or ",
            args$period_col, " holds -8/-9 codes; decide what they should become\")"
          )
        )
      )
    }
  )
}
