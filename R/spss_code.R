#' SPSS syntax for one step of the suite
#'
#' Written into the exported scripts as comments above each DV's R, so readers
#' who know SPSS can see what the R does. Each verb has its SPSS written out with
#' every setting inlined, giving the same values as the R, missing values
#' included. A hand-written step, or a condition using R with no SPSS written
#' here, gets a note pointing to the R instead.
#'
#' Assumes -8 and -9 are ordinary values in SPSS (not declared MISSING VALUES)
#' and that no WEIGHT is on, since weights change AGGREGATE sums.
#'
#' @return SPSS lines, without comment marks.
#' @keywords internal
#' @noRd
spss_step_code <- function(step) {
  call <- verb_call_of(step$derive)

  if (is.null(call)) {
    return("* No SPSS given: this DV is written in R by hand, see the R below.")
  }

  args <- resolve_verb_args(call, step$derive)
  emit <- spss_emitters()[[call$verb]]

  if (is.null(emit)) {
    return(paste0("* No SPSS given for ", call$verb, "(), see the R below."))
  }

  translated <- TRUE
  lines <- tryCatch(
    emit(args, step$dv),
    spss_untranslatable = function(problem) {
      translated <<- FALSE
      paste0("* No SPSS given: ", conditionMessage(problem), ", see the R below.")
    }
  )

  if (translated && !is.null(step$missing_code)) {
    lines <- spss_missing_code_lines(lines, setdiff(step$inputs, step$dv), step$dv, step$missing_code)
  }

  c(lines, if (!is.na(step$label)) paste0("VARIABLE LABELS ", step$dv, " ", spss_text(step$label), "."))
}


#' SPSS for a step with -8/-9 inputs as missing, the inputs put back, and missing in the DV coded
#' @keywords internal
#' @noRd
spss_missing_code_lines <- function(lines, inputs, dv, missing_code) {
  # Kept in tmp_ variables, not #scratch ones, which AGGREGATE would discard.
  kept <- paste0("tmp_given", seq_along(inputs))

  c(
    paste0("* -8 and -9 in the inputs count as missing while this DV is worked out; missing in it is written as ",
           spss_number(missing_code), "."),
    if (length(inputs) > 0L) paste0("COMPUTE ", kept, " = ", inputs, "."),
    if (length(inputs) > 0L) spss_wrap(paste0("RECODE ", paste(inputs, collapse = " "), " (-8, -9 = SYSMIS).")),
    lines,
    if (length(inputs) > 0L) paste0("COMPUTE ", inputs, " = ", kept, "."),
    paste0("RECODE ", dv, " (SYSMIS = ", spss_number(missing_code), ")."),
    if (length(inputs) > 0L) c("EXECUTE.", spss_wrap(paste0("DELETE VARIABLES ", paste(kept, collapse = " "), ".")))
  )
}


#' Stop a translation that has no SPSS, so the step gets a note instead
#' @keywords internal
#' @noRd
spss_untranslatable <- function(message) {
  stop(errorCondition(message, class = "spss_untranslatable"))
}


#' SPSS for a number: 2500, 0.25, $SYSMIS for NA
#' @keywords internal
#' @noRd
spss_number <- function(value) {
  if (!is.numeric(value) && !is.logical(value)) {
    spss_untranslatable(paste0("the value ", deparse(value), " is not a number"))
  }

  value <- as.numeric(value)
  # One at a time, so 0.25 in a vector does not turn 52 into 52.00
  vapply(value, function(number) {
    if (is.na(number)) "$SYSMIS" else format(number, scientific = FALSE, trim = TRUE, digits = 15)
  }, character(1L), USE.NAMES = FALSE)
}


#' SPSS for a quoted string, with quotes inside doubled
#' @keywords internal
#' @noRd
spss_text <- function(text) {
  paste0("\"", gsub("\"", "\"\"", text, fixed = TRUE), "\"")
}


#' Break a long command into lines at spaces; lines after the first are indented
#' @keywords internal
#' @noRd
spss_wrap <- function(text, width = 76L) {
  lines <- strwrap(text, width = width)
  c(lines[[1L]], if (length(lines) > 1L) paste0("  ", lines[-1L]))
}


#' ANY(x, -8, -9), true only when x has a value in the list, as R's %in%
#'
#' SPSS's ANY() is missing when x is missing, where R's %in% is false, so the
#' guard keeps the two the same.
#' @keywords internal
#' @noRd
spss_in <- function(variable, values) {
  paste0("NOT MISSING(", variable, ") AND ANY(", variable, ", ", paste(spss_number(values), collapse = ", "), ")")
}


#' What to do with -8/-9 codes held in `variable`, in SPSS
#' @keywords internal
#' @noRd
spss_sentinel_lines <- function(variable, sentinels, source_name = variable) {
  codes <- paste(spss_number(sentinel_codes()), collapse = ", ")

  switch(
    sentinels,
    keep = character(),
    zero = paste0("IF (ANY(", variable, ", ", codes, ")) ", variable, " = 0."),
    missing = paste0("IF (ANY(", variable, ", ", codes, ")) ", variable, " = $SYSMIS."),
    stop = paste0("* The R stops if ", source_name, " holds -8 or -9, so check it has none first."),
    stop("Unknown sentinels setting: ", sentinels, call. = FALSE)
  )
}


#' new = yes where the condition holds, no where it does not, missing where it is missing
#'
#' DO IF skips the whole structure when its condition is missing, so the DV is
#' cleared first, as R's ifelse() gives NA there.
#' @keywords internal
#' @noRd
spss_if_else <- function(new, condition, yes, no) {
  c(
    paste0("COMPUTE ", new, " = $SYSMIS."),
    spss_wrap(paste0("DO IF (", condition, ").")),
    paste0("  COMPUTE ", new, " = ", yes, "."),
    "ELSE.",
    paste0("  COMPUTE ", new, " = ", no, "."),
    "END IF."
  )
}


#' RECODE source (1=0) (2=2500) ... (ELSE=SYSMIS) INTO new.
#' @keywords internal
#' @noRd
spss_recode_lines <- function(source, table, new) {
  keys <- suppressWarnings(as.numeric(names(table)))

  if (anyNA(keys)) {
    spss_untranslatable("the lookup table has codes that are not numbers")
  }

  targets <- sub("^\\$", "", spss_number(unname(table)))
  pairs <- paste0("(", spss_number(keys), "=", targets, ")")
  spss_wrap(paste0("RECODE ", source, " ", paste(pairs, collapse = " "), " (ELSE=SYSMIS) INTO ", new, "."))
}


#' AGGREGATE a new household variable onto every row
#'
#' SPSS will not delete variables while transformations are pending, so the
#' emitters put EXECUTE before deleting their temporary variables.
#' @keywords internal
#' @noRd
spss_aggregate_lines <- function(by, results) {
  c(
    "AGGREGATE /OUTFILE=* MODE=ADDVARIABLES OVERWRITE=YES",
    paste0("  /BREAK=", paste(by, collapse = " ")),
    paste0("  /", results, c(rep("", length(results) - 1L), "."))
  )
}


#' An R condition written as SPSS: df$a == 1 & df$b %in% c(2, 3) -> a = 1 AND ...
#'
#' Covers what the plan writes: comparisons, &, |, !, %in% and is.na() on
#' columns and numbers. Anything else has no SPSS here.
#' @keywords internal
#' @noRd
spss_condition <- function(text) {
  operators <- c(
    "&" = "AND", "&&" = "AND", "|" = "OR", "||" = "OR",
    "==" = "=", "!=" = "~=", ">" = ">", ">=" = ">=", "<" = "<", "<=" = "<=",
    "+" = "+", "-" = "-", "*" = "*", "/" = "/"
  )
  logical_operators <- c("&", "&&", "|", "||")

  # (a | b) | c is written a OR b OR c: the brackets change nothing
  unwrap_same <- function(operand, operator) {
    if (is.call(operand) && identical(operand[[1L]], as.name("(")) && is.call(operand[[2L]]) &&
        as.character(operand[[2L]][[1L]]) %in% logical_operators &&
        operators[[as.character(operand[[2L]][[1L]])]] == operators[[operator]]) {
      operand[[2L]]
    } else {
      operand
    }
  }

  translate <- function(expr) {
    if (is.numeric(expr) || is.logical(expr)) {
      return(spss_number(expr))
    }

    if (is.symbol(expr)) {
      return(as.character(expr))
    }

    if (!is.call(expr) || !is.symbol(expr[[1L]])) {
      spss_untranslatable(paste0("the condition uses ", paste(deparse(expr), collapse = " ")))
    }

    fn <- as.character(expr[[1L]])

    if (fn %in% c("$", "[[") && identical(expr[[2L]], as.name("df"))) {
      return(as.character(expr[[3L]]))
    }

    if (fn == "(") {
      return(paste0("(", translate(strip_brackets(expr[[2L]])), ")"))
    }

    # NOT needs brackets round anything longer than one term
    if (fn == "!") {
      operand <- strip_brackets(expr[[2L]])
      simple <- !is.call(operand) || as.character(operand[[1L]]) %in% c("is.na", "$", "[[")
      return(paste0("NOT ", if (simple) translate(operand) else paste0("(", translate(operand), ")")))
    }

    if (fn == "-" && length(expr) == 2L) {
      return(paste0("-", translate(expr[[2L]])))
    }

    if (fn == "is.na") {
      return(paste0("MISSING(", translate(expr[[2L]]), ")"))
    }

    if (fn == "%in%") {
      values <- tryCatch(eval(expr[[3L]], baseenv()), error = function(problem) NULL)

      if (!is.numeric(values) || length(values) == 0L) {
        spss_untranslatable("the condition uses %in% with values that are not numbers")
      }

      return(spss_in(translate(expr[[2L]]), values))
    }

    if (fn %in% names(operators) && length(expr) == 3L) {
      left <- expr[[2L]]
      right <- expr[[3L]]

      if (fn %in% logical_operators) {
        left <- unwrap_same(left, fn)
        right <- unwrap_same(right, fn)
      }

      # A %in% inside OR is bracketed, so its AND does not read as joining the OR
      side <- function(operand) {
        text <- translate(operand)
        in_or <- operators[[fn]] == "OR" && is.call(operand) && identical(operand[[1L]], as.name("%in%"))
        if (in_or) paste0("(", text, ")") else text
      }

      return(paste(side(left), operators[[fn]], side(right)))
    }

    spss_untranslatable(paste0("the condition uses ", fn, "()"))
  }

  strip_brackets <- function(expr) {
    while (is.call(expr) && identical(expr[[1L]], as.name("("))) {
      expr <- expr[[2L]]
    }
    expr
  }

  # DO IF and IF put their own brackets round the condition
  translate(strip_brackets(parse(text = text, keep.source = FALSE)[[1L]]))
}


#' One function per verb: the SPSS that does the same as the verb
#' @keywords internal
#' @noRd
spss_emitters <- function() {
  list(
    dv_copy = function(args, dv) {
      c(
        paste0("COMPUTE ", dv, " = ", args$source_col, "."),
        spss_sentinel_lines(dv, args$sentinels, args$source_col)
      )
    },

    dv_clear_sentinels = function(args, dv) {
      c(
        paste0("COMPUTE ", dv, " = ", args$value_col, "."),
        paste0("IF (ANY(", dv, ", ", paste(spss_number(args$codes), collapse = ", "), ")) ", dv, " = ",
               spss_number(args$replacement), ".")
      )
    },

    dv_row_total = function(args, dv) {
      stops <- identical(args$sentinels, "stop")
      c(
        if (stops) spss_sentinel_lines("#part", "stop", "any of these columns"),
        paste0("COMPUTE ", dv, " = 0."),
        spss_wrap(paste0("DO REPEAT column = ", paste(args$value_cols, collapse = " "), ".")),
        "  COMPUTE #part = column.",
        if (!stops && args$sentinels != "keep") paste0("  ", spss_sentinel_lines("#part", args$sentinels)),
        if (isTRUE(args$na_as_zero)) "  IF (MISSING(#part)) #part = 0.",
        paste0("  COMPUTE ", dv, " = ", dv, " + #part."),
        "END REPEAT."
      )
    },

    dv_difference = function(args, dv) {
      c(
        paste0("COMPUTE #first = ", args$minuend_col, "."),
        paste0("COMPUTE #second = ", args$subtrahend_col, "."),
        spss_sentinel_lines("#first", args$sentinels, args$minuend_col),
        spss_sentinel_lines("#second", args$sentinels, args$subtrahend_col),
        paste0("COMPUTE ", dv, " = #first - #second.")
      )
    },

    # AGGREGATE's SUM skips missing values where R's sum() gives NA, so without
    # na_as_zero a household with any missing value is set missing afterwards.
    dv_sum_to_household = function(args, dv) {
      counts_missing <- !isTRUE(args$na_as_zero)
      c(
        paste0("COMPUTE tmp_amount = ", args$value_col, "."),
        spss_sentinel_lines("tmp_amount", args$sentinels, args$value_col),
        if (!counts_missing) "IF (MISSING(tmp_amount)) tmp_amount = 0.",
        spss_aggregate_lines(args$by, c(paste0(dv, " = SUM(tmp_amount)"),
                                        if (counts_missing) "tmp_missing = NUMISS(tmp_amount)")),
        if (counts_missing) c(paste0("IF (tmp_missing > 0) ", dv, " = $SYSMIS."), "EXECUTE."),
        paste0("DELETE VARIABLES tmp_amount", if (counts_missing) " tmp_missing", ".")
      )
    },

    dv_any_to_household = function(args, dv) {
      c(
        "COMPUTE tmp_flag = 0.",
        paste0("IF (", args$flag_col, " = 1) tmp_flag = 1."),
        spss_aggregate_lines(args$by, c(paste0(dv, " = MAX(tmp_flag)"),
                                        paste0("tmp_answered = NU(", args$flag_col, ")"))),
        paste0("IF (tmp_answered = 0) ", dv, " = $SYSMIS."),
        "EXECUTE.",
        "DELETE VARIABLES tmp_flag tmp_answered."
      )
    },

    dv_flag_positive = function(args, dv) {
      c(
        paste0("COMPUTE #value = ", args$value_col, "."),
        spss_sentinel_lines("#value", args$sentinels, args$value_col),
        spss_if_else(dv, paste0("#value > ", spss_number(args$threshold)), "1", "0")
      )
    },

    dv_flag_if = function(args, dv) {
      spss_if_else(dv, spss_condition(args$condition), "1", spss_number(args$otherwise))
    },

    dv_keep_if = function(args, dv) {
      condition <- spss_condition(args$condition)
      c(
        paste0("COMPUTE #value = ", args$value_col, "."),
        spss_sentinel_lines("#value", args$sentinels, args$value_col),
        spss_if_else(dv, condition, "#value", spss_number(args$otherwise))
      )
    },

    dv_flag_any_of = function(args, dv) {
      c(
        paste0("COMPUTE ", dv, " = 0."),
        spss_wrap(paste0("DO REPEAT column = ", paste(args$value_cols, collapse = " "), ".")),
        paste0("  IF (ANY(column, ", paste(spss_number(args$match_values), collapse = ", "), ")) ", dv, " = 1."),
        "END REPEAT.",
        spss_wrap(paste0("IF (NVALID(", paste(args$value_cols, collapse = ", "), ") = 0) ", dv, " = $SYSMIS."))
      )
    },

    dv_ratio = function(args, dv) {
      c(
        paste0("COMPUTE #top = ", args$numerator_col, "."),
        paste0("COMPUTE #bottom = ", args$denominator_col, "."),
        spss_sentinel_lines("#top", args$sentinels, args$numerator_col),
        spss_sentinel_lines("#bottom", args$sentinels, args$denominator_col),
        paste0("COMPUTE ", dv, " = ", spss_number(args$otherwise), "."),
        paste0("IF (#bottom > 0) ", dv, " = #top / #bottom."),
        paste0("IF (MISSING(#top) OR MISSING(#bottom)) ", dv, " = $SYSMIS.")
      )
    },

    # SPSS gives 0 / missing = 0 where R gives NA, hence the last-but-one line.
    dv_discount_to_present_value = function(args, dv) {
      if (is.null(args$rate) || length(args$rate) != 1L || is.na(args$rate)) {
        spss_untranslatable("the discount rate is not set yet")
      }

      c(
        paste0("COMPUTE #amount = ", args$amount_col, "."),
        paste0("COMPUTE #age = ", args$age_col, "."),
        spss_sentinel_lines("#amount", args$sentinels, args$amount_col),
        spss_sentinel_lines("#age", args$sentinels, args$age_col),
        paste0("COMPUTE #years = ", spss_number(args$target_age), " - #age."),
        paste0("COMPUTE ", dv, " = #amount / (1 + ", spss_number(args$rate), ") ** #years."),
        paste0("IF (MISSING(#years)) ", dv, " = $SYSMIS."),
        paste0("IF (#years <= 0) ", dv, " = $SYSMIS.")
      )
    },

    dv_band_midpoint = function(args, dv) {
      band <- args$band_col
      codes <- paste(spss_number(sentinel_codes()), collapse = ", ")
      c(
        spss_recode_lines(band, band_midpoint_tables()[[args$table]], dv),
        paste0("IF (", band, " < 0) ", dv, " = ", band, "."),
        switch(
          args$sentinels,
          keep = character(),
          zero = paste0("IF (ANY(", band, ", ", codes, ")) ", dv, " = 0."),
          missing = paste0("IF (ANY(", band, ", ", codes, ")) ", dv, " = $SYSMIS."),
          stop = spss_sentinel_lines(band, "stop")
        )
      )
    },

    # The multiplier is recoded into the DV itself, then multiplied by the amount.
    dv_annualise = function(args, dv) {
      if (is.null(args$period_codes)) {
        spss_untranslatable("the period table is not given")
      }

      amount <- args$amount_col
      period <- args$period_col
      table <- stats::setNames(args$multipliers, as.character(args$period_codes))
      either <- "#amount_missing OR #period_missing"
      c(
        spss_recode_lines(period, table, dv),
        paste0("COMPUTE ", dv, " = ", amount, " * ", dv, "."),
        paste0("COMPUTE #amount_zero = NOT MISSING(", amount, ") AND ", amount, " = 0."),
        paste0("COMPUTE #amount_missing = ", spss_in(amount, sentinel_codes()), "."),
        spss_wrap(paste0("COMPUTE #period_missing = ", spss_in(period, sentinel_codes()),
                         " AND NOT #amount_missing AND NOT #amount_zero.")),
        paste0("IF (#amount_zero) ", dv, " = 0."),
        paste0("IF (#amount_missing) ", dv, " = ", amount, "."),
        paste0("IF (#period_missing) ", dv, " = ", period, "."),
        switch(
          args$sentinels,
          keep = character(),
          zero = paste0("IF (", either, ") ", dv, " = 0."),
          missing = paste0("IF (", either, ") ", dv, " = $SYSMIS."),
          stop = paste0("* The R stops if ", amount, " or ", period, " holds -8 or -9, so check they have none first.")
        )
      )
    }
  )
}
