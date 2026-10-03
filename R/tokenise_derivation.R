#' @title tokenise_derivation
#'
#' @description
#'
#' Splits a derivation into tokens: keywords, variable names, numbers, strings
#' and operators. This is the first step of [parse_derivation()] and is exposed
#' so that a derivation that will not parse can be inspected piece by piece.
#'
#' The specifications mix SPSS syntax, SAS-like pseudocode and plain English,
#' so the tokeniser is deliberately forgiving:
#'
#' * Keywords (`IF`, `THEN`, `ELSE`, `OTHERWISE`, `AND`, `OR`, `NOT`, `IN`,
#'   `DO`, `END`, `COMPUTE`) are recognised in any case.
#' * SAS comparison words (`NE`, `EQ`, `GT`, `LT`, `GE`, `LE`) become operators.
#' * A variable name may carry a numbered range, as in `DCSC(1-5)R9`, and is
#'   kept as one token.
#' * Numbers written with a thousands comma, such as `12,570`, are read as one
#'   number and flagged in the notes.
#' * A keyword run into the following name, such as `ifHvDBurd_NumR9` or
#'   `ANDActEPAcFrYN2R9_i`, is split in two and flagged in the notes.
#'
#' @param text A single derivation.
#' @param known_names Variable names known to exist. Used only to decide
#'   whether a word such as `ifHvDBurd_NumR9` is a keyword run into a name or a
#'   genuine variable.
#'
#' @return A data frame with one row per token and the columns `kind` (one of
#'   `"kw"`, `"name"`, `"number"`, `"string"`, `"op"`, `"eof"`), `text`,
#'   `position` (character offset in `text`), `newline` (whether a line break
#'   came before the token) and `comma` (whether a number used a thousands
#'   comma). The last row is always an `"eof"` token. Notes about repairs are
#'   attached as the `"notes"` attribute.
#'
#'   A character the tokeniser does not recognise, such as `£`, raises a
#'   condition of class `derivation_parse_error`.
#'
#' @keywords internal
#' @noRd
#'
#' @examples
#' tokenise_derivation("IF CommiR9 in (4,5) THEN BillCredKeepNoR9 = 1")
tokenise_derivation <- function(text, known_names = character()) {
  if (!is.character(text) || length(text) != 1L || is.na(text)) {
    cli::cli_alert_danger("{.arg text} must be a single non-missing string.")
    stop("`text` must be a single non-missing string.", call. = FALSE)
  }

  patterns <- token_patterns()

  combined <- paste0(
    "(?<", names(patterns), ">", patterns, ")",
    collapse = "|"
  )

  matches <- gregexpr(combined, text, perl = TRUE)[[1L]]

  text_length <- nchar(text)

  if (matches[[1L]] == -1L) {
    if (text_length > 0L) {
      raise_parse_error(
        sprintf("unexpected character '%s'", substr(text, 1L, 1L)),
        1L
      )
    }

    return(eof_only_tokens(text_length))
  }

  starts <- as.integer(matches)
  lengths <- attr(matches, "match.length")
  ends <- starts + lengths - 1L

  # Every character must belong to a token. A gap means a character that no
  # pattern recognises.
  expected_starts <- c(1L, utils::head(ends, -1L) + 1L)
  gaps <- which(starts != expected_starts)

  if (length(gaps) > 0L) {
    gap_start <- expected_starts[[gaps[[1L]]]]
    raise_parse_error(
      sprintf("unexpected character '%s'", substr(text, gap_start, gap_start)),
      gap_start
    )
  }

  if (utils::tail(ends, 1L) < text_length) {
    gap_start <- utils::tail(ends, 1L) + 1L
    raise_parse_error(
      sprintf("unexpected character '%s'", substr(text, gap_start, gap_start)),
      gap_start
    )
  }

  capture_starts <- attr(matches, "capture.start")
  # Exactly one named group takes part in each match; the others report -1.
  kinds <- colnames(capture_starts)[
    max.col((capture_starts > 0L) * 1L, ties.method = "first")
  ]

  values <- substring(text, starts, ends)

  known_lower <- tolower(known_names)
  notes <- character()

  keywords <- derivation_keywords()
  word_operators <- c(NE = "!=", EQ = "==", GT = ">", LT = "<", GE = ">=", LE = "<=")

  out_kind <- character()
  out_text <- character()
  out_position <- integer()
  out_newline <- logical()
  out_comma <- logical()

  add_token <- function(kind, value, position, newline, comma = FALSE) {
    out_kind[[length(out_kind) + 1L]] <<- kind
    out_text[[length(out_text) + 1L]] <<- value
    out_position[[length(out_position) + 1L]] <<- position
    out_newline[[length(out_newline) + 1L]] <<- newline
    out_comma[[length(out_comma) + 1L]] <<- comma
  }

  after_newline <- TRUE

  for (index in seq_along(values)) {
    kind <- kinds[[index]]
    value <- values[[index]]
    position <- starts[[index]]

    if (kind == "ws") {
      after_newline <- after_newline || grepl("\n", value, fixed = TRUE)
      next
    }

    if (kind == "word") {
      glued <- split_glued_keyword(value, known_lower)

      if (!is.null(glued)) {
        notes <- c(notes, paste0("keyword run into a variable name: ", value))
        add_token("kw", glued[["keyword"]], position, after_newline)
        add_token("name", glued[["name"]], position, FALSE)
        after_newline <- FALSE
        next
      }

      upper <- toupper(value)

      if (upper %in% keywords) {
        add_token("kw", upper, position, after_newline)
      } else if (upper %in% names(word_operators)) {
        add_token("op", word_operators[[upper]], position, after_newline)
      } else {
        add_token("name", value, position, after_newline)
      }
    } else if (kind == "op") {
      add_token("op", gsub("\\s+", "", value), position, after_newline)
    } else if (kind == "number") {
      add_token("number", value, position, after_newline,
                comma = grepl(",", value, fixed = TRUE))
    } else {
      add_token(kind, value, position, after_newline)
    }

    after_newline <- FALSE
  }

  tokens <- data.frame(
    kind = c(out_kind, "eof"),
    text = c(out_text, ""),
    position = c(out_position, text_length + 1L),
    newline = c(out_newline, TRUE),
    comma = c(out_comma, FALSE),
    stringsAsFactors = FALSE
  )

  attr(tokens, "notes") <- unique(notes)

  tokens
}


#' Token patterns, in priority order
#' @keywords internal
#' @noRd
token_patterns <- function() {
  c(
    ws = "[ \\t\\r\\n]+",
    string = "'[^'\\n]*'|\"[^\"\\n]*\"",
    name = "[A-Za-z_][A-Za-z0-9_]*(?:\\(\\d+\\s*-\\s*\\d+\\)[A-Za-z0-9_]*)+",
    number = "\\d{1,3}(?:,\\d{3})+(?:\\.\\d+)?|\\d+\\.\\d+|\\d+|\\.\\d+",
    word = "[A-Za-z_][A-Za-z0-9_]*(?:\\.[A-Za-z][A-Za-z0-9_]*)*",
    op = "\\*\\*|[<>]\\s*=|<>|!=|~=|==|:=|&&|\\|\\||[=<>+\\-*/^,;:().&|!%]"
  )
}


#' Keywords recognised in derivations
#' @keywords internal
#' @noRd
derivation_keywords <- function() {
  c(
    "IF", "THEN", "ELSE", "ELSEIF", "ELIF", "OTHERWISE", "AND", "OR", "NOT", "IN",
    "DO", "END", "COMPUTE", "TRUE", "FALSE", "NA"
  )
}


#' Split a keyword run into a variable name
#' @keywords internal
#' @noRd
split_glued_keyword <- function(word, known_lower) {
  parts <- regmatches(
    word,
    regexec("^(if|and|or)([A-Z][A-Za-z0-9_]*)$", word, ignore.case = TRUE)
  )[[1L]]

  if (length(parts) == 0L || tolower(word) %in% known_lower) {
    return(NULL)
  }

  keyword <- parts[[2L]]
  remainder <- parts[[3L]]

  starts_upper <- grepl("^[A-Z]", remainder)
  one_case <- identical(keyword, toupper(keyword)) ||
    identical(keyword, tolower(keyword))

  split <- starts_upper && (
    tolower(remainder) %in% known_lower ||
      (one_case && grepl("[a-z]", remainder))
  )

  if (!split) {
    return(NULL)
  }

  c(keyword = toupper(keyword), name = remainder)
}


#' Raise a derivation parse error
#' @keywords internal
#' @noRd
raise_parse_error <- function(message, position = NA_integer_) {
  condition <- structure(
    class = c("derivation_parse_error", "error", "condition"),
    list(message = message, call = NULL, position = position)
  )

  stop(condition)
}


#' Tokens for an empty derivation
#' @keywords internal
#' @noRd
eof_only_tokens <- function(text_length) {
  tokens <- data.frame(
    kind = "eof",
    text = "",
    position = text_length + 1L,
    newline = TRUE,
    comma = FALSE,
    stringsAsFactors = FALSE
  )

  attr(tokens, "notes") <- character()

  tokens
}
