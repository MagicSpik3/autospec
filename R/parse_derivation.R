#' @title parse_derivation
#'
#' @description
#'
#' Parses a derivation into a tree of statements, so that its shape can be
#' recognised and its inputs read off exactly, instead of guessed at with
#' regular expressions.
#'
#' The grammar accepts the three dialects that appear in the specifications:
#'
#' * Pseudocode: `IF x > 0 THEN y = 1 ELSE y = 0`, `OTHERWISE`, `ELSE IF`.
#' * SPSS: `DO IF (x = 1). COMPUTE y = 2. ELSE IF ... END IF.`
#' * Assignments and arithmetic: `y = a + b`, `SUM(a, b)`, `x ** 2`.
#'
#' It also repairs the common slips found in the Round 9 workbooks, and records
#' each repair in `notes` so a reviewer can see what was assumed: an implied
#' `THEN`, `x = 1,3,5` meaning `x IN (1, 3, 5)`, `(otherwise 0)` in brackets,
#' an unmatched closing bracket, a keyword run into a variable name, an
#' assignment with nothing on its left, and so on.
#'
#' A derivation that still cannot be parsed after those repairs is reported
#' with `ok = FALSE` and an error message pointing at the offending token. That
#' is not a failure of the parser so much as a finding about the spec.
#'
#' @param text A single derivation, as written in the specification.
#' @param known_names Variable names known to exist, passed to
#'   [tokenise_derivation()].
#'
#' @return A list with elements:
#'
#' * `ok` — whether the derivation parsed.
#' * `statements` — a list of statement nodes (see Details).
#' * `error` — the parse error message, or `NA`.
#' * `notes` — repairs and assumptions made while parsing.
#' * `text` — the derivation after clean-up, as it was parsed.
#'
#' @details
#'
#' Every node is a list with a `type` element:
#'
#' * `assign` — `target` (the name assigned to, or `NA` when the spec leaves
#'   it implied) and `value`.
#' * `expr` — a bare expression, `value`.
#' * `if` — `branches`, a list of `list(condition, body)`, and `otherwise`,
#'   the else body or `NULL`.
#' * `bin` — `op` (`+ - * / ^ == != < > <= >= & |`), `left`, `right`.
#' * `in` — `x`, `set` (a list of nodes) and `negate`.
#' * `call` — `fn` (upper case), `args`, `named`.
#' * `name` — `name`; `num` — `value`; `str` — `value`; `lit` — `value`
#'   (`"TRUE"`, `"FALSE"`, `"NA"`); `neg` and `not` — `x`; `set` — `items`.
#'
#' @keywords internal
#' @noRd
#'
#' @examples
#' parsed <- parse_derivation("IF CommiR9 in (4,5) THEN Flag = 1 ELSE Flag = 0")
#' parsed$ok
#' parsed$statements[[1]]$type
parse_derivation <- function(text, known_names = character()) {
  if (!is.character(text) || length(text) != 1L || is.na(text)) {
    cli::cli_alert_danger("{.arg text} must be a single non-missing string.")
    stop("`text` must be a single non-missing string.", call. = FALSE)
  }

  prepared <- prepare_derivation_text(text)

  result <- function(ok, statements, error, notes) {
    list(
      ok = ok,
      statements = statements,
      error = error,
      notes = unique(notes),
      text = prepared$text
    )
  }

  tokens <- tryCatch(
    tokenise_derivation(prepared$text, known_names),
    derivation_parse_error = function(condition) condition
  )

  if (inherits(tokens, "derivation_parse_error")) {
    return(result(FALSE, list(), conditionMessage(tokens), prepared$notes))
  }

  kinds <- tokens$kind
  texts <- tokens$text
  newlines <- tokens$newline
  commas <- tokens$comma
  positions <- tokens$position
  token_count <- length(kinds)
  known_lower <- tolower(known_names)

  state <- new.env(parent = emptyenv())
  state$i <- 1L
  state$notes <- c(prepared$notes, attr(tokens, "notes"))

  add_note <- function(note) {
    state$notes <- c(state$notes, note)
  }

  # --- token access ---------------------------------------------------------

  peek_index <- function(offset = 0L) min(state$i + offset, token_count)
  peek_kind <- function(offset = 0L) kinds[[peek_index(offset)]]
  peek_text <- function(offset = 0L) texts[[peek_index(offset)]]

  advance <- function() {
    index <- peek_index()
    state$i <- state$i + 1L
    index
  }

  at_kw <- function(...) peek_kind() == "kw" && peek_text() %in% c(...)
  at_op <- function(...) peek_kind() == "op" && peek_text() %in% c(...)

  fail <- function(message) {
    index <- peek_index()
    where <- if (kinds[[index]] == "eof") {
      "end of text"
    } else {
      paste0("'", texts[[index]], "'")
    }

    raise_parse_error(paste(message, "at", where), positions[[index]])
  }

  skip_separators <- function() {
    while (at_op(";", ".", ",", ":")) {
      advance()
    }
  }

  at_else <- function() at_kw("ELSE", "ELSEIF", "ELIF", "OTHERWISE")
  at_block_end <- function() peek_kind() == "eof" || at_else() || at_kw("END")

  comparison_ops <- c(
    "=" = "==", "==" = "==", "!=" = "!=", "<>" = "!=", "~=" = "!=",
    "<" = "<", ">" = ">", "<=" = "<=", ">=" = ">=", "=<" = "<=", "=>" = ">="
  )

  flipped_ops <- c(
    "<" = ">=", ">" = "<=", "<=" = ">", ">=" = "<", "==" = "!=", "!=" = "=="
  )

  # --- expressions ----------------------------------------------------------

  read_expression <- function() {
    node <- and_expression()

    while (at_kw("OR") || at_op("|", "||")) {
      advance()
      right <- and_expression()

      # "x = 2 or 3" means x in (2, 3)
      extends_membership <- node_is(right, "num") && (
        node_is(node, "in") ||
          (node_is(node, "bin") && node$op == "==" && node_is(node$right, "num"))
      )

      if (extends_membership) {
        add_note("'x = a or b' read as x in (a, b)")

        if (node_is(node, "in")) {
          node <- list(type = "in", x = node$x,
                       set = c(node$set, list(right)), negate = FALSE)
        } else {
          node <- list(type = "in", x = node$left,
                       set = list(node$right, right), negate = FALSE)
        }

        next
      }

      node <- list(type = "bin", op = "|", left = node, right = right)
    }

    node
  }

  and_expression <- function() {
    node <- not_expression()

    while (at_kw("AND") || at_op("&", "&&")) {
      advance()
      node <- list(type = "bin", op = "&", left = node, right = not_expression())
    }

    node
  }

  not_expression <- function() {
    if (at_kw("NOT") || at_op("!")) {
      advance()
      return(list(type = "not", x = not_expression()))
    }

    comparison()
  }

  comparison <- function() {
    left <- additive()
    negate <- FALSE

    if (at_kw("NOT") && peek_kind(1L) == "kw" && peek_text(1L) == "IN") {
      advance()
      negate <- TRUE
    }

    if (at_kw("IN")) {
      advance()

      if (!at_op("(")) {
        fail("expected '(' after IN")
      }

      advance()
      values <- list(read_expression())

      while (at_op(",")) {
        advance()
        values <- c(values, list(read_expression()))
      }

      if (!at_op(")")) {
        fail("expected ')' to close IN list")
      }

      advance()

      return(list(type = "in", x = left, set = values, negate = negate))
    }

    flip <- FALSE

    # "PersonR9 NOT < PartNoR9"
    if (at_kw("NOT") && peek_kind(1L) == "op" &&
        peek_text(1L) %in% names(comparison_ops)) {
      advance()
      flip <- TRUE
    }

    if (peek_kind() == "op" && peek_text() %in% names(comparison_ops)) {
      op <- comparison_ops[[texts[[advance()]]]]

      if (flip) {
        op <- flipped_ops[[op]]
      }

      right <- additive()

      if (node_is(right, "set") && op %in% c("==", "!=")) {
        add_note("'x = (a, b)' read as x in (a, b)")
        return(list(type = "in", x = left, set = right$items,
                    negate = op == "!="))
      }

      # "x = 1,3,5,6" is written as set membership
      if (op %in% c("==", "!=") && node_is(right, "num") && at_op(",") &&
          peek_kind(1L) == "number") {
        members <- list(right)

        while (at_op(",") && peek_kind(1L) == "number") {
          advance()
          members <- c(members, list(primary()))
        }

        add_note("'x = a,b,c' read as x in (a, b, c)")
        return(list(type = "in", x = left, set = members, negate = op == "!="))
      }

      # "x = (2 OR 5 OR 8)" is written as set membership
      members <- or_of_literals(right)

      if (!is.null(members) && op %in% c("==", "!=")) {
        add_note("comparison against an OR of values, read as IN")
        return(list(type = "in", x = left, set = members, negate = op == "!="))
      }

      return(list(type = "bin", op = op, left = left, right = right))
    }

    left
  }

  additive <- function() {
    node <- multiplicative()

    while (at_op("+", "-")) {
      op <- texts[[advance()]]
      node <- list(type = "bin", op = op, left = node, right = multiplicative())
    }

    node
  }

  multiplicative <- function() {
    node <- unary()

    while (at_op("*", "/")) {
      op <- texts[[advance()]]
      node <- list(type = "bin", op = op, left = node, right = unary())
    }

    node
  }

  unary <- function() {
    if (at_op("-", "+")) {
      op <- texts[[advance()]]
      operand <- unary()

      if (op == "+") {
        return(operand)
      }

      if (node_is(operand, "num")) {
        return(list(type = "num", value = -operand$value))
      }

      return(list(type = "neg", x = operand))
    }

    power()
  }

  power <- function() {
    base <- primary()

    if (at_op("**", "^")) {
      advance()
      return(list(type = "bin", op = "^", left = base, right = unary()))
    }

    base
  }

  primary <- function() {
    kind <- peek_kind()
    value <- peek_text()

    if (kind == "number") {
      index <- advance()

      if (commas[[index]]) {
        add_note("number written with a thousands comma")
      }

      return(list(type = "num", value = as.numeric(gsub(",", "", value, fixed = TRUE))))
    }

    if (kind == "string") {
      advance()
      return(list(type = "str", value = substr(value, 2L, nchar(value) - 1L)))
    }

    if (kind == "kw" && value %in% c("TRUE", "FALSE", "NA")) {
      advance()
      return(list(type = "lit", value = value))
    }

    if (kind == "name") {
      advance()

      if (peek_kind() == "op" && peek_text() == "(" && !newlines[[peek_index()]]) {
        advance()
        args <- list()
        named <- list()

        if (!at_op(")")) {
          repeat {
            is_named <- peek_kind() == "name" &&
              grepl(".", peek_text(), fixed = TRUE) &&
              peek_kind(1L) == "op" && peek_text(1L) == "="

            if (is_named) {
              key <- texts[[advance()]]
              advance()
              named[[key]] <- read_expression()
            } else {
              args <- c(args, list(read_expression()))
            }

            if (at_op(",")) {
              advance()
              next
            }

            break
          }
        }

        if (!at_op(")")) {
          fail(paste0("expected ')' to close ", value, "("))
        }

        advance()

        return(list(type = "call", fn = toupper(value), args = args, named = named))
      }

      # "IF any DLBEH(1-3)R9_i = (2)" uses ANY as a prefix
      if (toupper(value) == "ANY" && peek_kind() %in% c("name", "number")) {
        return(list(type = "call", fn = "ANY", args = list(additive()),
                    named = list()))
      }

      return(list(type = "name", name = value))
    }

    if (at_op("(")) {
      advance()
      node <- read_expression()

      # "(1,2,3,4,5)" used as a set of values
      if (at_op(",") && node_is(node, "num")) {
        items <- list(node)

        while (at_op(",")) {
          advance()
          items <- c(items, list(read_expression()))
        }

        if (!at_op(")")) {
          fail("expected ')'")
        }

        advance()

        return(list(type = "set", items = items))
      }

      if (at_op(",")) {
        fail("comma inside brackets with no function name")
      }

      if (!at_op(")")) {
        fail("expected ')'")
      }

      advance()

      return(node)
    }

    fail(if (kind == "eof") "unexpected end of text" else "unexpected token")
  }

  # --- statements -----------------------------------------------------------

  statement <- function() {
    if (at_kw("DO") && peek_kind(1L) == "kw" && peek_text(1L) == "IF") {
      advance()
      advance()
      return(if_statement(block = TRUE))
    }

    if (at_kw("IF")) {
      advance()
      return(if_statement(block = FALSE))
    }

    if (at_kw("COMPUTE")) {
      advance()
    }

    if (at_else()) {
      fail("ELSE without a matching IF")
    }

    if (at_kw("END")) {
      fail("END without a matching DO IF")
    }

    # "then = OinRrAmr9 * 52": the DV itself is the implied target
    if (at_op("=")) {
      advance()
      add_note("assignment with no variable on its left")
      return(list(type = "assign", target = NA_character_, value = read_expression()))
    }

    if (peek_kind() == "name" && peek_kind(1L) == "op" &&
        peek_text(1L) %in% c("=", ":=")) {
      target <- texts[[advance()]]
      advance()

      if (at_op(";", ".") || peek_kind() == "eof") {
        fail(paste0("assignment to ", target, " has nothing on its right"))
      }

      return(list(type = "assign", target = target, value = read_expression()))
    }

    value <- read_expression()

    # "for each iteration": a lower-case word followed by another word is prose
    reads_like_prose <- node_is(value, "name") &&
      identical(value$name, tolower(value$name)) &&
      grepl("[a-z]", value$name) &&
      !(value$name %in% known_lower) &&
      peek_kind() == "name"

    if (reads_like_prose) {
      fail("reads like prose")
    }

    list(type = "expr", value = value)
  }

  # Accepts "THEN", ", THEN", ". THEN", "THEN;", "THEN:", or a bare "," or ":".
  # Returns TRUE when no marker was found, meaning THEN was implied.
  then_marker <- function() {
    marked <- FALSE

    repeat {
      if (at_kw("THEN")) {
        advance()
        marked <- TRUE
      } else if (at_op(",", ":")) {
        advance()
        marked <- TRUE
      } else if (at_op(";", ".") && peek_kind(1L) == "kw" && peek_text(1L) == "THEN") {
        advance()
      } else {
        break
      }
    }

    !marked
  }

  if_statement <- function(block) {
    branches <- list()
    otherwise <- NULL

    condition <- read_expression()

    if (then_marker()) {
      add_note("IF without THEN")
    }

    if (at_kw("DO")) {
      advance()
      block <- TRUE
    }

    branches[[1L]] <- list(condition = condition, body = read_body(block))

    repeat {
      skip_separators()

      is_else_if <- at_kw("ELSEIF", "ELIF") ||
        (at_kw("ELSE") && peek_kind(1L) == "kw" && peek_text(1L) == "IF")

      if (is_else_if) {
        advance()

        if (at_kw("IF")) {
          advance()
        }

        condition <- read_expression()
        then_marker()

        if (at_kw("DO")) {
          advance()
        }

        branches[[length(branches) + 1L]] <- list(
          condition = condition,
          body = read_body(block)
        )

        next
      }

      if (at_kw("ELSE", "OTHERWISE")) {
        advance()

        if (at_kw("DO")) {
          advance()
        }

        otherwise <- read_body(block)
      }

      break
    }

    skip_separators()

    if (at_kw("END")) {
      advance()

      if (at_kw("IF")) {
        advance()
      }
    } else if (block) {
      add_note("DO IF without END IF")
    }

    list(type = "if", branches = branches, otherwise = otherwise)
  }

  read_body <- function(block) {
    statements <- list()
    skip_separators()

    while (!at_block_end()) {
      # Without DO IF / END IF delimiters, a new IF after the first statement
      # starts a new statement rather than nesting inside this branch.
      if (!block && length(statements) > 0L && (at_kw("IF") || at_kw("DO"))) {
        break
      }

      item <- statement()

      # "(otherwise 0)": a bare number inside a branch sets the DV
      if (item$type == "expr" && node_is(item$value, "num")) {
        add_note("value with no variable name inside a branch")
        item <- list(type = "assign", target = NA_character_, value = item$value)
      }

      statements[[length(statements) + 1L]] <- item
      skip_separators()
    }

    statements
  }

  parsed <- tryCatch(
    {
      statements <- list()
      skip_separators()

      while (peek_kind() != "eof") {
        statements[[length(statements) + 1L]] <- statement()
        skip_separators()
      }

      statements
    },
    derivation_parse_error = function(condition) condition,
    error = function(condition) {
      structure(
        class = c("derivation_parse_error", "error", "condition"),
        list(message = paste("parser failure:", conditionMessage(condition)),
             call = NULL, position = NA_integer_)
      )
    }
  )

  if (inherits(parsed, "derivation_parse_error")) {
    return(result(FALSE, list(), conditionMessage(parsed), state$notes))
  }

  result(TRUE, parsed, NA_character_, state$notes)
}


#' Clean up a derivation before parsing
#' @keywords internal
#' @noRd
prepare_derivation_text <- function(text) {
  notes <- character()

  # Non-breaking spaces and tabs pasted in from Word or Outlook.
  text <- gsub(paste0("[\t", intToUtf8(0xA0), "]"), " ", text)
  cleaned <- text

  # En dash, em dash, and the replacement character Excel leaves behind.
  for (dash in c(intToUtf8(0x2013), intToUtf8(0x2014), intToUtf8(0xFFFD))) {
    cleaned <- gsub(dash, "-", cleaned, fixed = TRUE)
  }

  if (!identical(cleaned, text)) {
    notes <- c(notes, "dash character used as minus")
  }

  squared <- gsub("\\[(\\d+\\s*-\\s*\\d+)\\]", "(\\1)", cleaned, perl = TRUE)

  if (!identical(squared, cleaned)) {
    notes <- c(notes, "numbered range written in square brackets")
    cleaned <- squared
  }

  trailing_reference <- "\\n\\s*derived in\\b[^\\n]*\\bspec\\s*$"

  if (grepl(trailing_reference, cleaned, ignore.case = TRUE, perl = TRUE)) {
    cleaned <- sub(trailing_reference, "", cleaned, ignore.case = TRUE, perl = TRUE)
    notes <- c(notes, "also says it is derived in another spec")
  }

  cleaned <- sub(
    "^\\s*aggregated?\\s+from\\s+person\\s+level\\s*\\(\\s*([A-Za-z_][A-Za-z0-9_]*)\\s*\\)",
    "SUM(\\1)",
    cleaned,
    ignore.case = TRUE,
    perl = TRUE
  )

  cleaned <- sub(
    paste0(
      "\\s*[-(]?\\s*(aggregated?\\s+from\\s+person\\s+level(\\s+file)?",
      "|at\\s+person\\s+level)\\s*\\)?\\s*[;.]?\\s*$"
    ),
    "",
    cleaned,
    ignore.case = TRUE,
    perl = TRUE
  )

  otherwise_in_brackets <- "\\(\\s*(otherwise\\b[^()]*)\\)"

  if (grepl(otherwise_in_brackets, cleaned, ignore.case = TRUE, perl = TRUE)) {
    cleaned <- gsub(otherwise_in_brackets, " \\1", cleaned,
                    ignore.case = TRUE, perl = TRUE)
    notes <- c(notes, "OTHERWISE written in brackets")
  }

  if_in_brackets <- "^\\s*\\(\\s*(if\\b[^()]*(?:\\([^()]*\\)[^()]*)*)\\)\\s*(then\\b)"

  if (grepl(if_in_brackets, cleaned, ignore.case = TRUE, perl = TRUE)) {
    cleaned <- sub(if_in_brackets, "\\1 \\2", cleaned, ignore.case = TRUE, perl = TRUE)
    notes <- c(notes, "IF condition wrapped in brackets")
  }

  opens_before_if <- grepl("^\\s*\\(\\s*if\\b", cleaned, ignore.case = TRUE, perl = TRUE)

  if (opens_before_if &&
      count_character(cleaned, "(") == count_character(cleaned, ")") + 1L) {
    cleaned <- sub("^\\s*\\(", "", cleaned, perl = TRUE)
    notes <- c(notes, "unclosed bracket before IF removed")
  }

  balanced <- drop_unmatched_closers(cleaned)

  if (balanced$dropped) {
    notes <- c(notes, "unmatched closing bracket removed")
  }

  list(text = balanced$text, notes = notes)
}


#' Count occurrences of a single character
#' @keywords internal
#' @noRd
count_character <- function(text, character) {
  sum(strsplit(text, "", fixed = TRUE)[[1L]] == character)
}


#' Remove closing brackets that have no opening partner
#' @keywords internal
#' @noRd
drop_unmatched_closers <- function(text) {
  characters <- strsplit(text, "", fixed = TRUE)[[1L]]
  keep <- rep(TRUE, length(characters))
  depth <- 0L

  for (index in seq_along(characters)) {
    if (characters[[index]] == "(") {
      depth <- depth + 1L
    } else if (characters[[index]] == ")") {
      if (depth == 0L) {
        keep[[index]] <- FALSE
      } else {
        depth <- depth - 1L
      }
    }
  }

  list(
    text = paste(characters[keep], collapse = ""),
    dropped = !all(keep)
  )
}


#' Collect the values of an OR of numbers
#' @keywords internal
#' @noRd
or_of_literals <- function(node) {
  if (!node_is(node, "bin") || node$op != "|") {
    return(NULL)
  }

  collect <- function(item) {
    if (node_is(item, "num")) {
      return(list(item))
    }

    if (node_is(item, "bin") && item$op == "|") {
      left <- collect(item$left)
      right <- collect(item$right)

      if (!is.null(left) && !is.null(right)) {
        return(c(left, right))
      }
    }

    NULL
  }

  collect(node)
}


#' Test the type of a parse tree node
#' @keywords internal
#' @noRd
node_is <- function(node, type) {
  is.list(node) && identical(node$type, type)
}
