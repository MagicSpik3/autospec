#' Names read anywhere in a parse tree
#' @keywords internal
#' @noRd
tree_names <- function(node) {
  if (is.null(node) || length(node) == 0L) {
    return(character())
  }

  if (is.null(node$type)) {
    return(unlist(lapply(node, tree_names), use.names = FALSE))
  }

  switch(
    node$type,
    name = node$name,
    call = tree_names(node$args),
    bin = c(tree_names(node$left), tree_names(node$right)),
    not = tree_names(node$x),
    neg = tree_names(node$x),
    "in" = c(tree_names(node$x), tree_names(node$set)),
    set = tree_names(node$items),
    assign = tree_names(node$value),
    expr = tree_names(node$value),
    "if" = c(
      unlist(
        lapply(node$branches, function(branch) {
          c(tree_names(branch$condition), tree_names(branch$body))
        }),
        use.names = FALSE
      ),
      tree_names(node$otherwise)
    ),
    character()
  )
}


#' Names assigned to anywhere in a parse tree
#' @keywords internal
#' @noRd
tree_targets <- function(node) {
  if (is.null(node) || length(node) == 0L) {
    return(character())
  }

  if (is.null(node$type)) {
    return(unlist(lapply(node, tree_targets), use.names = FALSE))
  }

  switch(
    node$type,
    assign = node$target,
    "if" = c(
      unlist(
        lapply(node$branches, function(branch) tree_targets(branch$body)),
        use.names = FALSE
      ),
      tree_targets(node$otherwise)
    ),
    character()
  )
}


#' Flatten an addition into the names it adds
#' @keywords internal
#' @noRd
flatten_addition <- function(node) {
  if (node_is(node, "name")) {
    return(node$name)
  }

  if (node_is(node, "bin") && node$op == "+") {
    left <- flatten_addition(node$left)
    right <- flatten_addition(node$right)

    if (is.null(left) || is.null(right)) {
      return(NULL)
    }

    return(c(left, right))
  }

  if (node_is(node, "call") && node$fn == "SUM" && length(node$args) > 0L) {
    parts <- lapply(node$args, flatten_addition)

    if (any(vapply(parts, is.null, logical(1L)))) {
      return(NULL)
    }

    return(unlist(parts, use.names = FALSE))
  }

  NULL
}


#' Test whether a node is a particular number
#' @keywords internal
#' @noRd
is_number_node <- function(node, value = NULL) {
  node_is(node, "num") && (is.null(value) || isTRUE(node$value == value))
}


#' The single assignment in a branch body
#' @keywords internal
#' @noRd
single_assignment <- function(body) {
  if (length(body) == 1L && node_is(body[[1L]], "assign")) {
    return(body[[1L]])
  }

  NULL
}


#' Format a number for a drafted condition or argument
#' @keywords internal
#' @noRd
format_number <- function(value) {
  vapply(
    value,
    FUN = function(number) {
      if (is.na(number)) {
        return("NA")
      }

      format(number, scientific = FALSE, digits = 15L, trim = TRUE)
    },
    FUN.VALUE = character(1L),
    USE.NAMES = FALSE
  )
}


#' Translate a condition into R code
#' @keywords internal
#' @noRd
condition_to_r <- function(node, rename = identity) {
  to_r <- function(item) condition_to_r(item, rename)

  switch(
    node$type,
    num = format_number(node$value),
    name = {
      name <- rename(node$name)

      if (grepl("^[A-Za-z.][A-Za-z0-9._]*$", name)) name else paste0("`", name, "`")
    },
    lit = node$value,
    str = encodeString(node$value, quote = "\""),
    neg = paste0("-", to_r(node$x)),
    not = paste0("!(", to_r(node$x), ")"),
    "in" = {
      text <- paste0(
        to_r(node$x), " %in% c(",
        paste(vapply(node$set, to_r, character(1L)), collapse = ", "),
        ")"
      )

      if (isTRUE(node$negate)) paste0("!(", text, ")") else text
    },
    bin = {
      left <- to_r(node$left)
      right <- to_r(node$right)

      if (node$op %in% c("&", "|")) {
        paste0("(", left, " ", node$op, " ", right, ")")
      } else {
        paste(left, node$op, right)
      }
    },
    call = {
      if (node$fn != "ANY" || length(node$args) < 2L) {
        signal_untranslatable(node$fn)
      }

      first <- node$args[[1L]]
      rest <- node$args[-1L]

      if (!node_is(first, "num")) {
        paste0(
          to_r(first), " %in% c(",
          paste(vapply(rest, to_r, character(1L)), collapse = ", "),
          ")"
        )
      } else {
        paste0(
          "(",
          paste(
            vapply(rest, function(item) paste(to_r(item), "==", to_r(first)),
                   character(1L)),
            collapse = " | "
          ),
          ")"
        )
      }
    },
    signal_untranslatable(node$type)
  )
}


#' Signal that a condition cannot be written in R
#' @keywords internal
#' @noRd
signal_untranslatable <- function(what) {
  stop(structure(
    class = c("untranslatable", "error", "condition"),
    list(message = what, call = NULL)
  ))
}


#' The multiplier a period branch applies to an amount
#' @keywords internal
#' @noRd
evaluate_multiplier <- function(node, amount) {
  if (node_is(node, "num")) {
    return(node$value)
  }

  if (node_is(node, "name")) {
    return(if (tolower(node$name) == tolower(amount)) 1 else NULL)
  }

  if (node_is(node, "bin") && node$op %in% c("*", "/")) {
    left <- evaluate_multiplier(node$left, amount)
    right <- evaluate_multiplier(node$right, amount)

    if (is.null(left) || is.null(right)) {
      return(NULL)
    }

    if (node$op == "*") {
      return(left * right)
    }

    return(if (right == 0) NULL else left / right)
  }

  NULL
}
