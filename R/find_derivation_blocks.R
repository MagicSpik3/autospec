#' @title find_derivation_blocks
#'
#' @description
#'
#' Locates the derivation blocks in a specification sheet header.
#'
#' A specification sheet is laid out as several blocks side by side, one per
#' level of the data — typically input variables, person level and household
#' level. Each block repeats the same header labels, so a block is identified
#' by a `Derivation` column together with the nearest `Variable Name` and
#' `Label` columns to its left.
#'
#' Column G is the person-level derivation column on most sheets, but it is
#' not the only one: household-level derivations sit in a second block further
#' to the right, and `R9_Work_and_Pay` carries four blocks. Cataloguing column
#' G alone silently drops those derivations.
#'
#' @param header A character vector of header labels, in column order. This is
#'   the `header` attribute attached by [get_spec()].
#'
#' @return A data.table with one row per block and the columns:
#'
#' * `block_column` — Excel reference of the derivation column.
#' * `derivation_col`, `variable_col`, `label_col` — column positions. The
#'   label position is `NA` when the block has no `Label` column.
#' * `value_labels_col`, `notes_col` — positions of the block's `Value Labels`
#'   and `Notes` columns, found to the right of the derivation and before the
#'   next block begins. `NA` when the block has no such column.
#'
#' Blocks without a `Variable Name` column to the left of the derivation are
#' dropped, since their entries cannot be attributed to a variable.
#'
#' @import data.table
#'
#' @keywords internal
#' @noRd
#'
#' @examples
#' find_derivation_blocks(
#'   c("Variable Name", "Label", "Derivation", "Notes")
#' )
find_derivation_blocks <- function(header) {
  if (!is.character(header)) {
    cli::cli_abort("{.arg header} must be a character vector.")
  }

  labels <- normalise_header(header)

  derivation_cols <- which(labels == "derivation")

  if (length(derivation_cols) == 0L) {
    return(
      data.table::data.table(
        block_column = character(),
        derivation_col = integer(),
        variable_col = integer(),
        label_col = integer(),
        value_labels_col = integer(),
        notes_col = integer()
      )
    )
  }

  is_variable <- grepl("^variable( name)?$", labels)
  is_label <- labels == "label"
  is_value_labels <- grepl("^value labels?$", labels)
  is_notes <- grepl("^notes?$", labels)

  # The trailing columns of a block sit to the right of its derivation column
  # and stop where the next block's variable column begins.
  nearest_right <- function(candidates, derivation_col) {
    later_variables <- which(is_variable & seq_along(labels) > derivation_col)

    block_end <- if (length(later_variables) == 0L) {
      length(labels) + 1L
    } else {
      min(later_variables)
    }

    matches <- which(candidates)
    matches <- matches[matches > derivation_col & matches < block_end]

    if (length(matches) == 0L) {
      return(NA_integer_)
    }

    min(matches)
  }

  # The columns of a block sit to the left of its derivation column, and stop
  # at the previous block's derivation column.
  nearest_left <- function(candidates, derivation_col, boundary) {
    matches <- which(candidates)
    matches <- matches[matches < derivation_col & matches > boundary]

    if (length(matches) == 0L) {
      return(NA_integer_)
    }

    max(matches)
  }

  boundaries <- c(0L, utils::head(derivation_cols, -1L))

  blocks <- data.table::data.table(
    block_column = excel_letters(max(derivation_cols))[derivation_cols],
    derivation_col = as.integer(derivation_cols),
    variable_col = as.integer(mapply(
      nearest_left,
      derivation_col = derivation_cols,
      boundary = boundaries,
      MoreArgs = list(candidates = is_variable)
    )),
    label_col = as.integer(mapply(
      nearest_left,
      derivation_col = derivation_cols,
      boundary = boundaries,
      MoreArgs = list(candidates = is_label)
    )),
    value_labels_col = as.integer(vapply(
      derivation_cols,
      FUN = function(col) nearest_right(is_value_labels, col),
      FUN.VALUE = integer(1L)
    )),
    notes_col = as.integer(vapply(
      derivation_cols,
      FUN = function(col) nearest_right(is_notes, col),
      FUN.VALUE = integer(1L)
    ))
  )

  blocks[!is.na(variable_col)]
}


#' Decide which level of the data a block describes
#' @keywords internal
#' @noRd
block_level <- function(block_label, block_index, block_count) {
  label_count <- length(block_label)
  block_index <- rep_len(block_index, label_count)
  block_count <- rep_len(block_count, label_count)

  text <- tolower(ifelse(is.na(block_label), "", block_label))

  by_position <- ifelse(
    block_count > 1L & block_index == block_count,
    "household",
    ifelse(block_count > 1L, "person", "unknown")
  )

  by_label <- ifelse(
    grepl("household", text, fixed = TRUE), "household",
    ifelse(
      grepl("person", text, fixed = TRUE), "person",
      ifelse(grepl("input", text, fixed = TRUE), "input", NA_character_)
    )
  )

  ifelse(is.na(by_label), by_position, by_label)
}


#' Detect the block label for each derivation block
#'
#' The level of each block — `Person`, `Household`, `Input variables` — is
#' written on a row next to the header, aligned with the block's variable
#' column. That row sits above the header on some sheets and below it on
#' others, so rows on both sides are searched and the closest qualifying row
#' is used.
#'
#' A qualifying row carries a value in at least one variable column and no
#' value in any derivation column, which distinguishes it from a data row.
#'
#' @param grid A data.table of the whole sheet, including the header row.
#' @param header_row Position of the header row within `grid`.
#' @param blocks Block positions, as returned by [find_derivation_blocks()].
#' @param search Number of rows to search either side of the header.
#'
#' @return A character vector of block labels, one per block, `NA` where no
#'   qualifying row was found.
#'
#' @keywords internal
#' @noRd
detect_block_labels <- function(grid, header_row, blocks, search = 2L) {
  if (nrow(blocks) == 0L) {
    return(character())
  }

  candidate_rows <- setdiff(
    seq.int(
      from = max(1L, header_row - search),
      to = min(nrow(grid), header_row + search)
    ),
    header_row
  )

  # Prefer rows closest to the header, and a row below over a row above when
  # both are the same distance away.
  candidate_rows <- candidate_rows[
    order(abs(candidate_rows - header_row), -candidate_rows)
  ]

  for (this_row in candidate_rows) {
    values <- unlist(grid[this_row], use.names = FALSE)

    variable_values <- values[blocks$variable_col]
    derivation_values <- values[blocks$derivation_col]

    if (any(!is_blank(variable_values)) && all(is_blank(derivation_values))) {
      return(
        data.table::fifelse(
          is_blank(variable_values),
          NA_character_,
          trimws(variable_values)
        )
      )
    }
  }

  rep(NA_character_, nrow(blocks))
}
