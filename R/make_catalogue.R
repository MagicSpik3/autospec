#' @title make_catalogue
#'
#' @description
#'
#' Builds a catalogue of every derivation in a set of specification workbooks.
#'
#' Each sheet is read, its header row located, and its side-by-side derivation
#' blocks identified. Within a block, derivations split across several rows are
#' rejoined, section headings are carried down, and each derivation is
#' normalised, parameterised and classified.
#'
#' @details
#'
#' A derivation is written against a variable name once and is then allowed to
#' run over as many rows as it needs, so a row that names no variable continues
#' the derivation above it. This is the `"variable"` rule, and it is the
#' default because it attributes every derivation to a variable.
#'
#' The `"keyword"` rule additionally requires the row to open with `OTHERWISE`,
#' `ELSE` or `ELSE IF`, as [join_continued_items()] does. It is stricter, but
#' it leaves the overflow rows of a multi-row derivation stranded as separate
#' entries with no variable against them: across the six Round 9 workbooks it
#' produces 813 entries of which 211 name no variable, where the `"variable"`
#' rule produces 602 entries that all do.
#'
#' Rows that name a variable but give neither a label nor a derivation are
#' treated as section headings, such as `Transient debt` or `Heavy debt
#' burden`, and are recorded against the derivations that follow them.
#'
#' The catalogue covers every derivation column on a sheet, not only column G.
#' Household-level derivations sit in a second block, and restricting the
#' catalogue to column G would drop them. See [find_derivation_blocks()].
#'
#' @param excel_path_list Paths of the specification workbooks to catalogue.
#' @param exclude_sheets Sheet names to skip. Defaults to
#'   [default_exclude_sheets()].
#' @param join How a continuation row is recognised, either `"variable"` or
#'   `"keyword"`. See Details.
#' @param quiet Whether to suppress progress messages.
#'
#' @returns A data.table with one row per derivation and the columns:
#'
#' * `file_name`, `sheet_name` - where the derivation was found.
#' * `block`, `block_column` - the label of the block, and the Excel reference
#'   of its derivation column.
#' * `level` - `"person"`, `"household"`, `"input"` or `"unknown"`, from the
#'   block label or, where there is none, the block's position on the sheet.
#' * `section` - the section heading in force, where the sheet uses them.
#' * `excel_row` - the worksheet row the derivation starts on.
#' * `variable`, `label` - the variable the derivation defines.
#' * `value_labels`, `notes` - the block's value labels and notes, including
#'   any written on continuation rows.
#' * `instructions` - the derivation as written, with continuation rows joined.
#' * `outputs`, `inputs` - the variables written and read, separated by
#'   `"; "`, read from the parse tree by [analyse_derivation()].
#' * `parse_status` - `"parsed"`, `"unparsed"`, `"value_labels"`,
#'   `"upstream"`, `"midpoint"` or `"r_code"`.
#' * `parse_notes` - parse errors, repairs made to the text, and problems
#'   spotted in it.
#'
#' @importFrom readxl excel_sheets
#' @import data.table
#' @export
#'
#' @examples
#' \dontrun{
#' make_catalogue(list.files("specs", full.names = TRUE))
#' }
make_catalogue <- function(
    excel_path_list,
    exclude_sheets = default_exclude_sheets(),
    join = c("variable", "keyword"),
    quiet = FALSE
) {
  if (!is.character(excel_path_list)) {
    cli::cli_abort("{.arg excel_path_list} must be a character vector.")
  }

  join <- match.arg(join)

  missing_files <- excel_path_list[!file.exists(excel_path_list)]

  if (length(missing_files) > 0L) {
    cli::cli_abort(
      "Cannot find workbook{?s}: {.file {missing_files}}."
    )
  }

  entries <- list()

  for (this_file in excel_path_list) {
    if (tolower(tools::file_ext(this_file)) == "csv") {
      entries[[length(entries) + 1L]] <- catalogue_sheet(
        this_file = this_file,
        this_sheet = "Sheet1",
        join = join
      )
      next
    }

    sheet_names <- readxl::excel_sheets(this_file)
    sheet_names <- setdiff(sheet_names, exclude_sheets)

    if (!quiet) {
      cli::cli_alert_info(
        "{.file {basename(this_file)}}: {length(sheet_names)} sheet{?s}."
      )
    }

    for (this_sheet in sheet_names) {
      entries[[length(entries) + 1L]] <- catalogue_sheet(
        this_file = this_file,
        this_sheet = this_sheet,
        join = join
      )
    }
  }

  data_catalogue <- data.table::rbindlist(entries, use.names = TRUE)

  if (nrow(data_catalogue) == 0L) {
    return(empty_catalogue())
  }

  data_catalogue <- expand_atomic_catalogue_rows(data_catalogue)
  data_catalogue <- classify_catalogue(data_catalogue)

  if (!quiet) {
    cli::cli_alert_success(
      "Catalogued {nrow(data_catalogue)} derivation{?s} from
       {length(unique(data_catalogue$sheet_name))} sheet{?s}."
    )
  }

  data_catalogue[]
}


#' Catalogue the derivations on a single sheet
#'
#' @param this_file Path of the workbook.
#' @param this_sheet Name of the sheet.
#' @param join How continuation rows are recognised.
#'
#' @return A data.table of derivations, before classification.
#'
#' @keywords internal
#' @noRd
catalogue_sheet <- function(this_file, this_sheet, join) {
  spec_sheet <- read_spec_sheet(this_file = this_file, this_sheet = this_sheet)

  header <- attr(spec_sheet, "header", exact = TRUE)
  header_row <- attr(spec_sheet, "header_row", exact = TRUE)
  preamble <- attr(spec_sheet, "preamble", exact = TRUE)

  blocks <- find_derivation_blocks(header)

  if (nrow(blocks) == 0L || nrow(spec_sheet) == 0L) {
    return(empty_catalogue(classified = FALSE))
  }

  # A block label can sit either side of the header, so the sheet is
  # reassembled around its original header position for the search.
  grid <- data.table::rbindlist(
    list(
      preamble,
      spec_sheet[, seq_along(header), with = FALSE]
    ),
    use.names = FALSE
  )

  blocks[, block := detect_block_labels(
    grid = grid,
    header_row = header_row,
    blocks = blocks
  )]

  blocks[, level := block_level(block, seq_len(.N), .N)]

  per_block <- lapply(
    seq_len(nrow(blocks)),
    FUN = function(index) {
      catalogue_block(
        spec_sheet = spec_sheet,
        block_spec = blocks[index],
        this_file = this_file,
        this_sheet = this_sheet,
        join = join
      )
    }
  )

  data.table::rbindlist(per_block, use.names = TRUE)
}


#' Catalogue one derivation block within a sheet
#'
#' @param spec_sheet A sheet as returned by [get_spec()].
#' @param block_spec A single row of [find_derivation_blocks()], naming the
#'   block's column positions and its label.
#' @param this_file Path of the workbook.
#' @param this_sheet Name of the sheet.
#' @param join How continuation rows are recognised.
#'
#' @return A data.table of derivations, before classification.
#'
#' @keywords internal
#' @noRd
catalogue_block <- function(spec_sheet, block_spec, this_file, this_sheet,
                            join) {
  column_count <- ncol(spec_sheet)

  # A block can run past the end of a sheet whose trailing columns are empty,
  # in which case it holds nothing to catalogue.
  if (block_spec$derivation_col > column_count ||
      block_spec$variable_col > column_count) {
    return(empty_catalogue(classified = FALSE))
  }

  optional_column <- function(field) {
    position <- block_field(block_spec, field)

    if (is.na(position) || position > column_count) {
      return(rep(NA_character_, nrow(spec_sheet)))
    }

    spec_sheet[[position]]
  }

  derivation <- spec_sheet[[block_spec$derivation_col]]
  variable <- spec_sheet[[block_spec$variable_col]]
  label <- optional_column("label_col")
  value_labels <- optional_column("value_labels_col")
  notes <- optional_column("notes_col")

  # A section heading names a group of variables rather than a variable of its
  # own: it carries no derivation and no label.
  is_section <- !is_blank(variable) &
    is_blank(derivation) &
    is_blank(label)

  # Carry each section heading down over the derivations that follow it.
  last_section <- cummax(seq_along(is_section) * is_section)

  section <- data.table::fifelse(
    last_section == 0L,
    NA_character_,
    trimws(variable[pmax(last_section, 1L)])
  )

  is_empty <- is_blank(variable) & is_blank(label) & is_blank(derivation) &
    is_blank(value_labels) & is_blank(notes)

  keep <- !is_section & !is_empty

  if (!any(keep & !is_blank(derivation))) {
    return(empty_catalogue(classified = FALSE))
  }

  level <- block_field(block_spec, "level")

  rows <- data.table::data.table(
    file_name = this_file,
    sheet_name = this_sheet,
    block = as.character(block_field(block_spec, "block")),
    block_column = block_spec$block_column,
    level = if (is.na(level)) "unknown" else as.character(level),
    section = section[keep],
    excel_row = spec_sheet$excel_row[keep],
    variable = trimws(variable[keep]),
    label = trimws(label[keep]),
    value_labels = trimws(value_labels[keep]),
    notes = trimws(notes[keep]),
    instructions = data.table::fifelse(is_blank(derivation[keep]), NA_character_,
                                       derivation[keep])
  )

  # A row that names no variable carries on the derivation above it. The
  # specifications name a variable once and let its derivation run over as
  # many rows as it needs, so this is what holds a derivation together.
  unnamed <- is_blank(rows$variable)

  if (join == "keyword") {
    # The stricter rule: a continuation must also open with a continuation
    # keyword. It leaves the other overflow rows unattributed to a variable.
    unnamed <- unnamed &
      grepl(
        "^\\s*(OTHERWISE|ELSE(?:\\s+IF)?)\\b",
        rows$instructions,
        ignore.case = TRUE,
        perl = TRUE
      )
  }

  rows[, continuation := unnamed]

  rows[, statement := continuation_group(
    instructions,
    is_continuation = continuation
  )]

  joined <- rows[
    ,
    .(
      section = section[[1L]],
      excel_row = excel_row[[1L]],
      variable = variable[[1L]],
      label = first_filled(label),
      value_labels = join_filled(value_labels),
      notes = join_filled(notes),
      instructions = join_filled(instructions)
    ),
    by = .(file_name, sheet_name, block, block_column, level, statement)
  ]

  joined[, statement := NULL]

  joined[!is.na(instructions)]
}


#' Read an optional field from a block specification
#' @keywords internal
#' @noRd
block_field <- function(block_spec, field) {
  if (!field %in% names(block_spec)) {
    return(NA)
  }

  block_spec[[field]][[1L]]
}


#' The first non-blank value
#' @keywords internal
#' @noRd
first_filled <- function(values) {
  values <- values[!is_blank(values)]

  if (length(values) == 0L) NA_character_ else values[[1L]]
}


#' Join the non-blank values with line breaks
#' @keywords internal
#' @noRd
join_filled <- function(values) {
  values <- values[!is_blank(values)]

  if (length(values) == 0L) NA_character_ else paste(values, collapse = "\n")
}


#' Add the derived columns to a catalogue
#'
#' @param data_catalogue A catalogue of derivations.
#'
#' @return The catalogue with normalisation, classification and extraction
#'   columns added.
#'
#' @keywords internal
#' @noRd
classify_catalogue <- function(data_catalogue) {
  if (!data.table::is.data.table(data_catalogue)) {
    data_catalogue <- data.table::as.data.table(data_catalogue)
  }

  collapse <- function(values) {
    values <- values[!is.na(values)]

    if (length(values) == 0L) {
      return(NA_character_)
    }

    paste(values, collapse = "; ")
  }

  data_catalogue[, variable := data.table::fifelse(
    is_blank(variable),
    NA_character_,
    variable
  )]

  data_catalogue[, label := data.table::fifelse(
    is_blank(label),
    NA_character_,
    label
  )]

  dv_names <- clean_dv_name(data_catalogue$variable[!is.na(data_catalogue$variable)])
  known_names <- expand_variable_names(dv_names)

  analyses <- lapply(
    seq_len(nrow(data_catalogue)),
    FUN = function(index) {
      analyse_derivation(
        text = data_catalogue$instructions[[index]],
        dv = data_catalogue$variable[[index]],
        level = data_catalogue$level[[index]],
        known_names = known_names
      )
    }
  )

  data_catalogue[, outputs := vapply(
    analyses,
    FUN = function(analysis) collapse(analysis$outputs),
    FUN.VALUE = character(1L)
  )]

  data_catalogue[, inputs := vapply(
    analyses,
    FUN = function(analysis) collapse(analysis$inputs),
    FUN.VALUE = character(1L)
  )]

  data_catalogue[, parse_status := vapply(
    analyses,
    FUN = function(analysis) analysis$kind,
    FUN.VALUE = character(1L)
  )]

  data_catalogue[, parse_notes := vapply(
    analyses,
    FUN = function(analysis) {
      collapse(c(
        if (!is.na(analysis$parse_error)) paste("could not parse:", analysis$parse_error),
        analysis$notes
      ))
    },
    FUN.VALUE = character(1L)
  )]

  data.table::setcolorder(data_catalogue, names(empty_catalogue()))

  data_catalogue
}


#' An empty catalogue with the correct columns and types
#'
#' @param classified Whether to include the columns added once the derivations
#'   have been collected.
#'
#' @return An empty data.table.
#'
#' @keywords internal
#' @noRd
empty_catalogue <- function(classified = TRUE) {
  collected <- data.table::data.table(
    file_name = character(),
    sheet_name = character(),
    block = character(),
    block_column = character(),
    level = character(),
    section = character(),
    excel_row = integer(),
    variable = character(),
    label = character(),
    value_labels = character(),
    notes = character(),
    instructions = character()
  )

  if (!classified) {
    return(collected)
  }

  cbind(
    data.table::data.table(uid = character()),
    collected,
    data.table::data.table(
      outputs = character(),
      inputs = character(),
      parse_status = character(),
      parse_notes = character()
    )
  )
}
