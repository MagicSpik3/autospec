#' @title get_spec
#'
#' @description
#'
#' Reads one specification sheet and returns it as a data.table positioned by
#' Excel column reference.
#'
#' The specification workbooks do not place the header on the first row. Some
#' sheets open with a title banner and the header appears on row two, three or
#' four. The sheet is therefore read without column names, the header row is
#' located by searching for the `Derivation` label, and every row above and
#' including the header is dropped. Reading the sheet with `col_names = TRUE`
#' would consume a banner row as the column names and leave the real header
#' behind as a data row, which is how the literal text `"Derivation"` leaks
#' into a catalogue.
#'
#' Columns are named `col_A`, `col_B` and so on, matching the Excel column
#' references, because the header labels repeat across the sheet: a sheet
#' carries several side-by-side blocks, each with its own `Variable Name`,
#' `Label` and `Derivation` columns.
#'
#' @param this_file Filename and path of the specification workbook.
#' @param this_sheet Name of the sheet to retrieve.
#' @param header_search_rows Number of leading rows to search for the header.
#'
#' @returns A data.table of the sheet contents below the header row, with:
#'
#' * `excel_row` — the row number in the original worksheet.
#' * `col_A` ... — one column per worksheet column, all character.
#' * `column_g_original` — an unmodified copy of column G, retained for audit
#'   and comparison.
#'
#' The header row number and its labels are attached as the `header_row` and
#' `header` attributes, and the rows up to and including the header as the
#' `preamble` attribute. The preamble is needed because some sheets write the
#' level of each block on the row above the header.
#'
#' @importFrom readxl read_excel
#' @import data.table
#'
#' @keywords internal
#' @noRd
#'
#' @examples
#' \dontrun{
#' get_spec("specs/Physical_Wealth_DV_Spec_R9.xlsx", "Physical_R9")
#' }
get_spec <- function(this_file, this_sheet, header_search_rows = 10L) {

  read_spec_sheet(this_file, this_sheet, header_search_rows)
}


#' Read one specification sheet, whether from Excel or a CSV export.
#' @keywords internal
#' @noRd
read_spec_sheet <- function(this_file, this_sheet, header_search_rows = 10L) {
  if (tolower(tools::file_ext(this_file)) == "csv") {
    return(read_csv_spec(this_file, this_sheet, header_search_rows))
  }

  get_spec_excel(this_file, this_sheet, header_search_rows)
}


#' Read an Excel spec sheet via readxl.
#' @keywords internal
#' @noRd
get_spec_excel <- function(this_file, this_sheet, header_search_rows = 10L) {

  # Read without column names so that the header row is preserved as data and
  # can be located. readxl supplies placeholder names, which are replaced by
  # the Excel column references below.
  raw_sheet <- readxl::read_excel(
    path = this_file,
    sheet = this_sheet,
    col_types = "text",
    col_names = FALSE
  )

  data.table::setDT(raw_sheet)

  if (ncol(raw_sheet) == 0L || nrow(raw_sheet) == 0L) {
    stop(
      "Sheet ", sQuote(this_sheet), " in ", sQuote(this_file), " is empty.",
      call. = FALSE
    )
  }

  data.table::setnames(
    raw_sheet,
    paste0("col_", excel_letters(ncol(raw_sheet)))
  )

  # Locate the header row. It is the first row carrying a Derivation label.
  rows_to_search <- seq_len(min(header_search_rows, nrow(raw_sheet)))

  header_row <- NA_integer_

  for (this_row in rows_to_search) {
    candidate <- normalise_header(
      unlist(raw_sheet[this_row], use.names = FALSE)
    )

    if (any(candidate == "derivation")) {
      header_row <- this_row
      break
    }
  }

  if (is.na(header_row)) {
    stop(
      "No header row containing 'Derivation' was found in the first ",
      length(rows_to_search),
      " rows of sheet ",
      sQuote(this_sheet),
      " in ",
      sQuote(this_file),
      ".",
      call. = FALSE
    )
  }

  header <- unlist(raw_sheet[header_row], use.names = FALSE)

  # Check that the spreadsheet contains at least seven columns.
  if (ncol(raw_sheet) < 7L) {
    stop(
      "The sheet ",
      sQuote(this_sheet),
      " in ",
      sQuote(this_file),
      " has only ",
      ncol(raw_sheet),
      " columns. Column G is column 7.",
      call. = FALSE
    )
  }

  if (header_row >= nrow(raw_sheet)) {
    # The header is the final row, so the sheet carries no derivations.
    spec_sheet <- raw_sheet[0L, ]
    spec_sheet[, excel_row := integer()]
  } else {
    spec_sheet <- raw_sheet[
      seq.int(from = header_row + 1L, to = nrow(raw_sheet)),
    ]

    # Record the worksheet row so entries can be traced back to the sheet.
    spec_sheet[, excel_row := seq.int(
      from = header_row + 1L,
      to = nrow(raw_sheet)
    )]
  }

  # Keep the original Column G for audit and comparison.
  spec_sheet[, column_g_original := col_G]

  data.table::setattr(spec_sheet, "header_row", header_row)
  data.table::setattr(spec_sheet, "header", header)

  # The rows above the header are needed as well: several sheets write the
  # level of each block on the row above the header rather than below it.
  data.table::setattr(
    spec_sheet,
    "preamble",
    raw_sheet[seq_len(header_row), ]
  )

  spec_sheet[]
}


#' Read a CSV spec file as a single sheet.
#' @keywords internal
#' @noRd
read_csv_spec <- function(this_file, this_sheet, header_search_rows = 10L) {
  lines <- readLines(this_file, warn = FALSE, encoding = "UTF-8")
  lines <- lines[!grepl("^\\s*#", lines) & !grepl("^\\s*$", lines)]

  if (length(lines) == 0L) {
    stop(
      "CSV spec file ", sQuote(this_file), " contains no data rows.",
      call. = FALSE
    )
  }

  raw_sheet <- utils::read.csv(
    text = paste(lines, collapse = "\n"),
    header = FALSE,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    blank.lines.skip = FALSE,
    quote = '"',
    na.strings = c("", "NA")
  )

  data.table::setDT(raw_sheet)

  if (ncol(raw_sheet) == 0L || nrow(raw_sheet) == 0L) {
    stop(
      "CSV spec file ", sQuote(this_file), " is empty.",
      call. = FALSE
    )
  }

  data.table::setnames(
    raw_sheet,
    paste0("col_", excel_letters(ncol(raw_sheet)))
  )

  rows_to_search <- seq_len(min(header_search_rows, nrow(raw_sheet)))
  header_row <- NA_integer_

  for (this_row in rows_to_search) {
    candidate <- normalise_header(
      unlist(raw_sheet[this_row], use.names = FALSE)
    )

    if (any(candidate == "derivation")) {
      header_row <- this_row
      break
    }
  }

  if (is.na(header_row)) {
    stop(
      "No header row containing 'Derivation' was found in the first ",
      length(rows_to_search),
      " rows of CSV spec ",
      sQuote(this_file),
      ".",
      call. = FALSE
    )
  }

  header <- unlist(raw_sheet[header_row], use.names = FALSE)

  if (ncol(raw_sheet) < 7L) {
    stop(
      "The CSV spec ", sQuote(this_file), " has only ",
      ncol(raw_sheet),
      " columns. Column G is column 7.",
      call. = FALSE
    )
  }

  if (header_row >= nrow(raw_sheet)) {
    spec_sheet <- raw_sheet[0L, ]
    spec_sheet[, excel_row := integer()]
  } else {
    spec_sheet <- raw_sheet[
      seq.int(from = header_row + 1L, to = nrow(raw_sheet)),
    ]

    spec_sheet[, excel_row := seq.int(
      from = header_row + 1L,
      to = nrow(raw_sheet)
    )]
  }

  spec_sheet[, column_g_original := col_G]

  data.table::setattr(spec_sheet, "header_row", header_row)
  data.table::setattr(spec_sheet, "header", header)
  data.table::setattr(spec_sheet, "preamble", raw_sheet[seq_len(header_row), ])

  spec_sheet[]
}
