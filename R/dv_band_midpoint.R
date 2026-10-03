#' Replace a band code with the midpoint of its band
#'
#' Other negative codes are kept as they are. A non-negative code not in the
#' table gives `NA` with a warning.
#'
#' @param df A data frame.
#' @param band_col Band code column.
#' @param new_col Column to create.
#' @param table Name of a table in [band_midpoint_tables()].
#' @param sentinels What to do with -8/-9 codes (decision D1): `"keep"` keeps
#'   them, `"zero"` puts 0, `"missing"` puts `NA`, `"stop"` refuses.
#'
#' @return `df` with `new_col` added.
#' @export
#'
#' @examples
#' df <- data.frame(UGdVbSR9_i = c(1, 3, -9))
#' dv_band_midpoint(df, "UGdVbSR9_i", "HousGdsTR9", table = "ugdvbs", sentinels = "zero")
dv_band_midpoint <- function(df, band_col, new_col, table, sentinels = "keep") {
  verb <- "dv_band_midpoint"
  check_verb_basics(df, new_col, verb)
  check_columns_exist(df, band_col, verb)
  check_columns_numeric(df, band_col, verb)
  check_sentinels_setting(sentinels, verb, allow_keep = TRUE)

  tables <- band_midpoint_tables()

  if (missing(table) || !is.character(table) || length(table) != 1L ||
      !table %in% names(tables)) {
    verb_stop(verb, paste0(
      "`table` must be one of: ", paste(names(tables), collapse = ", "), "."
    ))
  }

  codes <- as.numeric(df[[band_col]])
  midpoints <- apply_midpoint(codes, tables[[table]])

  negative <- !is.na(codes) & codes < 0
  midpoints[negative] <- codes[negative]

  unknown <- !is.na(codes) & !negative & is.na(midpoints)

  if (any(unknown)) {
    warning(
      verb, "(): ", sum(unknown), " rows have a band code not in the ", table,
      " table and were set to NA.",
      call. = FALSE
    )
  }

  cleaned <- apply_sentinels(unname(midpoints), codes %in% sentinel_codes(), sentinels, verb, band_col)
  df[[new_col]] <- cleaned$values

  report_verb(new_col, paste0(
    "band midpoints of ", band_col, " from the ", table, " table",
    sentinel_note(structure(list(), replaced = cleaned$replaced), sentinels)
  ))

  df
}


#' Band midpoint lookup tables
#'
#' Each is a named numeric vector: names are band codes, values are pounds.
#' The top band is capped at 300,000 pounds.
#'
#' @return A named list of named numeric vectors.
#' @export
#'
#' @examples
#' band_midpoint_tables()$gcontvb
band_midpoint_tables <- function() {
  list(
    gcontvb = gcontvb_lookup,
    ugdvbs = ugdvbs_lookup,
    ugdvbl = ugdvbl_lookup,
    ugdvbos = ugdvbos_lookup
  )
}
