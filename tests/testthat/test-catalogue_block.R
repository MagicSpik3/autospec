make_sheet <- function(variable, label, derivation, uids = rep(NA_character_, length(variable)),
                       first_row = 2L) {
  # A stand-in for get_spec() output: columns positioned by Excel reference,
  # with the worksheet row each entry came from.
  sheet <- data.table::data.table(
    col_A = variable,
    col_B = label,
    col_C = derivation,
    col_D = uids
  )

  sheet[, excel_row := seq.int(
    from = first_row,
    length.out = .N
  )]

  sheet[]
}

one_block <- data.table::data.table(
  block_column = "C",
  derivation_col = 3L,
  variable_col = 1L,
  label_col = 2L,
  uid_col = 4L,
  block = "Person"
)


test_that("each derivation is catalogued against its variable", {
  sheet <- make_sheet(
    variable = c("CurrAccR9", "SavAccR9"),
    label = c("Has a current account", "Has a savings account"),
    derivation = c("CurrAccR9 = 1", "SavAccR9 = 1")
  )

  result <- catalogue_block(sheet, one_block, "spec.xlsx", "Sheet1", "variable")

  expect_equal(nrow(result), 2L)
  expect_identical(result$variable, c("CurrAccR9", "SavAccR9"))
  expect_identical(result$excel_row, c(2L, 3L))
  expect_identical(result$block, c("Person", "Person"))
  expect_identical(result$block_column, c("C", "C"))
})


test_that("source requirement UIDs are preserved in the catalogue", {
  sheet <- make_sheet(
    variable = c("FirstDV", "SecondDV"),
    label = c("First", "Second"),
    derivation = c("FirstDV = a", "SecondDV = b"),
    uids = c("REQ-001", "REQ-002")
  )

  result <- catalogue_block(sheet, one_block, "spec.xlsx", "Sheet1", "variable")

  expect_identical(result$uid, c("REQ-001", "REQ-002"))
})


test_that("rows with no derivation are left out", {
  sheet <- make_sheet(
    variable = c("CurrAccR9", "InputOnlyR9"),
    label = c("Has a current account", "An input variable"),
    derivation = c("CurrAccR9 = 1", NA_character_)
  )

  result <- catalogue_block(sheet, one_block, "spec.xlsx", "Sheet1", "variable")

  expect_equal(nrow(result), 1L)
  expect_identical(result$variable, "CurrAccR9")
})


test_that("a derivation spread over rows is rejoined under its variable", {
  # Only the first row names the variable; the rest continue its derivation.
  sheet <- make_sheet(
    variable = c("DVHasCSCR9", NA_character_, NA_character_),
    label = c("Has a credit card", NA_character_, NA_character_),
    derivation = c(
      "IF TOTCSCR9_SUM > 0 THEN",
      "    DVHasCSCR9 = 1",
      "    OTHERWISE DVHasCSCR9 = 0"
    )
  )

  result <- catalogue_block(sheet, one_block, "spec.xlsx", "Sheet1", "variable")

  expect_equal(nrow(result), 1L)
  expect_identical(result$variable, "DVHasCSCR9")
  expect_identical(result$excel_row, 2L)
  expect_identical(
    result$instructions,
    paste(
      "IF TOTCSCR9_SUM > 0 THEN",
      "    DVHasCSCR9 = 1",
      "    OTHERWISE DVHasCSCR9 = 0",
      sep = "\n"
    )
  )
})


test_that("the keyword rule joins only the rows that read as continuations", {
  sheet <- make_sheet(
    variable = c("DVHasCSCR9", NA_character_, NA_character_),
    label = c("Has a credit card", NA_character_, NA_character_),
    derivation = c(
      "IF TOTCSCR9_SUM > 0 THEN",
      "    DVHasCSCR9 = 1",
      "    OTHERWISE DVHasCSCR9 = 0"
    )
  )

  result <- catalogue_block(sheet, one_block, "spec.xlsx", "Sheet1", "keyword")

  # The middle row opens with neither OTHERWISE nor ELSE, so it is stranded
  # as an entry of its own with no variable against it.
  expect_equal(nrow(result), 2L)
  expect_identical(result$variable, c("DVHasCSCR9", NA_character_))
})


test_that("a row naming a variable starts a new derivation under both rules", {
  # Two consecutive derivations, the second opening with ELSE. It names its
  # own variable, so it must not be absorbed into the first.
  sheet <- make_sheet(
    variable = c("FirstR9", "SecondR9"),
    label = c("First", "Second"),
    derivation = c("FirstR9 = 1", "ELSE SecondR9 = 0")
  )

  for (rule in c("variable", "keyword")) {
    result <- catalogue_block(sheet, one_block, "spec.xlsx", "Sheet1", rule)

    expect_equal(nrow(result), 2L)
    expect_identical(result$variable, c("FirstR9", "SecondR9"))
  }
})


test_that("section headings are carried down over the derivations below", {
  sheet <- make_sheet(
    variable = c("Transient debt", "TOTCSC_transR9", "TOTCSC_sumR9"),
    label = c(NA_character_, "Transient total", "Sum"),
    derivation = c(
      NA_character_,
      "TOTCSC_transR9 = 1",
      "TOTCSC_sumR9 = 2"
    )
  )

  result <- catalogue_block(sheet, one_block, "spec.xlsx", "Sheet1", "variable")

  expect_equal(nrow(result), 2L)
  expect_identical(result$section, c("Transient debt", "Transient debt"))
})


test_that("derivations before the first section heading have no section", {
  sheet <- make_sheet(
    variable = c("EarlyR9", "Persistent debt", "LaterR9"),
    label = c("Early", NA_character_, "Later"),
    derivation = c("EarlyR9 = 1", NA_character_, "LaterR9 = 2")
  )

  result <- catalogue_block(sheet, one_block, "spec.xlsx", "Sheet1", "variable")

  expect_identical(result$section, c(NA_character_, "Persistent debt"))
})


test_that("a labelled row without a derivation is not a section heading", {
  # It is a variable that simply has no derivation stated, so it must not be
  # carried down as a heading over the rows that follow.
  sheet <- make_sheet(
    variable = c("NoDerivationR9", "LaterR9"),
    label = c("Has a label", "Later"),
    derivation = c(NA_character_, "LaterR9 = 2")
  )

  result <- catalogue_block(sheet, one_block, "spec.xlsx", "Sheet1", "variable")

  expect_equal(nrow(result), 1L)
  expect_identical(result$section, NA_character_)
})


test_that("a block with no derivations gives no entries", {
  sheet <- make_sheet(
    variable = c("OneR9", "TwoR9"),
    label = c("One", "Two"),
    derivation = c(NA_character_, "   ")
  )

  result <- catalogue_block(sheet, one_block, "spec.xlsx", "Sheet1", "variable")

  expect_equal(nrow(result), 0L)
})


test_that("a block beyond the end of the sheet gives no entries", {
  sheet <- make_sheet(
    variable = "OneR9",
    label = "One",
    derivation = "OneR9 = 1"
  )

  distant_block <- data.table::data.table(
    block_column = "Z",
    derivation_col = 26L,
    variable_col = 24L,
    label_col = 25L,
    block = NA_character_
  )

  result <- catalogue_block(
    sheet, distant_block, "spec.xlsx", "Sheet1", "variable"
  )

  expect_equal(nrow(result), 0L)
})


test_that("a block with no label column is catalogued without labels", {
  sheet <- make_sheet(
    variable = "OneR9",
    label = "ignored",
    derivation = "OneR9 = 1"
  )

  unlabelled <- data.table::data.table(
    block_column = "C",
    derivation_col = 3L,
    variable_col = 1L,
    label_col = NA_integer_,
    block = NA_character_
  )

  result <- catalogue_block(sheet, unlabelled, "spec.xlsx", "Sheet1", "variable")

  expect_equal(nrow(result), 1L)
  expect_identical(result$label, NA_character_)
})


test_that("value labels and notes on continuation rows stay with the derivation", {
  sheet <- data.table::data.table(
    col_A = c("FlagR9", NA_character_, NA_character_),
    col_B = c("A flag", NA_character_, NA_character_),
    col_C = c("IF x > 0 THEN", "  FlagR9 = 1", NA_character_),
    col_D = c("1 = Yes", "0 = No", NA_character_),
    col_E = c(NA_character_, NA_character_, "Built from x")
  )
  sheet[, excel_row := 2:4]

  block <- data.table::data.table(
    block_column = "C",
    derivation_col = 3L,
    variable_col = 1L,
    label_col = 2L,
    value_labels_col = 4L,
    notes_col = 5L,
    block = "Person",
    level = "person"
  )

  result <- catalogue_block(sheet, block, "spec.xlsx", "Sheet1", "variable")

  expect_equal(nrow(result), 1L)
  expect_identical(result$value_labels, paste("1 = Yes", "0 = No", sep = "\n"))
  expect_identical(result$notes, "Built from x")
  expect_identical(result$level, "person")
  expect_identical(result$instructions, paste("IF x > 0 THEN", "  FlagR9 = 1", sep = "\n"))
})


test_that("a block with no level recorded is marked unknown", {
  sheet <- make_sheet(variable = "OneR9", label = "One", derivation = "OneR9 = 1")

  result <- catalogue_block(sheet, one_block, "spec.xlsx", "Sheet1", "variable")

  expect_identical(result$level, "unknown")
})
