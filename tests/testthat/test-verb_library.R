test_that("the library holds the fourteen verbs, each described", {
  library_table <- verb_library()

  expect_identical(nrow(library_table), 14L)
  expect_false(anyDuplicated(library_table$verb) > 0L)
  expect_true(all(nzchar(library_table$what_it_does)))
  expect_true(all(nzchar(library_table$spec_wording_it_matches)))
})


test_that("every verb takes the data first and the DV name", {
  for (verb in registered_verbs()) {
    argument_names <- names(verb_formals(verb))

    expect_identical(argument_names[[1L]], "df", info = verb)
    expect_true("new_col" %in% argument_names, info = verb)
  }
})


test_that("the arguments table lists exactly what each function takes", {
  arguments <- verb_arguments()

  expect_setequal(unique(arguments$verb), registered_verbs())

  for (verb in registered_verbs()) {
    rows <- arguments[arguments$verb == verb, , drop = FALSE]
    expected <- setdiff(names(verb_formals(verb)), c("df", "new_col"))

    expect_identical(rows$argument, expected, info = verb)

    defaults <- vapply(rows$argument, formal_default, character(1L), verb = verb, USE.NAMES = FALSE)

    expect_identical(rows$must_be_given, ifelse(nzchar(defaults), "no", "yes"), info = verb)
    expect_identical(rows$if_not_given, gsub("\"", "", defaults), info = verb)
  }
})


test_that("the arguments table uses only the plain-English values", {
  arguments <- verb_arguments()

  expect_true(all(arguments$where_it_is_set %in% c("plan args", "plan condition", "settings.R")))
  expect_true(all(arguments$must_be_given %in% c("yes", "no")))
  expect_true(all(arguments$what_to_give %in% c(
    "one column name", "several column names", "a number", "a number or NA",
    "one or more numbers", "TRUE or FALSE", "a condition", "text", "annual or monthly",
    "stop, missing or zero", "keep, stop, missing or zero"
  )))
  expect_true(all(nzchar(arguments$what_it_is_for)))
  expect_identical(nzchar(arguments$settings_name), arguments$where_it_is_set == "settings.R")
})


test_that("every setting a verb reads is in the settings template", {
  arguments <- verb_arguments()
  settings_env <- new.env()
  eval(parse(text = settings_file_lines()), envir = settings_env)

  expect_true(all(
    arguments$settings_name[arguments$where_it_is_set == "settings.R"] %in% names(settings_env$settings)
  ))
})


test_that("show_verbs prints a verb and refuses unknown names", {
  expect_message(show_verbs("dv_keep_if"), "Copies a value where a condition is true")
  expect_error(suppressMessages(show_verbs("dv_nonsense")), "Unknown verb")
})
