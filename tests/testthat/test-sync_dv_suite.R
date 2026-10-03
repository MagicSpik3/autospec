data_names <- c("a", "s1", "s2", "hhserial")

starting_plan <- function() {
  rbind(
    fake_plan_row("copy_a", "dv_copy", args = "source_col = \"a\"", label = "Copy of a",
                  status = "auto", reviewed_by = NA),
    fake_plan_row("by_hand", status = "hand_written", reviewed_by = NA),
    fake_plan_row("C(1-2)", "dv_copy", args = "source_col = \"S(1-2)\"",
                  status = "needs_review", reviewed_by = NA),
    fake_plan_row("later", "dv_copy", args = "source_col = \"a\"", status = "auto", reviewed_by = NA)
  )
}


# A suite written before sign-off, with by_hand then written by hand
suite_before_sign_off <- function() {
  suite_dir <- file.path(tempfile(), "dv_suite")
  suppressMessages(write_dv_suite(starting_plan(), suite_dir, data_names = data_names))

  topic_path <- file.path(suite_dir, "test.R")
  lines <- readLines(topic_path)
  stub_line <- grep("derive = NULL", lines)[[2L]]
  lines[[stub_line]] <- "  derive = function(df) { df$by_hand <- df$a * 2; df } # my own code"
  writeLines(lines, topic_path)

  suite_dir
}


signed_off_plan <- function() {
  plan <- starting_plan()
  plan$status[c(1, 3)] <- "reviewed"
  plan$status[plan$dv == "later"] <- "duplicate"
  rbind(plan, fake_plan_row("brand_new", "dv_copy", args = "source_col = \"a\"",
                            status = "auto", reviewed_by = NA))
}


test_that("the index finds every step and whether it is written", {
  suite_dir <- suite_before_sign_off()
  index <- suite_step_index(suite_dir)

  expect_identical(index$dv, c("copy_a", "by_hand", "c1", "c2", "later"))
  expect_identical(index$written, c(FALSE, TRUE, FALSE, FALSE, FALSE))
  expect_true(all(grepl("^# ", readLines(file.path(suite_dir, "test.R"))[index$first_line])))
})


test_that("signed-off stubs are filled in, new DVs added and hand-written steps kept", {
  suite_dir <- suite_before_sign_off()

  actions <- suppressMessages(sync_dv_suite(signed_off_plan(), suite_dir, data_names = data_names))
  action_of <- function(dv) actions$action[actions$dv == dv]

  expect_identical(sort(actions$dv[actions$action == "filled in"]), c("c1", "c2", "copy_a"))
  expect_identical(action_of("by_hand"), "written by hand")
  expect_identical(action_of("brand_new"), "added")
  expect_identical(action_of("later"), "not in the plan")

  lines <- readLines(file.path(suite_dir, "test.R"))
  expect_true(any(grepl("# my own code", lines, fixed = TRUE)))
  expect_identical(sum(grepl("^steps\\$copy_a <- ", lines)), 1L)
  expect_identical(sum(grepl("^# copy_a -", lines)), 1L)
  expect_silent(parse(file.path(suite_dir, "test.R")))
})


test_that("the synced suite builds the DVs", {
  suite_dir <- suite_before_sign_off()
  suppressMessages(sync_dv_suite(signed_off_plan(), suite_dir, data_names = data_names))

  df <- data.frame(hhserial = 1:2, a = c(1, 2), s1 = c(5, 6), s2 = c(7, 8))
  result <- suppressMessages(run_dv_suite(df, suite_dir))

  expect_identical(result$copy_a, c(1, 2), ignore_attr = TRUE)
  expect_identical(result$c2, c(7, 8), ignore_attr = TRUE)
  expect_identical(result$by_hand, c(2, 4), ignore_attr = TRUE)
})


test_that("a file is backed up before it changes, and a new stub gets a test", {
  suite_dir <- suite_before_sign_off()
  suppressMessages(sync_dv_suite(signed_off_plan(), suite_dir, data_names = data_names))

  backups <- list.files(file.path(suite_dir, "_backup"), recursive = TRUE)
  expect_true(any(basename(backups) == "test.R"))
  expect_true(any(grepl("brand_new follows the spec", readLines(file.path(suite_dir, "tests", "test-test.R")))))
})


test_that("a second sync changes nothing", {
  suite_dir <- suite_before_sign_off()
  suppressMessages(sync_dv_suite(signed_off_plan(), suite_dir, data_names = data_names))
  before <- readLines(file.path(suite_dir, "test.R"))

  actions <- suppressMessages(sync_dv_suite(signed_off_plan(), suite_dir, data_names = data_names))

  expect_false(any(actions$action %in% c("filled in", "added")))
  expect_identical(readLines(file.path(suite_dir, "test.R")), before)
})


test_that("with no suite yet, sync writes one", {
  suite_dir <- file.path(tempfile(), "dv_suite")

  suppressMessages(sync_dv_suite(signed_off_plan(), suite_dir, data_names = data_names))

  expect_true(file.exists(file.path(suite_dir, "test.R")))
})


test_that("only the chosen topics are synced", {
  plan <- rbind(
    fake_plan_row("b", "dv_copy", args = "source_col = \"a\"", status = "auto",
                  file_name = "Property_Wealth_DV_Spec_R9.xlsx"),
    fake_plan_row("d", "dv_copy", args = "source_col = \"a\"", status = "auto",
                  file_name = "Income_DV_Spec_R9.xlsx")
  )
  suite_dir <- file.path(tempfile(), "dv_suite")
  suppressMessages(write_dv_suite(plan, suite_dir, data_names = "a"))
  plan$status <- "reviewed"

  actions <- suppressMessages(sync_dv_suite(plan, suite_dir, data_names = "a", topics = "income"))

  expect_identical(actions$dv, "d")
  expect_false(suite_step_index(suite_dir)$written[suite_step_index(suite_dir)$dv == "b"])
})


test_that("a suite file with a syntax error stops the sync and is named", {
  suite_dir <- suite_before_sign_off()
  cat("steps$broken <- dv_step(", file = file.path(suite_dir, "test.R"), append = TRUE)

  expect_error(suppressMessages(sync_dv_suite(signed_off_plan(), suite_dir)), "syntax error")
})


test_that("steps assigned with [[ ]] are found too", {
  expect_identical(assigned_step_name(quote(steps[["x"]] <- dv_step(NA, "a", NULL))), "x")
  expect_identical(assigned_step_name(quote(steps$`x(1)` <- dv_step(NA, "a", NULL))), "x(1)")
  expect_null(assigned_step_name(quote(settings <- list())))
})


test_that("derive is read as set or not, however the call is written", {
  expect_false(step_has_derive(quote(dv_step(label = NA, inputs = "a", derive = NULL))))
  expect_false(step_has_derive(quote(dv_step(NA, "a", NULL))))
  expect_true(step_has_derive(quote(dv_step(NA, "a", function(df) df))))
})
