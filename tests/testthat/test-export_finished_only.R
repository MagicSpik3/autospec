tracking_data <- function() data.frame(hhserial = c(1, 2), a = c(1, 2))


export_tracked <- function(suite_dir, plan, finished_only = TRUE, scripts_dir = tempfile("scripts")) {
  utils::capture.output(suppressMessages(export_dv_scripts(
    suite_dir, scripts_dir, tracking_data(), title = "Test",
    plan = plan, finished_only = finished_only
  )))
  scripts_dir
}


read_left_out <- function(scripts_dir) {
  utils::read.csv(file.path(scripts_dir, "left_out.csv"), stringsAsFactors = FALSE)
}


# Stage 4: deliver finished DVs while the rest are debugged

test_that("finished DVs are written, with a populated unit test that passed", {
  expect_identical(finished_dvs(finished_plan()), c("done", "no_uid", "uses_broken"))
})


test_that("a plan without the tracking columns has no finished DVs", {
  expect_identical(finished_dvs(fake_plan_row("a")), character())
})


test_that("exporting only finished DVs leaves the rest out, even when their tests fail", {
  scripts_dir <- export_tracked(tracking_suite(), finished_plan())
  topic <- readLines(file.path(scripts_dir, "dvs", "topic.R"))

  expect_true(any(grepl("df$done <-", topic, fixed = TRUE)))
  expect_true(any(grepl("df$no_uid <-", topic, fixed = TRUE)))

  for (dv in c("broken", "draft", "untested", "uses_broken")) {
    expect_false(any(grepl(paste0("df$", dv, " <-"), topic, fixed = TRUE)), info = dv)
  }
})


test_that("the manifest lists each DV left out and why", {
  left_out <- read_left_out(export_tracked(tracking_suite(), finished_plan()))
  reason <- function(dv) left_out$reason[left_out$dv == dv]

  expect_setequal(left_out$dv, c("broken", "draft", "todo", "untested", "uses_broken"))
  expect_match(reason("todo"), "not written")
  expect_match(reason("untested"), "no unit test")
  expect_match(reason("draft"), "not populated")
  expect_match(reason("broken"), "not passed")
  # Finished itself, but it reads a DV that is not delivered
  expect_match(reason("uses_broken"), "broken")
})


test_that("exporting only finished DVs needs the plan", {
  expect_error(
    suppressMessages(export_dv_scripts(tracking_suite(), tempfile("scripts"), tracking_data(), finished_only = TRUE)),
    "plan"
  )
})


test_that("with no finished DVs nothing is exported", {
  plan <- finished_plan()
  plan$`Unit Test Passed` <- NA_character_
  scripts_dir <- tempfile("scripts")

  expect_error(
    suppressMessages(export_dv_scripts(tracking_suite(), scripts_dir, tracking_data(), plan = plan, finished_only = TRUE)),
    "No DVs are finished"
  )
  expect_false(dir.exists(scripts_dir))
})


test_that("a normal export also writes the manifest, listing DVs not written", {
  left_out <- read_left_out(export_tracked(tracking_suite(include_failing = FALSE), plan = NULL, finished_only = FALSE))

  expect_identical(left_out$dv, "todo")
  expect_match(left_out$reason, "not written")
})


test_that("a code error in a DV left out does not stop exporting the finished ones", {
  suite_dir <- tracking_suite()
  cat(
    "steps$bad <- dv_step(label = 'Bad', inputs = 'a', uid = 'U8', derive = function(df) {",
    "  df$bad <- no_such_function(df$a)",
    "  df",
    "})",
    sep = "\n", file = file.path(suite_dir, "topic.R"), append = TRUE
  )

  left_out <- read_left_out(export_tracked(suite_dir, finished_plan()))

  expect_match(left_out$reason[left_out$dv == "bad"], "not in the plan")
})
