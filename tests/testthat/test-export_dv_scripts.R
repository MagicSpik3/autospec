# A suite using every verb once, plus a hand-written step, so each plain-R
# emitter is proved against its verb by the exporter's own check.
export_example_suite <- function(extra_savings = character(), extra_debt = character()) {
  suite_dir <- tempfile("dv_suite")
  dir.create(file.path(suite_dir, "tests"), recursive = TRUE)
  writeLines(c(
    "settings <- list(by = \"hhserial\", na_as_zero = TRUE, codes = c(-8, -9), replacement = 0,",
    "                 sentinels = \"zero\", sentinels_in_calculations = \"missing\", rate = 0.05, target_age = 66)"
  ), file.path(suite_dir, "settings.R"))
  writeLines(c(
    "steps <- list()",
    "steps$copy_a <- dv_step(label = 'Copy of a', inputs = 'a',",
    "  derive = function(df) dv_copy(df, source_col = 'a', new_col = 'copy_a', sentinels = settings$sentinels))",
    "steps$total <- dv_step(label = 'Total', inputs = c('copy_a', 'b'),",
    "  derive = function(df) dv_row_total(df, value_cols = c('copy_a', 'b'), new_col = 'total', na_as_zero = settings$na_as_zero, sentinels = settings$sentinels))",
    "steps$gap <- dv_step(label = 'Gap', inputs = c('a', 'b'),",
    "  derive = function(df) dv_difference(df, minuend_col = 'a', subtrahend_col = 'b', new_col = 'gap', sentinels = settings$sentinels_in_calculations))",
    "steps$hh_total <- dv_step(label = 'Household total', inputs = 'total',",
    "  derive = function(df) dv_sum_to_household(df, value_col = 'total', new_col = 'hh_total', by = settings$by, na_as_zero = settings$na_as_zero, sentinels = settings$sentinels))",
    "steps$has_a <- dv_step(label = 'Has a', inputs = 'a',",
    "  derive = function(df) dv_flag_positive(df, value_col = 'a', new_col = 'has_a', threshold = 0, sentinels = settings$sentinels))",
    "steps$hh_has_a <- dv_step(label = 'Anyone has a', inputs = 'has_a',",
    "  derive = function(df) dv_any_to_household(df, flag_col = 'has_a', new_col = 'hh_has_a', by = settings$by))",
    "steps$big <- dv_step(label = 'Big', inputs = 'a',",
    "  derive = function(df) dv_flag_if(df, condition = df$a > 5 & df$a < 100, new_col = 'big', otherwise = 0))",
    "steps$kept <- dv_step(label = 'Kept', inputs = c('a', 'code'),",
    "  derive = function(df) dv_keep_if(df, condition = df$code %in% c(1, 2), value_col = 'a', new_col = 'kept', otherwise = NA, sentinels = settings$sentinels))",
    "steps$any_two <- dv_step(label = 'Any two', inputs = c('code', 'code2'),",
    "  derive = function(df) dv_flag_any_of(df, value_cols = c('code', 'code2'), match_values = c(2, 3), new_col = 'any_two'))",
    "steps$cleared <- dv_step(label = 'Cleared', inputs = 'a',",
    "  derive = function(df) dv_clear_sentinels(df, value_col = 'a', new_col = 'cleared', codes = settings$codes, replacement = settings$replacement))",
    "steps$share <- dv_step(label = 'Share', inputs = c('a', 'b'),",
    "  derive = function(df) dv_ratio(df, numerator_col = 'a', denominator_col = 'b', new_col = 'share', otherwise = 0, sentinels = settings$sentinels_in_calculations))",
    "steps$today <- dv_step(label = 'Today', inputs = c('a', 'age'),",
    "  derive = function(df) dv_discount_to_present_value(df, amount_col = 'a', age_col = 'age', new_col = 'today', rate = settings$rate, target_age = settings$target_age, sentinels = settings$sentinels_in_calculations))",
    "steps$mid <- dv_step(label = 'Midpoint', inputs = 'band',",
    "  derive = function(df) dv_band_midpoint(df, band_col = 'band', new_col = 'mid', table = 'ugdvbs', sentinels = settings$sentinels))",
    "steps$yearly <- dv_step(label = 'Yearly', inputs = c('amount', 'period'),",
    "  derive = function(df) dv_annualise(df, amount_col = 'amount', period_col = 'period', new_col = 'yearly', period_codes = c(1, 2), multipliers = c(52, 12), sentinels = settings$sentinels))",
    extra_savings
  ), file.path(suite_dir, "savings.R"))
  writeLines(c(
    "steps <- list()",
    "steps$hrp_a <- dv_step(label = 'HRP a', inputs = c('hrp', 'copy_a'),",
    "  derive = function(df) {",
    "    # The HRP's value, given to everyone in the household",
    "    household <- df[[settings$by]]",
    "    is_hrp <- df$hrp %in% 1",
    "    df$hrp_a <- df$copy_a[is_hrp][match(household, household[is_hrp])]",
    "    df",
    "  })",
    "steps$todo <- dv_step(label = 'To do', inputs = 'a', derive = NULL)",
    extra_debt
  ), file.path(suite_dir, "debt.R"))

  suite_dir
}


export_example_data <- function() {
  data.frame(
    hhserial = c(1, 1, 2, 2, 3),
    hrp = c(1, 0, 0, 1, 1),
    a = c(10, -9, 3, 200, -8),
    b = c(5, 2, -9, 0, 1),
    code = c(1, 2, 3, -9, NA),
    code2 = c(0, 0, 3, NA, NA),
    age = c(30, 40, 70, 50, 60),
    band = c(1, 3, -8, 11, 5),
    amount = c(100, 0, -8, 50, 20),
    period = c(1, 2, 1, -9, 2)
  )
}


export_quietly <- function(suite_dir, scripts_dir = tempfile("scripts"), data = export_example_data()) {
  suppressMessages(export_dv_scripts(suite_dir, scripts_dir, data, title = "Test survey"))
  scripts_dir
}


test_that("every verb exports as plain R that matches the suite", {
  scripts_dir <- export_quietly(export_example_suite())

  expect_setequal(list.files(scripts_dir),
                  c("dvs", "run_dvs.R", "dv_config.xlsx", "README.md", paste0(basename(scripts_dir), ".Rproj")))
  expect_setequal(list.files(file.path(scripts_dir, "dvs")), c("savings.R", "debt.R"))

  savings <- readLines(file.path(scripts_dir, "dvs", "savings.R"))
  expect_false(any(grepl("dv_step|dv_[a-z_]+\\(|settings\\$|library\\(", savings)))
  expect_true("df$copy_a <- df$a" %in% savings)
  expect_true("df$copy_a[df$copy_a %in% c(-8, -9)] <- 0" %in% savings)
  expect_true("df$big <- ifelse(df$a > 5 & df$a < 100, 1, 0)" %in% savings)
  expect_true("df$hh_total <- ave(amount, paste(df$hhserial), FUN = sum)" %in% savings)
  expect_true("attr(df$total, \"label\") <- \"Total\"" %in% savings)
  expect_true(any(grepl("^# Needs: copy_a, b$", savings)))

  debt <- readLines(file.path(scripts_dir, "dvs", "debt.R"))
  expect_true("household <- df[[\"hhserial\"]]" %in% debt)
  expect_true("# The HRP's value, given to everyone in the household" %in% debt)
  expect_false(any(grepl("^df$|return\\(df\\)|function\\(df\\)", debt)))

  readme <- readLines(file.path(scripts_dir, "README.md"))
  expect_true("- todo" %in% readme)
})


test_that("run_dvs.R reads the config, builds the DVs and saves rds and sav", {
  skip_if_not_installed("haven")
  scripts_dir <- normalizePath(export_quietly(export_example_suite()), winslash = "/")
  data_file <- file.path(scripts_dir, "input.csv")
  utils::write.csv(export_example_data(), data_file, row.names = FALSE)
  save_as <- file.path(scripts_dir, "output")

  set_run_config(file.path(scripts_dir, "dv_config.xlsx"), data_file = data_file, save_as = save_as)

  rscript <- file.path(R.home("bin"), "Rscript")
  log <- system2(rscript, c("--vanilla", "-e", shQuote(paste0("setwd('", scripts_dir, "'); source('run_dvs.R')"))),
                 stdout = TRUE, stderr = TRUE)

  expect_true(file.exists(paste0(save_as, ".rds")), info = paste(log, collapse = "\n"))
  expect_true(file.exists(paste0(save_as, ".sav")))

  result <- readRDS(paste0(save_as, ".rds"))
  expected <- suppressMessages(run_dv_suite(export_example_data(), export_example_suite()))
  expect_equal(as.numeric(result$total), as.numeric(expected$total))
  expect_equal(as.numeric(result$hrp_a), as.numeric(expected$hrp_a))
  expect_identical(attr(haven::read_sav(paste0(save_as, ".sav"))$total, "label"), "Total")

  report <- utils::read.csv(paste0(save_as, "_dv_report.csv"))
  expect_true(all(c("total", "hrp_a") %in% report$DV))
  expect_true(any(grepl("hh_total", log)))
})


test_that("exporting again keeps the settings and choices in the config", {
  suite_dir <- export_example_suite()
  scripts_dir <- export_quietly(suite_dir)
  config_file <- file.path(scripts_dir, "dv_config.xlsx")

  set_run_config(config_file, data_file = "C:/data/in.sav", run = c(`debt.R` = "No"))
  export_quietly(suite_dir, scripts_dir)

  settings <- readxl::read_excel(config_file, sheet = "Settings")
  expect_identical(settings$Value[settings$Setting == "Data file"], "C:/data/in.sav")
  topics <- readxl::read_excel(config_file, sheet = "Topics")
  expect_identical(topics$`Run?`[topics$File == "debt.R"], "No")
  expect_identical(topics$`Run?`[topics$File == "savings.R"], "Yes")
})


test_that("hand-written code mixing a verb with other code is refused", {
  suite_dir <- export_example_suite(extra_debt = c(
    "steps$mixed <- dv_step(label = 'Mixed', inputs = 'a', derive = function(df) {",
    "  df <- dv_copy(df, 'a', 'mixed')",
    "  df$mixed <- df$mixed * 2",
    "  df",
    "})"
  ))

  expect_error(export_quietly(suite_dir), "mixed: hand-written code calls dv_copy")
})


test_that("annualising with the was.utils table is refused", {
  suite_dir <- export_example_suite(extra_savings = c(
    "steps$yearly2 <- dv_step(label = 'Yearly 2', inputs = c('amount', 'period'),",
    "  derive = function(df) dv_annualise(df, amount_col = 'amount', period_col = 'period', new_col = 'yearly2', sentinels = 'keep'))"
  ))

  expect_error(export_quietly(suite_dir), "period_codes")
})


test_that("topics that need each other in both directions are refused", {
  suite_dir <- export_example_suite(
    extra_savings = "steps$from_debt <- dv_step(label = 'From debt', inputs = 'hrp_a', derive = function(df) dv_copy(df, 'hrp_a', 'from_debt'))"
  )

  expect_error(export_quietly(suite_dir), "both directions")
})




test_that("plain R that gives a different answer stops the export", {
  suite_dir <- export_example_suite()
  scripts_dir <- tempfile("scripts")

  # A verb whose plain R is deliberately wrong, so the check has something to catch
  emitters <- plain_emitters()
  emitters$dv_copy <- function(args, dv) paste0(col_ref(dv), " <- ", col_ref(args$source_col), " + 1")
  local_mocked_bindings(plain_emitters = function() emitters)

  expect_error(suppressMessages(export_dv_scripts(suite_dir, scripts_dir, export_example_data())), "does not match")
  expect_false(dir.exists(scripts_dir))
})


test_that("a DV the suite cannot build on the data is left out, not shipped", {
  scripts_dir <- tempfile("scripts")
  data <- export_example_data()
  data$band <- NULL

  messages <- character()
  withCallingHandlers(
    export_dv_scripts(export_example_suite(), scripts_dir, data),
    message = function(condition) {
      messages <<- c(messages, conditionMessage(condition))
      invokeRestart("muffleMessage")
    }
  )

  savings <- readLines(file.path(scripts_dir, "dvs", "savings.R"))
  expect_false(any(grepl("df$mid", savings, fixed = TRUE)))
  expect_true(any(grepl("1 DV whose input is missing", messages)))
  expect_true("- mid" %in% readLines(file.path(scripts_dir, "README.md")))
})


test_that("each file lists the DVs it builds, so unchanged ones are still reported", {
  scripts_dir <- export_quietly(export_example_suite())
  savings <- readLines(file.path(scripts_dir, "dvs", "savings.R"))

  expect_true(any(grepl("^dvs_in_file <- ", savings)))
  expect_true(any(grepl("\"copy_a\"", savings)))
  expect_true(any(grepl("for (dv in dvs_in_file)", readLines(file.path(scripts_dir, "run_dvs.R")), fixed = TRUE)))
})


test_that("a verb argument left at its default is written out, not dropped", {
  suite_dir <- export_example_suite(extra_savings = c(
    "steps$plain_copy <- dv_step(label = 'Plain copy', inputs = 'b',",
    "  derive = function(df) dv_copy(df, 'b', 'plain_copy'))"
  ))

  savings <- readLines(file.path(export_quietly(suite_dir), "dvs", "savings.R"))

  # dv_copy() defaults to sentinels = "keep", so no sentinel line follows the copy
  expect_true("df$plain_copy <- df$b" %in% savings)
  expect_false(any(grepl("plain_copy[df$plain_copy %in%", savings, fixed = TRUE)))
})


test_that("the spec reference names the workbook without our folders", {
  suite_dir <- export_example_suite(extra_savings = c(
    "# D:/network/specs/Physical_Wealth_DV_Spec_R9.xlsx, sheet Physical_R9, row 5",
    "#   HousGdsTR9 = a",
    "steps$from_spec <- dv_step(label = 'From spec', inputs = 'a',",
    "  derive = function(df) dv_copy(df, 'a', 'from_spec'))"
  ))

  savings <- readLines(file.path(export_quietly(suite_dir), "dvs", "savings.R"))

  expect_true("# Spec: Physical_Wealth_DV_Spec_R9.xlsx, sheet Physical_R9, row 5" %in% savings)
  expect_false(any(grepl("D:/network", savings, fixed = TRUE)))
})


test_that("each block shows its SPSS above the R, and hand-written ones say where to look", {
  scripts_dir <- export_quietly(export_example_suite())
  savings <- readLines(file.path(scripts_dir, "dvs", "savings.R"))

  copy_at <- which(savings == "df$copy_a <- df$a")
  expect_equal(savings[copy_at - 4:1], c(
    "# In SPSS:",
    "#   COMPUTE copy_a = a.",
    "#   IF (ANY(copy_a, -8, -9)) copy_a = 0.",
    "#   VARIABLE LABELS copy_a \"Copy of a\"."
  ))
  expect_true("#   DO IF (a > 5 AND a < 100)." %in% savings)
  expect_true("#   RECODE period (1=52) (2=12) (ELSE=SYSMIS) INTO yearly." %in% savings)
  expect_true(any(grepl("declared MISSING VALUES", savings, fixed = TRUE)))

  debt <- readLines(file.path(scripts_dir, "dvs", "debt.R"))
  expect_true("#   * No SPSS given: this DV is written in R by hand, see the R below." %in% debt)

  # The SPSS is comments only, so the R still runs and matches: the export's own check passed
  expect_false(any(grepl("^(COMPUTE|IF|DO|RECODE|AGGREGATE)", c(savings, debt))))
})


test_that("the export makes an RStudio project, keeping one already there", {
  scripts_dir <- export_quietly(export_example_suite())
  project <- paste0(basename(scripts_dir), ".Rproj")

  expect_true(file.exists(file.path(scripts_dir, project)))
  expect_true(any(grepl(project, readLines(file.path(scripts_dir, "README.md")), fixed = TRUE)))

  file.rename(file.path(scripts_dir, project), file.path(scripts_dir, "ours.Rproj"))
  export_quietly(export_example_suite(), scripts_dir)

  expect_equal(list.files(scripts_dir, pattern = "[.]Rproj$"), "ours.Rproj")
})


test_that("the Topics sheet has an Order that runs each file after the files it uses", {
  scripts_dir <- export_quietly(export_example_suite())

  topics <- readxl::read_excel(file.path(scripts_dir, "dv_config.xlsx"), sheet = "Topics")

  expect_identical(names(topics), c("File", "Run?", "Order", "DVs", "Uses DVs from"))
  expect_identical(topics$File, c("savings.R", "debt.R"))
  expect_equal(topics$Order, c(1, 2))
  expect_identical(topics$`Uses DVs from`[topics$File == "debt.R"], "savings.R")
  expect_true(topics$`Uses DVs from`[topics$File == "savings.R"] %in% c(NA, ""))

  debt <- readLines(file.path(scripts_dir, "dvs", "debt.R"))
  expect_true("# This file uses DVs from: savings.R." %in% debt)
  savings <- readLines(file.path(scripts_dir, "dvs", "savings.R"))
  expect_true("# This file uses DVs from: no other file." %in% savings)
})


test_that("exporting again keeps the Order users set, and warns when it cannot work", {
  suite_dir <- export_example_suite()
  scripts_dir <- export_quietly(suite_dir)
  config_file <- file.path(scripts_dir, "dv_config.xlsx")
  set_run_config(config_file, order = c(`debt.R` = 1, `savings.R` = 2))

  messages <- messages_from(export_dv_scripts(suite_dir, scripts_dir, export_example_data(), title = "Test survey"))

  topics <- readxl::read_excel(config_file, sheet = "Topics")
  expect_identical(topics$File, c("debt.R", "savings.R"))
  expect_true(any(grepl("debt.R (Order 1) uses DVs from savings.R", messages, fixed = TRUE)))
})


test_that("choices in a config with numbered file names are kept", {
  suite_dir <- export_example_suite()
  scripts_dir <- export_quietly(suite_dir)
  config_file <- file.path(scripts_dir, "dv_config.xlsx")

  workbook <- openxlsx::loadWorkbook(config_file)
  old_topics <- data.frame(File = c("1_savings.R", "2_debt.R"), DVs = c(15, 2), `Run?` = c("Yes", "No"),
                           check.names = FALSE)
  openxlsx::removeWorksheet(workbook, "Topics")
  openxlsx::addWorksheet(workbook, "Topics")
  openxlsx::writeData(workbook, "Topics", old_topics)
  openxlsx::saveWorkbook(workbook, config_file, overwrite = TRUE)

  export_quietly(suite_dir, scripts_dir)

  topics <- readxl::read_excel(config_file, sheet = "Topics")
  expect_identical(topics$`Run?`[topics$File == "debt.R"], "No")
  expect_equal(topics$Order, c(1, 2))
})


test_that("new files go after the numbers users set", {
  expect_identical(kept_order(c(3, NA, 1, NA), c(1, 2, 3, 4)), c(3, 4, 1, 5))
  expect_identical(kept_order(c(NA, NA), c(1, 2)), c(1, 2))
})


test_that("a file ordered before one it uses DVs from is found", {
  topics <- data.frame(File = c("a.R", "b.R", "c.R"), Order = c(2, 1, 3),
                       `Uses DVs from` = c("", "a.R", "a.R, b.R"), check.names = FALSE)

  expect_identical(run_order_problems(topics), "b.R (Order 1) uses DVs from a.R, which must have a lower Order")

  topics$Order <- c(1, 2, 3)
  expect_identical(run_order_problems(topics), character())
})


test_that("run_dvs.R runs files in the Order given and stops when it cannot work", {
  skip_if_not_installed("haven")
  scripts_dir <- normalizePath(export_quietly(export_example_suite()), winslash = "/")
  data_file <- file.path(scripts_dir, "input.csv")
  utils::write.csv(export_example_data(), data_file, row.names = FALSE)
  config_file <- file.path(scripts_dir, "dv_config.xlsx")
  rscript <- file.path(R.home("bin"), "Rscript")
  run_scripts <- function() {
    system2(rscript, c("--vanilla", "-e", shQuote(paste0("setwd('", scripts_dir, "'); source('run_dvs.R')"))),
            stdout = TRUE, stderr = TRUE)
  }

  set_run_config(config_file, data_file = data_file, save_as = file.path(scripts_dir, "output"),
                 order = c(`savings.R` = 5, `debt.R` = 9))
  log <- run_scripts()
  expect_true(any(grepl("Running, in this order: savings.R > debt.R", log)), info = paste(log, collapse = "\n"))

  set_run_config(config_file, order = c(`savings.R` = 2, `debt.R` = 1))
  log <- run_scripts()
  expect_true(any(grepl("debt.R uses DVs from savings.R, so on the Topics sheet give savings.R a lower Order", log)),
              info = paste(log, collapse = "\n"))

  set_run_config(config_file, order = c(`savings.R` = 1, `debt.R` = 1))
  log <- run_scripts()
  expect_true(any(grepl("share an Order number", log)), info = paste(log, collapse = "\n"))
})


test_that("the plain R matches the suite on data spelling its columns in another case", {
  suite <- suppressMessages(read_dv_suite(export_example_suite()))
  steps <- suite$steps[vapply(suite$steps, function(step) is.function(step$derive), logical(1L))]
  steps <- steps[order_suite_steps(steps)]
  blocks <- lapply(steps, plain_step_code, settings = suite$settings)
  upper_data <- export_example_data()
  names(upper_data) <- toupper(names(upper_data))

  expect_identical(suppressMessages(check_scripts_match(upper_data, suite, blocks)), character())
})


test_that("topic files match the data's case and run_dvs.R keeps the data's spelling", {
  skip_if_not_installed("haven")
  scripts_dir <- normalizePath(export_quietly(export_example_suite()), winslash = "/")
  upper_data <- export_example_data()
  names(upper_data) <- toupper(names(upper_data))
  upper_data$TOTAL <- 0
  data_file <- file.path(scripts_dir, "input.rds")
  saveRDS(upper_data, data_file)
  save_as <- file.path(scripts_dir, "output")
  set_run_config(file.path(scripts_dir, "dv_config.xlsx"), data_file = data_file, save_as = save_as)

  rscript <- file.path(R.home("bin"), "Rscript")
  log <- system2(rscript, c("--vanilla", "-e", shQuote(paste0("setwd('", scripts_dir, "'); source('run_dvs.R')"))),
                 stdout = TRUE, stderr = TRUE)

  result <- readRDS(paste0(save_as, ".rds"))
  expected <- suppressMessages(run_dv_suite(upper_data, export_example_suite()))
  expect_true(all(names(upper_data) %in% names(result)), info = paste(log, collapse = "\n"))
  expect_false("total" %in% names(result))
  expect_equal(as.numeric(result$TOTAL), as.numeric(expected$TOTAL))
  report <- utils::read.csv(paste0(save_as, "_dv_report.csv"))
  expect_identical(report$Outcome[report$DV == "TOTAL"], "replaced")
})
