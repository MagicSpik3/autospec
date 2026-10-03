# Sets up autospec on your machine: installs the packages it needs, builds its
# documentation, installs it, and runs its tests.
#
# How to run it:
#   1. Open autospec.Rproj in RStudio (this puts R in the right folder).
#   2. Restart R: Session > Restart R.
#   3. Run: source("setup_autospec.R")
#
# It is safe to run again at any time, for example after pulling new code.
# options(autospec.destructive_build = TRUE)
local({
  say <- function(kind, text) {
    if (requireNamespace("cli", quietly = TRUE)) {
      switch(kind,
        heading = cli::cli_h1(text),
        info = cli::cli_alert_info(text),
        done = cli::cli_alert_success(text),
        warn = cli::cli_alert_warning(text),
        fail = cli::cli_alert_danger(text)
      )
    } else {
      prefix <- c(heading = "\n== ", info = "i ", done = "v ", warn = "! ", fail = "x ")
      message(prefix[[kind]], text)
    }
  }

  give_up <- function(text) {
    say("fail", text)
    stop("Setup stopped. Fix the problem above and run setup_autospec.R again.", call. = FALSE)
  }

  # Step 1 ----------------------------------------------------------------------
  say("heading", "Step 1 of 8: checking R")

  if (getRversion() < "4.1.0") {
    give_up(paste0(
      "You have R ", getRversion(), ". autospec needs R 4.1 or newer. ",
      "Install a newer R from the ONS software centre, then run this again."
    ))
  }

  say("done", paste0("R ", getRversion(), " is new enough."))

  destructive_build <- isTRUE(getOption("autospec.destructive_build", FALSE)) ||
    identical(tolower(Sys.getenv("autospec_DESTRUCTIVE_BUILD", "FALSE")), "true")

  # Step 2 ----------------------------------------------------------------------
  say("heading", "Step 2 of 8: checking you are in the autospec folder")

  if (!file.exists("DESCRIPTION") ||
      !identical(unname(read.dcf("DESCRIPTION", fields = "Package")[1, 1]), "autospec")) {
    give_up(paste0(
      "R is working in ", getwd(), ", which is not the autospec folder. ",
      "Open autospec.Rproj in RStudio, or run setwd() with the folder path, then run this again."
    ))
  }

  if ("autospec" %in% loadedNamespaces()) {
    if (!destructive_build) {
      give_up(paste(
        "autospec is already loaded in this R session, so it cannot be reinstalled.",
        "Restart R (Session > Restart R in RStudio) and run this again.",
        "For a test rebuild, set options(autospec.destructive_build = TRUE) or autospec_DESTRUCTIVE_BUILD=true."
      ))
    }

    autospec:::unload_autospec_for_reinstall(destructive = TRUE)
    say("warn", "autospec was already loaded; it has been unloaded so the rebuild can continue without an R restart.")
  }

  say("done", paste0("Working in ", getwd(), "."))

  # Step 3 ----------------------------------------------------------------------
  say("heading", "Step 3 of 8: installing the packages autospec uses")

  description <- read.dcf("DESCRIPTION", fields = c("Imports", "Suggests"))
  listed <- unlist(strsplit(paste(description[1, ], collapse = ","), ","))
  listed <- trimws(sub("\\(.*\\)", "", listed))
  base_packages <- rownames(installed.packages(priority = "base"))
  needed <- setdiff(unique(c(listed[nzchar(listed)], "roxygen2", "testthat")), c(base_packages, "was.utils"))

  missing_packages <- needed[!vapply(needed, requireNamespace, logical(1L), quietly = TRUE)]

  if (length(missing_packages) == 0L) {
    say("done", paste0("All ", length(needed), " packages are already installed."))
  } else {
    say("info", paste0("Installing ", length(missing_packages), " package(s): ",
                       paste(missing_packages, collapse = ", "), ". This can take a few minutes."))

    install_messages <- character()

    tryCatch(
      withCallingHandlers(
        utils::install.packages(missing_packages),
        warning = function(problem) {
          install_messages <<- c(install_messages, conditionMessage(problem))
          invokeRestart("muffleWarning")
        }
      ),
      error = function(problem) {
        install_messages <<- c(install_messages, conditionMessage(problem))
      }
    )

    still_missing <- missing_packages[!vapply(missing_packages, requireNamespace, logical(1L), quietly = TRUE)]

    if (length(still_missing) > 0L) {
      give_up(paste0(
        "Could not install: ", paste(still_missing, collapse = ", "), ". ",
        if (length(install_messages) > 0L) paste0("R said: ", paste(install_messages, collapse = "; "), ". ") else "",
        "This is usually the package mirror. Run getOption(\"repos\") to see where R downloads from, ",
        "and ask the team for the ONS mirror setting if it is not set."
      ))
    }

    say("done", "Packages installed.")
  }

  # Step 4 ----------------------------------------------------------------------
  say("heading", "Step 4 of 8: checking for was.utils")

  if (requireNamespace("was.utils", quietly = TRUE)) {
    say("done", "was.utils is installed.")
  } else {
    local_copy <- normalizePath(file.path("..", "was.utils"), mustWork = FALSE)

    if (file.exists(file.path(local_copy, "DESCRIPTION"))) {
      say("info", paste0("Installing was.utils from ", local_copy, "."))
      tryCatch(
        utils::install.packages(local_copy, repos = NULL, type = "source"),
        error = function(problem) say("warn", paste0("was.utils did not install: ", conditionMessage(problem)))
      )
    }

    if (requireNamespace("was.utils", quietly = TRUE)) {
      say("done", "was.utils is installed.")
    } else {
      say("warn", paste(
        "was.utils is not installed. Everything works without it except dv_annualise(),",
        "which needs its period multipliers. To add it later, clone was.utils from the",
        "Household_Finance_Surveys GitLab group into the folder next to autospec and run this again."
      ))
    }
  }

  # Step 5 ----------------------------------------------------------------------
  say("heading", "Step 5 of 8: building the help pages")

  tryCatch(
    roxygen2::roxygenise("."),
    error = function(problem) give_up(paste0("Building the help pages failed: ", conditionMessage(problem)))
  )

  say("done", "Help pages built. Try ?run_dv_suite once autospec is loaded.")

  # Step 6 ----------------------------------------------------------------------
  say("heading", "Step 6 of 8: installing autospec")

  tryCatch(
    utils::install.packages(".", repos = NULL, type = "source"),
    warning = function(problem) give_up(paste0("Installing autospec failed: ", conditionMessage(problem))),
    error = function(problem) give_up(paste0("Installing autospec failed: ", conditionMessage(problem)))
  )

  if (!requireNamespace("autospec", quietly = TRUE)) {
    give_up("autospec did not install. Scroll up for the error R printed.")
  }

  say("done", paste0("autospec ", utils::packageVersion("autospec"), " is installed."))

  # Step 7 ----------------------------------------------------------------------
  say("heading", "Step 7 of 8: running the tests")

  results <- as.data.frame(testthat::test_local(".", stop_on_failure = FALSE, reporter = "summary"))
  failed <- sum(results$failed) + sum(results$error)

  if (failed == 0L) {
    say("done", paste0("All ", sum(results$nb), " checks passed."))
  } else {
    say("warn", paste0(
      failed, " test(s) failed; they are listed above. autospec is installed, but tell ",
      "the autospec maintainers before relying on it."
    ))
  }

  # Step 8 ----------------------------------------------------------------------
  say("heading", "Step 8 of 8: checking the config file")

  if (file.exists("autospec_config.yaml")) {
    say("done", "autospec_config.yaml exists. Check its round and file paths before running.")
  } else {
    autospec::create_autospec_config("autospec_config.yaml")
    say("warn", "Open autospec_config.yaml and set the round and the data file before running.")
  }

  say("heading", "Setup finished")
  say("info", "Next: open run_pipeline.Rmd and run it from the top, or use Addins > autospec: Where am I?")
})
