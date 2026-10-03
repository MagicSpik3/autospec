# autospec

DV refers to 'derived variable' - a function that creates a new column in a data frame reliant on existing columns in the dataframe.
It is simple and performs every row of the data frame in the same way.
DV Suite is teh set of operations that have been created.
Builds functoins specification workbooks.

autospec does not build DVs behind the scenes. It reads the specs and writes
the R code that builds them, the **DV suite**, which you can read, check, edit
after feedback, and run.

## Set up (once, and after pulling new code)

1. Open `autospec.Rproj` in RStudio.
2. Restart R: **Session > Restart R**.
3. Run `source("setup_autospec.R")`.

The script says what it is doing at each step. If a step fails, it tells you
what to do; fix that and run it again.

## Settings for the round

`autospec_config.yaml` holds the round number and every file path: the spec
folder, the data file, and where outputs and the DV suite go. Anywhere it says
`{round}`, the round number is filled in. For a new round, change `round` and
`data_file`, and check the rest. `read_autospec_config()` reports what it finds.

Variable names are matched ignoring case, so the spec and the data can spell
them differently. The suite spells every name the way the data does; new DVs
follow the data's style unless `name_case` says otherwise.

## From specs to DVs

Work through `run_pipeline.Rmd` from top to bottom; every chunk is safe to run
again. [HOW_TO_RUN.md](HOW_TO_RUN.md) walks through it step by step, including
settling duplicate DVs, writing tests and running it again after the specs
change. The main steps are also in RStudio's **Addins** menu under *autospec*.

When unsure what to do next, run `autospec_status(config)`: it shows each
topic's progress and names the next step. In short:

| Step | Code | You |
|---|---|---|
| 1. Update the plan | `update_dv_plan()` | Settle DVs defined twice (`QA_duplicate_dvs.csv`, or `sheet_priority` in the config) |
| 2. Try it out, any time | `trial_run()` | Look at the output; nothing needs signing off |
| 3. Review the plan | | Open `dv_plan.csv`, check each row, and sign it off: set `status` to `reviewed` and fill in `reviewed_by` and `reviewed_on` |
| 4. Sync the suite | `sync_dv_suite()` | After each batch of sign-offs. Write the steps still at `derive = NULL` |
| 5. Check the suite | `check_dv_suite()`, `test_dv_suite()` | Fix anything reported |
| 6. Build the DVs | `run_dv_suite()` | |
| 7. Export as plain R | `export_dv_scripts()` | Commit the scripts repo; its users fill in `dv_config.xlsx` and run `run_dvs.R` |

To work on some specs only, list them under `topics` in the config
(`show_topics()` lists the names); the trial run, sync and build then do only
those.

## The DV suite

`dv_suite/` holds one file per spec (`financial_wealth.R`, `income.R`, ...),
plus `settings.R` for the choices that apply to every DV, such as the household
identifier and the pension discount rate. Each DV is one step:

```r
steps$DVCISAvR9 <- dv_step(
  label = "Value of cash ISAs",
  inputs = "FCISAvR9_i",
  derive = function(df) {
    dv_copy(
      df,
      source_col = "FCISAvR9_i",
      new_col = "DVCISAvR9"
    )
  }
)
```

`derive` calls one of the 14 functions in the verb library (`dv_copy()`,
`dv_row_total()`, `dv_flag_if()`, ...; see `show_verbs()`), or holds code
written by hand. If you change what a step reads, update its `inputs`: they
decide the order steps run in.

Run all of it, or part of it:

```r
was_data <- run_dv_suite(was_data, "dv_suite")
was_data <- run_dv_suite(was_data, "dv_suite", topics = "property_wealth")
was_data <- run_dv_suite(was_data, "dv_suite", dvs = "DVHValueR9")
```

Every DV built gets its label from the spec, and a step that fails is reported
without stopping the rest.
