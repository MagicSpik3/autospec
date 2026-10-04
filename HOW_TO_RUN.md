# How to run autospec

A step-by-step guide to going from the DV specs to a dataset with DVs in it.
The code is split at the human plan review: use `run_pipeline.Rmd` to draft the
plan, review it, then continue in `run_pipeline_after_review.Rmd`. Chunk names
are given in brackets, like (`plan`). The same steps are in RStudio's
**Addins** menu under *autospec*.

This is for the DV team. The team that applies the DVs never sees any of it:
they get plain R scripts (step 8) in their own repo, with an Excel config and
a short script that adds the DVs to their data.

For a self-contained management walkthrough, knit
[`demos/hello_world.Rmd`](demos/hello_world.Rmd).

## The idea

1. **Plan**: a spreadsheet matching each DV in the specs to a ready-made
   function, which you check and sign off.
2. **Suite**: R code, one file per spec and one step per DV, that you can read
   and edit. Signed-off rows become working steps.
3. **Run**: the suite adds the DVs to the data.
4. **Export**: the finished DVs are written out as plain R scripts for the
   team that applies them, with nothing of this tool in them.

Every chunk is safe to run again. Nothing you edit by hand (sign-offs in the
plan, code in the suite) is ever overwritten.

**Not sure what to do next?** Run the `status` chunk, or *Addins > autospec:
Where am I?*. It shows each topic's progress and names the next step.

## Before you start

- **Set up** (once, and after pulling new code): open `autospec.Rproj`, restart
  R, run `source("setup_autospec.R")`.
- **Config**: in `autospec_config.yaml`, check `round`, `data_file` and
  `spec_folder`. Everything else follows from those.
- **Topics** (optional): to work on some specs only, list them under `topics`
  in the config. `show_topics(config)` lists the names, such as
  `property_wealth`. The trial run, sync and build then do only those.

## Steps

### 1. Update the plan (`plan`)

Reads the specs and writes `dv_plan.csv` to the output folder. Run it the first
time, and again whenever the specs change (see [below](#when-the-specs-change)).
Anything the parser could not read is listed; that is almost always a typo in
the spec, so raise it with the spec owner.

Each row has a `status`:

| Status | Meaning | What you do |
|---|---|---|
| `auto` | A clear match to a function | Check it and sign it off |
| `needs_review` | A match, but something needs a decision (see `notes`) | Check it carefully and sign it off |
| `hand_written` | No function fits | Sign it off once you agree; you write the code in step 4 |
| `not_a_derivation` | Value labels, or a DV built elsewhere | Nothing (see [below](#rows-that-are-not-derivations)) |
| `duplicate` | The same DV defined again on another row; `notes` says which row it is built from | Nothing |

#### DVs defined more than once

Some DVs appear on more than one sheet. When every copy has the same spec
text, one row is left to sign off and the rest are marked `duplicate`
automatically. When the text differs, the plan cannot guess which is right:
those DVs are listed side by side in `QA_duplicate_dvs.csv`. Two ways to settle
them:

- **A whole sheet at once**: list the sheet to prefer under `sheet_priority`
  in the config, then run the `plan` chunk again. Every DV on that sheet and
  another is built from the preferred sheet's row, and the others are marked
  `duplicate`. List several sheets, most preferred first.
- **One at a time**: in the plan, sign off the right row and set the others'
  status to `duplicate`.

Duplicates never stop a trial run; only the real suite needs them settled.

### 2. Trial run, any time (`trial`)

Tries every drafted DV on the data **without signing anything off**. It
writes a throwaway suite in the output folder, replacing the last trial, and
never touches the plan or the real suite. Use it to see output early, or to
check a topic after changing the plan.

```r
trial <- trial_run(was_data, config, topics = "property_wealth")
```

Before running, it lists any inputs missing from the data. If many of them end
in `_i` and the data has them without it, the data file is probably not the
imputed master.

**Don't mark rows `reviewed` just to get a run.** A sign-off is the record that
someone checked the row. Reading the plan warns about rows marked reviewed with
no `reviewed_by`, and about rows marked reviewed whose notes say they are not a
derivation, which is what marking every row at once does.

### 3. Review the plan

Open `dv_plan.csv` in Excel. For each row, compare `verb`, `args` and
`condition` with `instructions`, and read `notes`. When it is right, set
`status` to `reviewed` and fill in `reviewed_by` and `reviewed_on`.

Don't edit `dv`, `sheet_name` or `instructions`: they are how your sign-offs
are kept when the plan is updated. Corrections to `verb`, `args` or `condition`
are kept only once the row is signed off.

You don't need to finish the review before going on. Sign off a batch, sync
(step 4), and repeat.

#### DVs that must hold -9 where they are missing

For a DV whose missing values must be written as -9, put `-9` in its
`missing_code` column. While that DV is worked out, -8 and -9 in its inputs
count as missing (`NA`); the inputs are then put back as they were, and every
missing value in the DV, whatever caused it, is written as -9. The step's own
`sentinels` setting then meets no -8 or -9, so choose `na_as_zero` in its
arguments to decide whether a missing input makes a total missing.

The step in the suite shows it as `missing_code = -9`, and a hand-written step
can have it too. The value is kept when the plan is updated, even if the spec
text changes. Sync does not change a step that is already written: if you set
`missing_code` after that, sync lists the step, and you add or remove
`missing_code = -9` in its `dv_step()` yourself. The export writes the same
handling into the plain R and the SPSS, and the run report says how many values
each such DV had written as -9.

### 4. Sync the suite with the plan (`sync`)

The first time, this writes the suite folder: one `.R` file per spec, plus
`settings.R` and a `tests` folder. After that, each sync:

- fills in every step still at `derive = NULL` whose row is now signed off;
- adds a step for any DV new to the plan;
- never touches a step that has code in it, whether written by hand or by an
  earlier sync;
- lists steps the plan no longer builds, without deleting them;
- saves a copy of each file it changes in the suite's `_backup` folder.

Steps still at `derive = NULL` after a sync are either not signed off yet, or
have no function that fits: write those by hand. Replace `derive = NULL` with a
function that adds the DV, copying variable names from the step's `inputs` so
the spelling matches the data. To try one step on the data:

```r
run_dv_step(was_data, read_dv_suite(config$suite_folder), "dv_name")
```

Sync also notes which DVs are already in the data. If something earlier builds
one with nothing written (imputation, or code written before), mark it
`not_a_derivation` in the plan instead of writing it again.

### 5. Write the tests

Every step with nothing written gets a test in the `tests` folder. A test gives
the step some made-up rows and says what the spec says the answer should be.
Once you have written the step, fill in two things, then delete the `skip()`
line:

- `given`: input values, one row per situation.
- `expected`: the DV value for each row, **worked out by hand from the spec**,
  not by running your code.

Example, for a spec that says *start at 0; if the amount is over 0, repay 10%
of it; if it is over 0 and under 5, repay 5*:

```r
test_that("dcsc1r9 follows the spec", {
  given <- data.frame(
    dcscamos1r9_i = c(1000,  30,   5,  4.99,  0,  -8,  -9,  NA)
  )
  expected <-      c( 100,   3, 0.5,  5,     0,   0,   0,   0)

  result <- run_dv_step(given, suite, "dcsc1r9")

  expect_equal(result[["dcsc1r9"]], expected, ignore_attr = TRUE)
})
```

To choose rows, include:

- one row for each IF and ELSE in the spec;
- a row on each side of every cut-off (`5` and `4.99` above);
- rows for `0`, `-8`, `-9` and `NA`.

If writing down the expected value gives an answer that looks wrong, check with
the spec owner before going further.

### 6. Check and test (`check`)

`check_dv_suite()` finds mistakes in the code without touching the data.
`test_dv_suite()` runs your tests. Run both after every edit.

### 7. Build the DVs and save (`build`, `save`)

First set the pension discount rate in `settings.R`. Then run the suite. It
builds the DVs in the right order and writes a report listing each DV as built,
not written yet, skipped (and why) or failed (and why). A failed step doesn't
stop the rest.

### 8. Export as plain R (`export`)

Set `scripts_folder` in the config to your checkout of the scripts repo
(`was-dvs`), then run the `export` chunk. It checks the suite and runs the
tests, then runs the plain R on the data and compares every DV with the suite.
Nothing is exported if any of that fails. Then commit and push the scripts
repo; that is all the other team sees.

What goes in the repo:

| File | What it is |
|---|---|
| `dvs/<topic>.R` | One file per topic, one block per DV: the spec it came from, the columns it needs, and plain base R that builds it. |
| `run_dvs.R` | Reads the data (`.sav`, `.csv` or `.rds`), runs the chosen files in the chosen order, prints what each added, saves as `.rds` and `.sav` with a report. |
| `dv_config.xlsx` | Where they give their data file, where to save, which topic files to run, and the **Order** to run them in. |

The **Topics** sheet lists each file with the files it uses DVs from. Its
**Order** is filled in so every file runs after those; users can renumber it,
and their numbers are kept when you export again. `run_dvs.R` checks the order
before it starts and stops, saying what to change, if a file would run before
one it needs. The export warns about this too.
| `README.md` | How to run it, and which DVs are not yet built. |

They need R with `haven` and `readxl`, not autospec. Each block is
self-contained, so they can edit one DV without touching another; to rerun one
DV they run its block.

Things to know:

- **Every verb has a plain-R version**, and the export proves them equal on
  the data. A hand-written step is copied as it is; one that mixes a verb with
  other code is refused, so write such steps as one verb call or in plain R.
- **A DV whose input is missing from the data** is exported unchecked, with a
  warning naming it. Export from complete data.
- **Settings are inlined** (`c(-8, -9)`, the household id, the discount rate),
  so a convention change means exporting again, not editing their scripts.
- Export again after any fix. `dv_config.xlsx` keeps what is already in it.

## Running it again

| What changed | What to do |
|---|---|
| **New data, same specs** | Load the data and build (step 7). Nothing else. |
| **You signed off more rows** | Sync (step 4). The new sign-offs become working steps. |
| **You fixed a step** after feedback | Check, test, then run just that DV: `run_dv_suite(was_data, config$suite_folder, dvs = "dv_name")`. Export again (step 8) and push the scripts repo so the other team gets the fix. |
| **The specs changed** | See below. |
| **A new round** | Change `round` and `data_file` in the config and put the new specs in the spec folder. Outputs and the suite go to new folders for that round, so start again from step 1. |

### When the specs change

1. **Update the plan** (`plan`). Rows whose spec text hasn't changed keep their
   sign-off. Rows whose text has changed lose it, and `notes` says "spec text
   changed since it was reviewed by …"; the chunk lists them. The old plan is
   saved in `plan_backups` in the output folder.
2. **Review** the rows that lost their sign-off, and any new ones.
3. **Sync** (`sync`). New DVs get steps and new sign-offs fill in their stubs.
   A step that already has code is not rewritten, so for a DV whose spec
   changed, compare the step with the new spec text in the plan and edit it by
   hand.

## Data spelt in another case

The suite spells each variable the way the data it was written for did. Data
that spells them in another case, such as mixed-case Round 8 data against a
suite written for lower-case Round 9 data, runs anyway: for the run, those
columns take the suite's spelling, and afterwards they get the data's spelling
back. A DV already in the data is replaced under the data's spelling, not added
as a second column. The exported scripts do the same, so the other team's data
can be in any case.

Two things still stop a run: data with two columns whose names differ only in
case, and a suite that spells one variable two ways, such as `a` in one step's
inputs and `A` in another's. `check_dv_suite()` reports the second.

## Rows that are not derivations

Some rows in the specs don't describe a DV to build. The plan marks these
`not_a_derivation` and the suite leaves them out. Each suite file lists them in
the comment at the top, so nothing disappears silently. There are two kinds:

- **Value labels.** The input variable blocks (for example on
  `R9_Work_and_Pay`) have a "derivation" column that holds the variable's value
  labels, like `1.0: 'One week', 2.0: 'Two weeks'`. They describe codes in the
  data and give nothing to build.
- **Built somewhere else.** Rows saying "Imputed from derived …",
  "Derived in the Financial Wealth DV Spec" or "Annual amounts are derived from
  imputed banded variable …". The DV comes from imputation or from another
  spec's step.

If a row has been marked wrongly, change its `status` in the plan to
`needs_review` or `hand_written`. It will then get a step in the suite.
