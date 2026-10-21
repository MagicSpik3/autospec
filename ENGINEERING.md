# Engineering: autospec architecture

Last reviewed: 2026-10-19

This is the reference for the overall direction of autospec. Read it before any
design change. When the architecture changes, update this file in the same change.
The working rules for agents are in [CLAUDE.md](CLAUDE.md).

## 1. What autospec does

Input: spec workbooks (CSV or XLSX) that hold pseudo-code for derived variables
(DVs).

Output:

1. A **plan** (`dv_plan.csv`): the audit. One row per DV requirement, with its
   UID, its sign-off, whether its function is written, and the state of its unit
   tests.
2. A **suite**: one R function per DV, plus a unit-test stub for each.
3. An **export**: a standalone R project with the finished DV functions and their
   unit tests, for delivery to the customer.

Autospec does not run the survey pipeline. It writes the code and proves that the
code works on test data.

## 2. The flow

```
spec CSV/XLSX
   │  make_catalogue()            parse the pseudo-code into derivation rows
   ▼
catalogue (QA_derivation_catalogue.csv)
   │  build_dv_plan()             match each row to a verb, draft a plan row
   ▼
dv_plan.csv  ◄── sign-off by a person; tracking columns
   │  sync_dv_suite() / write_dv_suite()
   ▼
suite folder (dv_suite/R9)       settings.R, one file per topic, test files
   │  check_dv_suite(), run_dv_suite(), trial_run()
   ▼
evidence ledger + QA reports
   │  export_dv_scripts()
   ▼
scripts folder (standalone R project, DV functions and tests)
```

`autospec_status()` reads the plan and the suite and reports where the round has
got to. It changes nothing.

## 3. Artifacts and their owners

| Artifact | Written by | Read by | Meaning |
| --- | --- | --- | --- |
| Spec workbooks (CSV/XLSX) | Survey team | `make_catalogue()` | Source of truth for what each DV is. |
| `QA_derivation_catalogue.csv` | `update_dv_plan()` | `build_dv_plan()` | Parsed spec rows. Regenerated freely. |
| `dv_plan.csv` | `update_dv_plan()`, then people | sync, status, export | The audit. People sign off rows and set tracking values here. |
| `plan_backups/` | `update_dv_plan()` | people | Copy of the plan before each update. |
| `dv_suite/R9/` | `write_dv_suite()`, `sync_dv_suite()`, people | check, run, export | The DV functions and tests. Hand-written steps live here. |
| Evidence ledger (CSV) | `run_dv_suite()` | readiness gate | Per-UID outcome, tied to a suite signature. |
| `QA_spec_issues.csv` | `update_dv_plan()` | the spec owner | One row per defect in the spec. Removed when there are none. |
| `QA_*.csv` reports | the steps above | people | Human-readable records. |
| Scripts folder | `export_dv_scripts()` | the customer | The delivered product. |

Rule: each artifact has one writer. Generated files may be overwritten. Files with
people's sign-offs or hand-written code are backed up before any change.

## 4. Identity and the plan as audit

- Every DV requirement has a **UID**. Explicit UIDs in the spec (`REQ-HW-001`) are
  kept; generated ones (`dv_000001`) are stable within a spec version. UIDs are
  never reused.
- The plan is keyed by DV name and sheet. Sign-offs and tracking values are carried
  over by that key on each update, and survive changes to the spec text. A spec
  change is reported, and a sign-off whose text changed is dropped and reported.
- The plan must have the required columns and only the recognised statuses
  (`plan_statuses()`). Tracking columns are optional on read so older plans still
  load.

### 4.0 Spec issues and review notes

The plan keeps two kinds of message apart, because different people act on
them (`R/spec_issues.R`):

- **`spec_issues`**: defects in the spec itself, for the spec owner. Each is
  written `[code] message`. Omissions: `no_else`, `missing_value`,
  `no_derivation`, `no_dv_name`, `branch_without_name`. Errors:
  `unknown_variable`, `duplicate_dv`, `unparsed`. Repaired typos: `typo`.
- **`notes`**: prompts for the reviewer about how the spec was drafted, such as
  "confirm the drafted condition".

Rules:

- A DV is a column, so every row needs a value. An `IF` with no `ELSE`, before
  the DV has any value, is a `no_else` issue whatever verb fits
  (`missing_else()`). The DV takes `default_else` from the config where no
  branch applies: 0 (the default, as the published DVs are), -9 (the legacy
  code for missing) or NA (`R/default_else.R`). The plan's `implied_else`
  column records the value on each row where it was used: a row no verb fits,
  for whoever writes it, or a row whose verb takes `otherwise`. A verb that
  cannot take it, such as `dv_annualise`, is flagged but has nothing implied.
  The suite shows it above the step as `# Implied ELSE:`, which the export
  keeps; `# Spec issues:` is dropped from the export, like `# Plan notes:`.
- A plain unknown variable is an issue only when the data's names were given;
  without them, an input may still be in the data. A near miss of a DV name
  ("did you mean ...") is always an issue.
- A row with a spec issue is never `auto`. Spec issues do not block sign-off or
  export; the reviewer's decision is recorded in `args` as usual.
- `spec_issue_patterns()` maps each drafting message to its code. A new defect
  message needs an entry there, and a line in `test-spec_issues.R`.

### 4.1 DV states

A DV moves through these states. The plan's tracking columns record them.

| Column | Yes means | Needs |
| --- | --- | --- |
| `DV Written` | The suite step has a function body (`derive` is not `NULL`). | Nothing. |
| `Unit Test Stub` | A test for the DV exists in the suite's `tests/` folder. | Nothing. `write_dv_suite()` writes the stub before the DV is written. |
| `Unit Test Populated` | That test's `skip()` line has been removed. | `Unit Test Stub` |
| `Unit Test Passed` | The test ran without a failure or an error. | `Unit Test Populated` and `DV Written` |

Values are `yes`, `no` or blank, in any case. Blank means not yet. The rules are
in `tracking_needs()` and checked by `tracking_problems()`.

**Where the values come from.** `update_dv_tracking(config)` reads the suite
(`suite_tracking()`), runs its tests (`unit_test_results()`), and records both in
the plan (`record_tracking()`), backing up the old plan first. Values typed by
hand are replaced. Plan rows with no suite step are left blank.

**How a test is tied to its DV.** By the test name: `Requirement UID <uid>: ...`
first, then `<dv> follows the spec`. These are the names `write_dv_suite()` writes.
A test with any other name belongs to no DV.

## 5. Modules and their layers

Keep each layer free of the layers above it. Pure logic does not read or write
files.

| Layer | Modules | Responsibility |
| --- | --- | --- |
| Parsing | `get_spec`, `make_catalogue`, `parse_derivation`, `tokenise_derivation`, `match_verb`, `derivation_tree`, `find_derivation_blocks` | Turn spec text into structured derivations. |
| Verbs | `dv_*.R`, `verb_library`, `verb_checks` | The fixed set of DV operations, each a pure function over a data frame. |
| Code generation | `plain_code`, `spss_code`, `write_dv_suite` | Turn a plan row into R code, with SPSS shown as comments. |
| Plan | `build_dv_plan`, `update_dv_plan`, `spec_issues`, `dv_tracking`, `update_dv_tracking` | Draft the plan, flag spec issues, keep sign-offs, check tracking values, and record them from the suite. |
| Suite | `sync_dv_suite`, `dv_suite`, `check_dv_suite`, `trial_run`, `name_case`, `resolve_names`, `suite_tracking` | Write, check, and run the suite; keep the evidence ledger; read each DV's state and test results. |
| Export | `export_dv_scripts`, `export_selection` | Choose what to deliver and build the delivered project from the suite. |
| Status and UI | `autospec_status`, `addins`, `autospec_config`, `setup_autospec` | Reports and entry points. Read-only except for explicit actions. |

Config (`autospec_config.yaml`) is read once by `read_autospec_config()` and passed
down as an object. Modules do not read the file themselves.

## 6. Export: current behaviour and target

### Current

`export_dv_scripts()` checks the suite, runs its example tests, builds the scripts
on the data, and compares each DV with the suite on that data. Any mismatch stops
the export. `select_export_steps()` decides what is exported.

- **Default:** every step with a function body is exported, as before.
- **`finished_only = TRUE`** (needs `plan`): only finished DVs are exported. A DV
  is finished when its plan row has `DV Written`, `Unit Test Stub`, `Unit Test
  Populated` and `Unit Test Passed` all `yes` (`finished_dvs()`).
  - A finished DV that reads a DV left out is left out too, since delivering it
    alone would break.
  - Check errors and failing tests of DVs left out do not stop the export.
    Suite-wide errors, such as in `settings.R`, still do.
  - With no finished DVs, it stops and writes nothing.
- **Manifest:** every export writes `left_out.csv` (`dv`, `uid`, `reason`). The
  reason is the first gap found, in the order the work is done: not written; not
  in the plan; no unit test; unit test not populated; unit test not passed; needs
  a DV that is left out; an input is missing from the data.

The README still lists only the DVs not written. `left_out.csv` is the full list.

### Still to do

- **Unit tests in the project:** each exported DV carries its unit test in the
  delivered project, so the customer can rerun it (stage 6).
- **Rerunnable:** exporting again with the same plan gives the same output.

## 7. Decisions

### Taken (2026-10-19)

| | Decision | Taken | Built in |
| --- | --- | --- | --- |
| A | Can a test stub exist before its DV is written? | Yes. See the chain in section 4.1. | `tracking_needs()` |
| B | Meaning of `DV Written`. | The step has a function body (`derive` is not `NULL`). The data check stays in the export. | `suite_tracking()` |
| C | Source of tracking values. | The suite and its test runs set them; the plan records them. Values typed by hand are replaced on the next update. | `update_dv_tracking()` |
| E | How a test is tied to its DV. | By the test name: `Requirement UID <uid>: ...` first, then `<dv> follows the spec`. | `test_owners()` |
| F | When is a test populated? | When its `skip()` line has been removed. | `suite_tracking()` |

### Open

Each has a recommendation.

| | Decision | Recommendation |
| --- | --- | --- |
| D | One audit or two. | Two, with separate jobs. The plan records development state (code and unit tests) per UID. The evidence ledger records run outcomes on a data snapshot. |
| G | Should DVs built from a verb get a test stub? Today only hand-written steps do, so verb DVs can never be finished. | Yes: every DV gets a stub (stage 5). |
| H | How the delivered unit tests run. Today's tests call `run_dv_step()` from autospec; the delivered scripts do not use autospec. | Rewrite each test to run the plain-R block, so the customer needs only `testthat` (stage 6). |

## 8. Engineering rules

These apply to every change. They come from [CLAUDE.md](CLAUDE.md), restated here
for the architecture.

- **MVP and YAGNI.** Build what the current stage needs. Do not add options for
  cases nobody has asked about.
- **Test first.** Write a failing test, then the code that passes it. A change is
  not finished without tests, including edge and error cases.
- **Single-purpose modules.** A new concept gets its own module; see the table in
  section 5.
- **Explicit interfaces.** Functions take what they need as arguments. Column names
  and statuses come from one definition (`plan_columns()`, `plan_statuses()`,
  `tracking_columns()`), not repeated strings.
- **Stop on failure.** A failing test, build or check stops the work and is reported
  with its exact output before any fix is attempted.
- **Outward-facing changes need confirmation.** Writing to the scripts folder that
  goes to the customer, or changing a sign-off, is not done without the user's
  say-so.

## 9. Verification

- Full suite: `Rscript -e "devtools::test()"`. The current baseline is 1235
  expectations passed, 0 failed, 3 skipped (the `dv_annualise` tests, which need
  `was.utils`).
- Focused runs: `devtools::test(filter = "dv_tracking")` and so on.
- End-to-end tests (`tests/testthat/test-e2e_*.R`) cover the path from spec to
  suite on small fixtures. Keep them passing when the flow changes.

## 10. Roadmap

Each stage is test-first. Its tests are written, run red, and agreed before the
code. Finish and verify one stage before starting the next.

The shared fixture is `tests/testthat/helper-tracking_suite.R`. It is one small
suite with a step at each point of the chain: written and passing, failing, stub
only, not written, matched by name, with no test, and passing but reading a
failing DV. `finished_plan()` is the matching plan.

| Stage | Goal | New functions | Tests | State |
| --- | --- | --- | --- | --- |
| 1 | Read each DV's state from the suite files, without running anything. | `suite_tracking(suite_dir)`: `uid`, `dv`, `DV Written`, `Unit Test Stub`, `Unit Test Populated`. | `test-suite_tracking.R`, first 8 tests | Done. |
| 2 | Run the tests and record each DV's result. | `unit_test_results(suite_dir)`: `uid`, `dv`, `Unit Test Passed` (`yes`, `no`, or blank when skipped or untested). | `test-suite_tracking.R`, last 3 tests | Done. |
| 3 | Record stages 1 and 2 in the plan. | `record_tracking(plan, tracking)` (pure); `update_dv_tracking(config)` (reads, runs, writes the plan with a backup; exported). | `test-record_dv_tracking.R`, 7 tests; `test-dv_tracking.R` for the rule change | Done. |
| 4 | Deliver finished DVs only, with a manifest. | `finished_dvs(plan)`, `tracking_gap(plan)`, `select_export_steps()`; `export_dv_scripts(..., plan = NULL, finished_only = FALSE)`; `left_out.csv`. | `test-export_finished_only.R`, 8 tests | Done. |
| 4b | Keep spec defects apart from review notes, and list them for the spec owner. | `spec_issue_codes()`, `missing_else()`, `add_spec_issues()`, `spec_issue_table()`; plan column `spec_issues`; `QA_spec_issues.csv`; status count. | `test-spec_issues.R`, 16 tests; additions in `test-update_dv_plan.R`, `test-autospec_status.R` | Done. |
| 4c | A configurable value for a missing ELSE, recorded where used. | `default_else` in the config; `parse_default_else()`, `format_else_value()`; `match_verb(..., default_else)`, `build_dv_plan(..., default_else)`; plan column `implied_else`; `# Implied ELSE:` in the suite. | `test-default_else.R`, 14 tests; additions in `test-autospec_config.R`, `test-update_dv_plan.R` | Done. |
| 5 | Give verb DVs a test stub too, so they can be finished. | Change to `write_dv_suite()` / `sync_dv_suite()`. | To write after decision G. | Not started. |
| 6 | Ship each delivered DV's unit test in the scripts project. | Change to `export_dv_scripts()`. | To write after decision H. | Not started. |

After `update_dv_tracking()`, the `autospec_status()` counts reflect the suite,
not hand-typed values.

Next: decision G, then stage 5. Until verb DVs get test stubs, only hand-written
DVs can be finished, so a finished-only export leaves every verb DV out with "no
unit test".
