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

### 4.1 DV states

A DV moves through these states. The plan's tracking columns record them.

| Column | Yes means | Requires |
| --- | --- | --- |
| `DV Written` | The DV function has a body, not only a stub. | Signed-off plan row. |
| `Unit Test Stub` | A unit-test skeleton exists for the DV. | `DV Written` |
| `Unit Test Populated` | The unit test has real `given` and `expected` values. | `Unit Test Stub` |
| `Unit Test Passed` | The unit test has been run and passed. | `Unit Test Populated` |

Blank means not yet. Values are `yes`, `no` or blank, in any case. Each step needs
the step before it (`tracking_problems()`).

Open question: the tracking values are set by hand today. See section 7.

## 5. Modules and their layers

Keep each layer free of the layers above it. Pure logic does not read or write
files.

| Layer | Modules | Responsibility |
| --- | --- | --- |
| Parsing | `get_spec`, `make_catalogue`, `parse_derivation`, `tokenise_derivation`, `match_verb`, `derivation_tree`, `find_derivation_blocks` | Turn spec text into structured derivations. |
| Verbs | `dv_*.R`, `verb_library`, `verb_checks` | The fixed set of DV operations, each a pure function over a data frame. |
| Code generation | `plain_code`, `spss_code`, `write_dv_suite` | Turn a plan row into R code, with SPSS shown as comments. |
| Plan | `build_dv_plan`, `update_dv_plan`, `dv_tracking` | Draft the plan, keep sign-offs, check tracking values. |
| Suite | `sync_dv_suite`, `dv_suite`, `check_dv_suite`, `trial_run`, `name_case`, `resolve_names` | Write, check, and run the suite; keep the evidence ledger. |
| Export | `export_dv_scripts` | Build the delivered project from the suite. |
| Status and UI | `autospec_status`, `addins`, `autospec_config`, `setup_autospec` | Reports and entry points. Read-only except for explicit actions. |

Config (`autospec_config.yaml`) is read once by `read_autospec_config()` and passed
down as an object. Modules do not read the file themselves.

## 6. Export: current behaviour and target

### Current

`export_dv_scripts()` checks the suite, runs its example tests, builds the scripts
on the data, and compares each DV with the suite on that data. Any mismatch stops
the export. It writes every step that has a function body. Steps without one are
listed in the README as not yet written.

It does **not**:
- filter on `Unit Test Passed` or any other tracking value;
- ship each DV's unit tests as part of the delivered project;
- offer a way to deliver only the finished DVs.

### Target

The delivery workflow needs to deliver finished DVs while the rest are still being
debugged.

- **Definition of finished:** a DV is finished when `DV Written`, `Unit Test
  Populated` and `Unit Test Passed` are all `yes`, and it matches the suite on the
  data.
- **Option:** `export_dv_scripts(..., finished_only = TRUE)` exports only finished
  DVs. The default is to export everything written, as now.
- **Manifest:** every export writes a list of the DVs it left out and why, so the
  customer can see what is missing.
- **Unit tests in the project:** each exported DV carries its unit test in the
  delivered project, so the customer can rerun it.
- **Rerunnable:** exporting again with the same plan gives the same output.

## 7. Open decisions

These need your decision before the next build stage.

1. **Meaning of `DV Written`.** Is it "function body written", as the table in 4.1
   says, or "written and matches the suite"? The second is stricter.
2. **Source of tracking values.** Today they are typed in the plan. Should the
   suite and test runs set `Unit Test Stub`, `Unit Test Populated` and `Unit Test
   Passed` automatically, with the plan as a record of those results?
3. **One audit or two.** The plan and the evidence ledger both record outcomes.
   Choose one as the source of truth, or define what each one records.
4. **Finished-only definition.** Should "finished" also require the DV's check
   against the suite on the data, as the definition in section 6 does?
5. **Unit test runner.** Are the per-DV tests run with `testthat` in the scripts
   project, and should the export fail when one fails?

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

- Full suite: `Rscript -e "devtools::test()"`. The current baseline is 1079 passed,
  0 failed, 3 skipped (the `dv_annualise` tests, which need `was.utils`).
- Focused runs: `devtools::test(filter = "dv_tracking")` and so on.
- End-to-end tests (`tests/testthat/test-e2e_*.R`) cover the path from spec to
  suite on small fixtures. Keep them passing when the flow changes.
