# Autospec Agent Handoff

Updated: 2026-10-04

## Product context

The package was renamed from `wealthdv` to `autospec`. Preserve the user's in-progress work; do not reset or clean generated/user files unless asked. The requested workflow is staged: complete one stage, run its full end-to-end tests, show the user the result, and wait for confirmation before moving to the next feature stage.

## Completed stages

### Stable requirement UIDs

- `specs/TEST_ALL_VERBS.csv` has source-authored IDs `REQ-HW-001` through `REQ-HW-014` in `Requirement UID` columns next to the derivation blocks.
- `find_derivation_blocks()` recognizes optional `Requirement UID` or `UID` columns after the derivation-block notes.
- Catalogue and plan preserve explicit IDs; numbered outputs get deterministic suffix IDs; legacy specs without explicit IDs retain generated IDs.
- Duplicate IDs are rejected.
- `demos/hello_world.Rmd` shows the UID path. `tests/testthat/test-e2e_all_verbs.R` checks UID identity survives catalogue row reversal.

### Readiness and totals

- `check_dv_readiness(df, suite, dvs, trusted_inputs)` returns readiness, missing inputs and producer UIDs waiting for successful runs.
- `run_dv_suite(..., require_ready = TRUE, trusted_inputs = character())` is an opt-in gate. Default behavior remains backward-compatible.
- Successful and failed per-UID outcomes accumulate in the dataframe attribute `dv_suite_evidence`; latest evidence replaces older outcome for that UID.
- Optional `evidence_file` + required `evidence_id` persist evidence across CSV/dataframe save-reload boundaries. IDs scope one immutable dataset snapshot; mismatched IDs are ignored.
- Persisted evidence includes a per-step MD5 signature covering UID, DV, inputs, derive code, missing-code handling, and suite settings. Changed implementation/settings do not reuse prior success.
- In persistent mode with `stop_on_error = TRUE`, a failed outcome is written before the run throws, invalidating earlier success for that UID.
- Existing derived columns are not trusted by presence alone; `trusted_inputs` is the explicit override.
- `make_required_input_table()` reports raw data presence, producer DVs and producer UIDs.
- `specs/totals.csv` intentionally lists F before E and D. `test_data/totals_input.csv` supplies A/B/C. The end-to-end test verifies ordinary topological execution E,D,F, blocks F before producer evidence, and runs D then E in separate gated calls before F, including dataframe CSV reload between stages.
- `demos/hello_world.Rmd` includes all-verbs UID provenance and the staged totals workflow with persistent evidence. Its rendered `demos/hello_world.html` may be regenerated while validating.

## Verification

- Latest focused persistence tests: `Rscript -e "devtools::test(filter = 'e2e_totals|dv_readiness')"` -> 48 passed, 0 failed.
- Full package suite after Stage 3: `Rscript -e "devtools::test()"` -> 1012 passed, 0 failed, 3 skipped (`was.utils` absent).
- The hello-world Rmd rendered successfully with persistent CSV evidence across save/reload.
- `devtools::document()` completes but emits existing unresolved-link warnings for internal topics such as `get_spec`, `find_derivation_blocks`, `join_continued_items`, `tokenise_derivation`, `find_number_ranges`, and `parse_derivation`.

## Current Stage

Stage 3 (durable evidence) is implemented and verified. The ledger is CSV with `evidence_id`, requirement UID, DV, outcome, and suite signature. It uses direct `data.table::fwrite()` writes; atomic replacement is not yet implemented. The user must confirm Stage 3 before any further stage is started. After confirmation, ask which next scope they want; likely candidates are ledger write atomicity, richer evidence provenance, or production workflow integration.
