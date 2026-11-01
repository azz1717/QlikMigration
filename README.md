# Qlik app migration tool

Takes the apps in one Qlik Cloud space, tidies each load script to the house
style, points every on-prem QVD load at its cloud view, and uploads the
result. Base R only (R 4.3.1, no packages). How to run it: `RUNBOOK.md`.

## Run it
- **Once per machine:** copy `qlik_cli_path.txt.example` to
  `qlik_cli_path.txt` and put the path to qlik.exe on its one line. Sign
  qlik-cli in (`qlik context use <name>`). First time on a tenant:
  `Rscript fleet/fleet.R doctor --space <id>` (read-only check).
- **Migrate: double-click `RUN_MIGRATION.bat`.** Pick the space, the apps,
  where they go; type YES. Live by default - this writes to the cloud.
- **Reports: double-click `RUN_REPORTS.bat`.** Pick the space and apps;
  each gets a `report.html`. Nothing in the cloud changes.

## What each file does, in run order
**Start and control** (`fleet/` - also where each machine's run data lives)
- `RUN_MIGRATION.bat` / `RUN_REPORTS.bat` - find R, open the screens.
- `fleet/console_ui.R` - the screens: asks the questions, runs one fleet.R
  command per step, shows one results screen. Holds no logic.
- `fleet/fleet.R` - the conductor. Every step is a command here (`fetch`,
  `process`, `upload`, `verify`, `report`, ...), so it also runs without
  the screens. Section headers say which step you are in.
- `fleet/qlik_api.R` - the only file that talks to Qlik Cloud; every call is
  logged to `fleet/audit.log`.
- `fleet/flags.csv` - features the reports flag (GeoAnalytics, Inphinity,
  REST, ...). Add a row to add a flag.

**Step 1 - Download** (fleet.R `spaces`, `apps`, `add`, `fetch`): each app is
unbuilt to `fleet/apps/<app name>/`. A failed download fails that app.

**Step 2 - Styling** (`2_styling/`, run by fleet.R `process`)
- `run_styling.R` - runs the eight passes below, in this order.
- `01_explicit_aliases.R` - every field gets an explicit `AS` name.
- `02_bracket_references.R` - field names go in `[brackets]`.
- `03_leading_commas.R` - commas move to the start of the line.
- `04_spacing.R` - consistent spaces around commas, operators, brackets.
- `05_keyword_case.R` - Qlik keywords and functions in UPPER case.
- `06_vertical_layout.R` - indentation and blank lines.
- `07_alias_alignment.R` - lines up the `AS` column.
- `08_commented_code.R` - applies passes 1-7 to commented-out code too.

**Step 3 - Retargeting** (`3_retargeting/`, same `process` command)
- `retarget_loads.R` - rewrites each on-prem QVD load to its cloud view;
  what it cannot map goes in `dev_notes.txt` and the app still uploads.
  Finishes by re-aligning the `AS` column with styling pass 7.
- `retarget_shared.R` - its small helpers.
- `qvd_field_map.csv` - THE MAP: on-prem QVD + field -> cloud view + field.
  Generated and frozen; never hand-edit.

**Step 4 - Upload** (fleet.R `upload`, `verify`, `results`): overwrite the
originals or upload copies named `<app> [mig]` into another space, download
each again to confirm the script, then print the results screen.

**Reports** (`reports/`, fleet.R `report`)
- `render_report.R` - builds `report.html` (review + tech debt).
- `usage_report.R` - `usage-*.csv`: unused tables, fields, variables.
- `app_usage.R`, `script_loads.R`, `script_refs.R`, `script_debt.R`,
  `json_strings.R` - the readers behind those two.

**Shared building blocks** (`shared/`)
- `qlik_tokenizer.R` - script <-> tokens (styling never edits raw text).
- `qlik_reserved_words.R` - the Qlik keyword and function list.
- `comment_substream.R` - commented-out code as tokens (pass 8, retargeting).
- `csv_read.R`, `json_read.R` - safe readers. `find_rscript.bat` - finds R.

## Where results land
`fleet/apps/<app name>/` - script_styled.qvs, script_retargeted.qvs,
retarget_report.csv, dev_notes.txt, changes/, log.txt, upload.log,
script_unlocked.qvs (Data Manager apps only),
report.html. `fleet/master.csv` - every app and its stage;
`fleet/master_loads.csv` - every load that still needs a hand.
Run data stays on the machine (not in git).

Everything removed for the handover (tests, offline mock, map-building tools
and lineage data, diagnostics, sample app) is in git tag `pre-restructure`.
