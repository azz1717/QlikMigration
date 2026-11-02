# Qlik app migration

Takes the apps in one Qlik Cloud space, restyles each app's load script,
points its on-prem QVD loads at the Qlik Cloud views, and puts the script
back in the cloud. Run once. Base R only (R 4.3.1), talks to the cloud
only through qlik-cli.

## Setup (once per machine)
- `rscript_path.txt` - one line: the full path to Rscript.exe.
- `qlik_cli_path.txt` - one line: the full path to qlik.exe.
- qlik-cli signed in to the tenant (`qlik context use <name>`).

## Run
Double-click `RUN_MIGRATION.bat`. It asks:
1. Which space are the apps in?
2. Which apps? (`all`, or numbers like `1,3-5`)
3. Where should they go? Back into the same space (overwrites the
   originals), or into another space as copies named `<app> [mig]`.
4. Type YES to start.

Then, per app: download -> unlock a Data Manager section -> style ->
retarget -> upload (script only, no reload) -> download again to check the
script really landed -> tag the app (GeoAnalytics / Inphinity / NPrinting).
One results screen at the end: `ok`, or `FAILED` with the reason.

## What you get
- `output\<app name>\original.qvs` - the script as downloaded.
- `output\<app name>\migrated.qvs` - the script that was uploaded.
- `output\<app name>\notes.txt` - read this first: where the app went,
  whether the check passed, how many loads were retargeted, the loads
  NOT retargeted (your by-hand list, with line numbers and reasons), and
  every qlik command run with its reply.
- `output\migrated_apps.csv` - one row per app per run: date, app, from,
  to, ok/FAILED, loads retargeted, GeoAnalytics, Inphinity, NPrinting.

## The files, in the order they run
- `RUN_MIGRATION.bat` - reads `rscript_path.txt`, runs `migrate.R`.
- `migrate.R` - the whole run: the questions, then each step above.
- `flags.csv` - what to look for to mark GeoAnalytics, Inphinity Forms
  and NPrinting, and the tag each one gets in the cloud.
- `shared\qlik_tokenizer.R` - splits a script into pieces (words,
  quoted names, comments...) so nothing inside a name or comment is
  ever changed by accident. Everything else works on these pieces.
- `shared\qlik_reserved_words.R` - Qlik's keywords and function names.
- `2_styling\run_styling.R` - runs the eight styling steps in order:
  - `01_explicit_aliases.R` - every field gets an explicit `AS` name.
  - `02_bracket_references.R` - field names in `[brackets]`.
  - `03_leading_commas.R` - commas move to the start of the line.
  - `04_spacing.R` - spaces around commas, operators and brackets.
  - `05_keyword_case.R` - Qlik keywords and functions in CAPITALS.
  - `06_vertical_layout.R` - indentation and blank lines.
  - `07_alias_alignment.R` - lines up the `AS` column with tabs.
  - `08_commented_code.R` - the same style for commented-out code
    (uses `shared\comment_substream.R`).
- `3_retargeting\retarget_loads.R` - rewrites each on-prem QVD load to
  its cloud view, using `qvd_field_map.csv` (the map of QVD fields to
  view columns). `retarget_shared.R` finds the loads.
- `shared\json_read.R`, `shared\csv_read.R` - read qlik's replies and
  the CSV files (base R has no JSON reader).

## What it sends to the cloud
Reads: `space ls`, `app ls`, `app unbuild` (download and check),
`item ls`, `item collections`, `collection ls`.
Writes: `app copy` (copy mode only), `app space update` (only if a copy
landed in the wrong space), `app build --no-reload` (the script),
`collection create` / `collection item create` (the tags).
Nothing else is changed: sheets, charts, measures, variables and data
connections stay as they are. No app is ever reloaded.

## Good to know
- Loads the map can't place are left exactly as written and listed in
  notes.txt. Retargeting takes whatever it can: 5% is still 5% less by hand.
- If retargeting itself breaks on an app, the styled script is uploaded
  instead and the results screen says "styled only".
- Data Manager apps: the locked "auto-generated" section is unlocked so the
  new script sticks. Those tables can no longer be edited in Data Manager.
