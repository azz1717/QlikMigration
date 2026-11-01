# Migration pipeline - how to run it and what comes out

Everything runs from the repo root on a machine that has R and qlik-cli
signed in (`qlik context use <name>` done once).

**Live is the default.** Every cloud-writing step writes to the cloud unless
you add `--dry-run`, which only prints what it would do.
**There is no coverage check.** Every app is retargeted as far as the map
allows and uploaded with whatever that gives.

Command line: every step is `Rscript fleet/fleet.R <verb>`. Use it for
unattended runs and for the verbs the batch screens do not offer (`status`,
`rollup`, `stamp`, `reconcile`, `doctor`).

## The two batch screens

### RUN_MIGRATION.bat - download, format + retarget, upload, check
Double-click it. Three questions, one YES, then one results screen
(this is a real run against the offline mock tenant):

    Qlik app migration
    ------------------------------------------------------------------------
    1. Which space are the apps in?
    [1] Adam - Personal  (personal)
    [2] Curated Data Store  (managed)
    [3] Migration Staging  (shared)
    [4] On Prem Apps  (shared)
    Space number (Q to quit):
    2. Which apps in "On Prem Apps"?
    [1] 01 ESS QVD Builder - CDP
    [2] 05 - Grant Acquittals Report
    [3] Grants QVD Generator
    Apps: all, or numbers like 1,3-5 (Q to quit):
    3. Where should the finished apps go?
    [1] Back into "On Prem Apps" - overwrite the originals
    [2] Into another space - as copies named "<app> [mig]"
    Choice (Q to quit):
    Which space should the copies go to?   (only after [2])
    ------------------------------------------------------------------------
    3 app(s) from "On Prem Apps"
      -> copies in "Migration Staging"
    Download, format + retarget, upload, then check each upload.
    ------------------------------------------------------------------------
    Type YES to start, anything else to stop:

    Running. Every detail goes to fleet/last_run.log
      1/4 Downloading the apps ...
      2/4 Formatting + retargeting ...
      3/4 Uploading ...
      4/4 Checking each upload ...
    ========================================================================
    RESULTS - 3 app(s), copies into "Migration Staging"
    ------------------------------------------------------------------------
    ok      01 ESS QVD Builder - CDP
    ok      05 - Grant Acquittals Report
    ok      Grants QVD Generator
    ------------------------------------------------------------------------
    3 ok, 0 not ok.  Per-app files: fleet/apps/<app name>/
    Full log of this run: fleet/last_run.log
    Each app's qlik replies: fleet/apps/<app name>/upload.log

An app that did not make it is `FAILED` with the reason in qlik's own words,
for example a download that failed:

    FAILED  05 - Grant Acquittals Report
            not sent: the download failed and the folder holds only an
            older copy. fetch: exit 1: Error: ...

An app that went up styled but not retargeted says
`sent the styled script - not fully retargeted`. Typing anything but YES
stops with "Nothing in the cloud was changed."

### RUN_REPORTS.bat - the per-app review, separate from the migration
Asks the same two questions (space, apps) every time, downloads those apps
(a read - nothing in the cloud changes), writes the reports and ends on:

    Running. Every detail goes to fleet/last_run.log
      1/2 Downloading the apps ...
      2/2 Writing the reports ...
    ========================================================================
    REPORTS - 2 app(s)
    ------------------------------------------------------------------------
    ok      01 ESS QVD Builder - CDP
            fleet/apps/01 ESS QVD Builder - CDP/report.html
    ok      05 - Grant Acquittals Report
            fleet/apps/05 - Grant Acquittals Report/report.html
    ------------------------------------------------------------------------
    2 ok, 0 not ok.  Open each report.html in a browser.

Per app: `report.html` (the readable review), `usage-tables.csv`,
`usage-fields.csv`, `usage-vars.csv` (unused tables, fields, variables,
dimensions, measures), `flags.csv` (GeoAnalytics, Inphinity, REST,
NPrinting hint, section access, unknown sources).

## 1. One-time setup
1. Copy `qlik_cli_path.txt.example` to `qlik_cli_path.txt` and put the path
   to qlik.exe on its one line.
2. Put every app that is to be migrated in one Qlik Cloud shared space.
   Nothing else goes in that space. QVD builder/generator apps never do.
3. Create the staging space that will receive the rebuilt copies.
4. First time only, before anything else:
   `Rscript fleet/fleet.R doctor --space <id>`
   Read-only; it writes nothing to the cloud. It makes each call in
   the appendix and checks the reply carries the keys this tool reads,
   printing PASS/FAIL per call and exiting 1 if any failed. If a call
   FAILs with "expected `data`, got keys: ..." the tenant answers in a
   shape this code does not read yet - send that line on, do not run
   other verbs.

## Step 1 - which space are the apps in?
    Rscript fleet/fleet.R spaces
`spaces` prints `[n] name (type)` and caches the list, ids and all, in
`fleet/spaces.csv`. Everything after this is scoped to the one space:
`--space <id>` (or `--space <n>`, the number from that list) selects it.

## Step 2 - which apps, then download them
    Rscript fleet/fleet.R apps --space <space id>
    Rscript fleet/fleet.R add --space <space id> --apps 1,3-5
    Rscript fleet/fleet.R fetch --selected
`apps` lists the space's apps numbered; `add` takes those numbers (or
`--all`) and writes one row per app to `fleet/manifest.csv`, the ledger.
It also records the pick in `fleet/selection.csv`, which is what
`--selected` means from then on - use it for every later step so a
3-app migration stays 3 apps. `fetch` downloads each app's script and
objects into `fleet/apps/<app name>/`. Both are tenant READS - nothing
changes in the cloud. If an app's download fails, that app is FAILED for
the rest of the run: an older copy left in its folder is never formatted
or uploaded.

## Step 3 - format and retarget
    Rscript fleet/fleet.R process --selected
Runs the eight styling passes (`2_styling/`), then rewrites every on-prem
QVD load to its cloud view using `3_retargeting/qvd_field_map.csv`. Per app
it writes `script_styled.qvs`, `script_retargeted.qvs`,
`retarget_report.csv` (one line per load: retargeted / not-in-map /
multi-source / commented / out-of-scope), `changes/` (one CSV per styling
pass), `log.txt`. Loads it could not map are listed in `dev_notes.txt`
and `master_loads.csv`; the app still uploads. Nothing here touches the
cloud, and nothing here blocks.
Data Manager apps are unlocked automatically; their tables are no longer
editable in Data Manager afterwards (Adam 2026-11-02). The `///$autogenerated`
line goes, the tab becomes "Unlocked from Data Manager", styling starts from
`script_unlocked.qvs` (script.qvs is never changed) and the results screen
says "unlocked Data Manager section".

Reports by command line: `Rscript fleet/fleet.R report --selected`
(what RUN_REPORTS.bat runs after its own download).

## Step 4 - where the rebuilt apps go
    Rscript fleet/fleet.R upload --mode copy --to-space <staging id> --selected --dry-run
    Rscript fleet/fleet.R upload --mode copy --to-space <staging id> --selected
    Rscript fleet/fleet.R verify --selected
    Rscript fleet/fleet.R results --selected
Copies each app into staging as "<name> [mig]" and builds a script into the
copy (script only, no reload). It sends the BEST script the app has:
`script_retargeted.qvs`, else `script_styled.qvs`, else the original
`script.qvs`, and says which one it used. `--mode overwrite` builds onto
the original instead and needs no `--to-space`. Do one app live before
the batch. This is the only step that writes to the cloud.

`upload` checks where each copy actually landed and tries once to move a
copy that landed elsewhere; if that fails the app is FAILED with where it
went. A build that prints "Error:" is FAILED even if qlik-cli exits 0.
Every qlik call and its reply go to `fleet/apps/<app name>/upload.log`.

`verify` downloads each copy uploaded in THIS run and compares it with the
script that was sent. `results` prints the screen shown above.

KNOWN PROBLEM (2026-09-23, not fixed): `app build` runs with `--silent` and
a 60 s timeout. Trust `verify`, not a "built" row.

## The master list    (refreshed after every step)
    Rscript fleet/fleet.R rollup
    Rscript fleet/fleet.R status
Three files in `fleet/`, the record of the whole migration on this machine
(not in git - each machine keeps its own; fleet.R starts fresh ones):
- `master.csv` - one row per app: stage, readiness 0-100, load counts,
  what is still outstanding (`blockers` column), one column per flag,
  unused-object counts. Sort by readiness to see who is nearest done.
- `master_loads.csv` - every load statement in every app and its
  status. Filter status != retargeted for the outstanding work. `tab`
  and `tab_line` are WHERE THE EDITOR SHOWS IT - open that tab and go
  to that line; `line` is the line in the whole .qvs file, which the
  editor never shows.
- `master_unused.csv` - every unused table, field, variable, dimension
  and measure, by app. EMPTY until you run the reports: the counts
  come from the per-app `usage-*.csv` files, which only `report` writes.

## Tags in the hub
    Rscript fleet/fleet.R stamp --space <space id>
Progress tag: mig:processed, mig:built or mig:verified. Outstanding
tags, one per active flag: mig:inphinity, mig:geoanalytics, mig:nprint,
mig:unknown-src, ... `reconcile` reports where hub tags and the ledger
disagree (`fleet/tag_drift.csv`).

## When a QVD is not in the map
The map is frozen for the handover: `3_retargeting/qvd_field_map.csv` is
generated, never edited by hand. A load it cannot map is left exactly as
written and listed in the app's `dev_notes.txt` (tab and line) for a
developer to point at its cloud view by hand. The tools that built the map
are in git tag `pre-restructure`.

## Where things are
| What | Where |
|---|---|
| Ledger | fleet/manifest.csv |
| The apps you picked | fleet/selection.csv (what `--selected` means) |
| Master list | fleet/master.csv, master_loads.csv, master_unused.csv |
| Per-app outputs | fleet/apps/<app name>/ |
| Which folder is whose | fleet/apps/index.csv (app id -> folder) |
| Loads that need a hand | fleet/apps/<app name>/dev_notes.txt |
| This run's screen log | fleet/last_run.log |
| Cloud call audit | fleet/audit.log, fleet/apps/<app name>/upload.log |
| The QVD map | 3_retargeting/qvd_field_map.csv (generated, never edit) |
| Flag definitions | fleet/flags.csv (add a row to add a flag) |

## Appendix - the qlik-cli calls, and what `doctor` checks
The walk-through ran against the tenant on 2026-09-22/23 (3 apps). Not every
call below has a measured tenant reply: `data-connection ls` and
`app space update` (the move after a stray copy) are untested on the tenant.
`doctor` makes the read-only calls for real.
| call | used by |
|---|---|
| paging: `--limit N`, `--next <token>`; reply `links.next.href` with `next=`, rows under `data` | every listing |
| `app unbuild --app <id> --dir <d> --no-data` | fetch, verify |
| `item ls --resourceIds <csv> --resourceType app` -> rows with `id`, `resourceId` | add, reconcile-ids, upload |
| `item collections <itemId>` -> `{id, name}` rows | stamp, reconcile |
| `collection ls` -> `{id, name, type}` rows | stamp |
| `collection create --name X --type public` -> `id`; 409 on duplicate | stamp |
| `collection item create --collectionId C --id <itemId>` | stamp |
| `collection item rm <itemId> --collectionId C` | stamp |
| `app copy <id> --attributes-spaceId S --attributes-name N` -> `attributes.id` | upload |
| `app ls --spaceId S` | apps, upload (where did the copy land) |
| `app space update <id> --spaceId S` | upload (move a stray copy) |
| `app build --app <id> --script f --no-reload --silent` | upload |
| `data-connection ls --spaceId S` -> rows with `qName` or `name` | upload (warning only) |
