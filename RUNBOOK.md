# Migration pipeline — how to run it and what comes out

Everything runs from the repo root on a machine that has R and, for the
cloud steps, qlik-cli signed in (`qlik context use <name>` done once).
Menu: double-click `launch_console_ui.bat`. Command line: every menu
item is `Rscript fleet/fleet.R <verb>`; use it for unattended runs.
Cloud-writing steps print their commands and do nothing until you add
`--live` (menu: type LIVE when asked).

## 1. One-time setup
1. Put the path to qlik.exe in `qlik_cli_path.txt` (copy the .example).
2. Put every app that is to be migrated in one Qlik Cloud shared space.
   Nothing else goes in that space. QVD builder/generator apps never do.
3. Create the staging space that will receive the rebuilt copies.
4. First time only, before anything else:
   `Rscript fleet/fleet.R doctor --space <id>`
   Read-only; it writes nothing to the cloud. It makes each call in
   the appendix and checks the reply carries the keys this tool reads,
   printing PASS/FAIL per call and exiting 1 if any failed. The
   appendix is the reference of what it checks. If a call FAILs with
   "expected `data`, got keys: ..." the tenant answers in a shape this
   code does not read yet — send that line on, do not run other verbs.

## 2. Connect and pick the apps    (menu [3])
    Rscript fleet/fleet.R spaces --name "<part of space name>"
    Rscript fleet/fleet.R apps --space <space id>
    Rscript fleet/fleet.R add --space <space id> --all
`add` writes one row per app to `fleet/manifest.csv`, the ledger. Every
later step reads and updates it. Use `--apps 1,3-5` instead of `--all`
for a subset.

## 3. Unpack    (menu [4])
    Rscript fleet/fleet.R fetch --all
Downloads each app's script and objects into `fleet/apps/<app id>/`.

## 4. Format and retarget    (menu [5])
    Rscript fleet/fleet.R process --all
Runs the styling passes, then rewrites every on-prem QVD load to its
cloud view using `retargeting/qvd_field_map.csv`. Per app it writes
`script_styled.qvs`, `script_retargeted.qvs`, `retarget_report.csv`
(one line per load: retargeted / not-in-map / multi-source / commented /
out-of-scope), `changes/` (one CSV per styling pass), `log.txt`.
An app with any not-in-map or multi-source load is marked **blocked**
and will not upload. `--allow-unresolved` overrides for a look.

## 5. Review and tech-debt report    (menu [6])
    Rscript fleet/fleet.R report --all
Independent of step 4; can run the day the apps are fetched. Per app:
`report.html` (the readable review), `usage-tables.csv`,
`usage-fields.csv`, `usage-vars.csv` (unused tables, fields, variables,
dimensions, measures), `flags.csv` (GeoAnalytics, Inphinity, REST,
NPrinting hint, section access, unknown sources).

## 6. The master list    (menu [7], also refreshed after every step)
    Rscript fleet/fleet.R rollup
    Rscript fleet/fleet.R status
Three files in `fleet/`, the record of the whole migration:
- `master.csv` — one row per app: stage, readiness 0-100, load counts,
  what is still outstanding (`blockers` column), one column per flag,
  unused-object counts. Sort by readiness to see who is nearest done.
- `master_loads.csv` — every load statement in every app and its
  status. Filter status != retargeted for the outstanding work.
- `master_unused.csv` — every unused table, field, variable, dimension
  and measure, by app.

## 7. Rebuild into staging    (menu [8])
    Rscript fleet/fleet.R upload --mode copy --to-space <staging id> --all
    Rscript fleet/fleet.R upload --mode copy --to-space <staging id> --all --live
    Rscript fleet/fleet.R verify --all --live
Copies each app into staging as "<name> [mig]", builds the retargeted
script into the copy (script only, no reload), then `verify` pulls the
copy back and confirms the script matches. `--mode overwrite` builds
onto the original instead. Do one app live before the batch.

## 8. Tags in the hub    (menu [9])
    Rscript fleet/fleet.R stamp --all --live
Progress tag: mig:processed, mig:built or mig:verified. Outstanding
tags, one per active flag: mig:inphinity, mig:geoanalytics, mig:nprint,
mig:unknown-src, ... `reconcile` reports where hub tags and the ledger
disagree (`fleet/tag_drift.csv`).

## 9. When a QVD is not in the map
- View now exists in the cloud: refresh `fixtures/DBfixture1.csv` (and
  `fixtures/loaded_schemas.csv` for a new schema), then
  `Rscript retargeting/map_refresh.R`. It prints what flipped and which
  apps to re-process.
- No lineage at all: `Rscript retargeting/map_add.R --onprem-qvd
  "<path>" --schema <S> --view <V> --all-fields` (or `--fields a=b,...`),
  then map_refresh. Rows land in `retargeting/lineage_manual.csv`.
- Any time: `Rscript retargeting/map_check.R` validates the map.

## Where things are
| What | Where |
|---|---|
| Ledger | fleet/manifest.csv |
| Master list | fleet/master.csv, master_loads.csv, master_unused.csv |
| Per-app outputs | fleet/apps/<app id>/ |
| Cloud call audit | fleet/audit.log |
| The QVD map | retargeting/qvd_field_map.csv (generated, never edit) |
| Hand mappings | retargeting/lineage_manual.csv |
| Flag definitions | fleet/flags.csv (add a row to add a flag) |

## Appendix — the qlik-cli calls, and what `doctor` checks
Never run against a tenant yet; the code sends exactly these, and
setup step 4's `doctor` makes the read-only ones for real.
| call | used by |
|---|---|
| paging: `--limit N`, `--next <token>`; reply `links.next.href` with `next=`, rows under `data` | every listing |
| `app unbuild --app <id> --dir <d> --no-data` | fetch, verify |
| `item ls --resourceIds <csv> --resourceType app` -> rows with `id`, `resourceId` | add, reconcile-ids |
| `item collections <itemId>` -> `{id, name}` rows | stamp, reconcile |
| `collection ls` -> `{id, name, type}` rows | stamp |
| `collection create --name X --type public` -> `id`; 409 on duplicate | stamp |
| `collection item create --collectionId C --id <itemId>` | stamp |
| `collection item rm <itemId> --collectionId C` | stamp |
| `app copy <id> --attributes-spaceId S --attributes-name N` -> `attributes.id` | upload |
| `app build --app <id> --script f --no-reload --silent` | upload |
| `data-connection ls --spaceId S` -> rows with `qName` or `name` | upload (warning only) |
