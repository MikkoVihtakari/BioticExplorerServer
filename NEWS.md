# BioticExplorerServer 0.8.5 (development version)

*2026-08-23*

- Added `gearcondition` to the default `codeindex` reference tables. `samplequality` remains
  included; both central survey-quality fields are now refreshed by `compileDatabase()` and
  `updateDatabase()`.
- Excluded deprecated Reference API rows before reducing code tables to their offline
  code-to-meaning mappings.

# BioticExplorerServer 0.8.4

*2026-08-04*

- Fixed the first `updateDatabase()` run after `compileDatabase()`: it now
  establishes the API delivery manifest from API timestamps, avoiding a
  needless full redownload caused by differences between API routing keys and
  parsed mission labels.
- Rebased older mission-derived update manifests to API delivery keys, so
  unchanged years are no longer repeatedly flagged for download.
- Ignore malformed API inventory years outside the supported 1900–current-year
  range.
- Retried transient Reference API connection failures while preparing cruise
  series, gear, taxa, and coded-field reference tables. This keeps both
  `updateDatabase()` and `compileDatabase()` from failing on a dropped TLS
  connection during reference refreshes.
- Retried transient Biotic API timeouts and parallelized delivery-inventory and
  metadata discovery with bounded concurrency. Reference-code tables that are
  not exposed by the Reference API are no longer requested.


# BioticExplorerServer 0.8.0

*2026-06-19*

- Added incremental update refinements to `updateDatabase()`. Progress reporting
  is now concise by default, interactive sessions use a progress bar, and
  `verbose = TRUE` still exposes delivery-level logging. Metadata scans stop
  after the first changed delivery in a year while still tracking post-download
  baselines so later additions, removals, and changes are detected reliably.
- Updated `updateDatabase()` to refresh the cruise-series, gear, taxa, and
  coded-field reference tables on every run. Reference tables are replaced
  together only after all downloads succeed, and fresh cruise-series and gear
  mappings are used immediately for changed annual data.
- Improved resilience for incremental updates. Missing deliveries that return
  HTTP 404 during metadata lookup are skipped with one aggregate warning, while
  other HTTP failures remain fatal, and incremental writes from `data.table`
  inputs now target the actual database columns correctly.
- Added start time, successful finish time, and elapsed minutes to the log
  output from `compileDatabase()` and `updateDatabase()`.
- Added the missing `dbplyr` runtime dependency required by `indexDatabase()`
  on fresh R installations.
- Added regression tests covering concise and verbose update progress output,
  unavailable deliveries, `data.table` writes, and operation timing helpers.

# BioticExplorerServer 0.7.0

*2026-06-18*

- Added `updateDatabase()`, which uses delivery-level API metadata to download and
  atomically replace only years whose data changed, were added, or were removed.
- Added database schema and package-version metadata. An incompatible schema now
  triggers a full sibling rebuild through `compileDatabase()` and a validated swap.
- Fixed `overwrite = TRUE` so existing annual data are replaced instead of duplicated.
- Added a delivery manifest containing metadata-only change signals from the Biotic API.

- Added `prepareReferenceCodes()`, which pulls the simple coded `KeyType` fields (`sex`, `maturationstage`, `missiontype`, `nation`, …) from the NMD Reference API and writes them to the DuckDB database as the long-format `codeindex` table via `compileDatabase()`. This lets agents and the Shiny app decode these fields offline with a join instead of a per-code API call. Editor-identity columns (`updatedBy`/`insertedBy`/timestamps) are stripped on build so no staff usernames enter the database. Composite taxa/sex-keyed tables (`specialstage`, `eggstage`, …) are left to the API.

# BioticExplorerServer 0.6.0

*2026-06-15*

- Added taxa reference export to DuckDB as the `taxaindex` table through `downloadDatabase()`.
- Updated database examples and agent guidance for the taxa index.
- Removed generated documentation output that exposed local machine-specific database paths.

# BioticExplorerServer 0.5.0

*2026-05-19*

### Bug fixes

- Fixed a critical crash caused by self-referential default arguments (`icesAreas = icesAreas`, `cruiseSeries = cruiseSeries`, `gearCodes = gearCodes`) in `bioticToDatabase()`, `downloadDatabase()`, and `downloadDatabaseToFiles()`. These now default to `NULL` and load the bundled package data in the function body.
- `stndat` merge changed from right join (`all.y = TRUE`) to full outer join (`all = TRUE`), so empty fishing stations (tows with no catch) are now correctly preserved in `$stnall` with `commonname = NA` instead of being silently dropped.
- Individual records that cannot be matched to a catch sample entry now emit a warning and are excluded from `$indall` before age merges, rather than stopping execution with an error.
- Added a guard for empty age data (`nrow(age) == 0`) before computing `$ageall` to prevent crashes on surveys with no age readings.
- Fixed progress bar maximum in `bioticToDatabase()` (10 → 11 steps) and `indexDatabase()` (6 → 7 steps).
- Fixed outdated API URL in `downloadDatabaseToFiles()` (old `tomcat7.imr.no:8080` → `biotic-api.hi.no`).
- Fixed typo "Gead codes" → "Gear codes" in `compileDatabase()`.

### Other changes

- Added pkgdown website.
- Added comprehensive `@return` documentation for all exported functions.
- Added NEWS.md.

---

# BioticExplorerServer 0.4.4

*2025-02-25*

- Updated Biotic API URL to `https://biotic-api.hi.no`.
- Rewrote `prepareTaxaList()` to handle missing names in the taxa reference list.
- Updated reference API URL.

---

# BioticExplorerServer 0.4.3

*2025-01-22*

- Completed conversion from MonetDB to DuckDB.
- Fixed `dbPath` handling on Windows.
- Added `read_only` connection mode documentation to README.

---

# BioticExplorerServer 0.4.x

*2022–2024*

- Increased download timeout limit.
- Fixed a bug in the age element of `bioticToDatabase()`.
- Fixed a bug in calculation of `numberofreads` in `$ageall`.
- Removed debugging messages from `prepareCruiseSeriesList()`.
- Added all age readings to `$ageall`. Fixed a warning in `prepareCruiseSeriesList()`.
- Turned off s2 for ICES area determination to improve spatial join compatibility.
- Fixed crash in `compileDatabase()` → `indexDatabase()`.
- Fixed default path for the index file.

---

# BioticExplorerServer 0.3.x

*2021*

- Replaced `sp` and `rgdal` dependencies with `sf` for spatial operations.
- Fixed CRS transform issue on Linux ([sf#1419](https://github.com/r-spatial/sf/issues/1419)).
- Fixed ICES area CRS assignment.
- Fixed a critical issue with NA `commonname` values crashing database compilation.
- Stopped converting station dates with `as.Date` to preserve time-zone information.

---

# BioticExplorerServer 0.2.x

*2020–2021*

- Transitioned from MonetDBLite to DuckDB as the embedded database engine.
- Added ICES area calculation using bundled simplified polygons.
- Added cruise series information and index (`csindex` table).
- Added age reading data (`$ageall` / `ageall` table).
- Added gear category classification.
- Added `downloadDatabaseToFiles()` for saving XML and RDS outputs.
- Improved performance of `indexDatabase()`.

---

# BioticExplorerServer 0.1.0

*2020-05-14*

- Initial release.
- `compileDatabase()`, `downloadDatabase()`, `bioticToDatabase()`, and `indexDatabase()` functions.
- DuckDB backend for storing NMD Biotic data parsed from annual XML files.
- ICES area and gear category added to station data.
- Cruise series index stored in the database.
