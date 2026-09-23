# bctu 0.24.2

## Changes

* The table of contents field is no longer flagged for update: even that one flag brought Word's update prompt back on every opening. F9 on the table fills the page numbers.

# bctu 0.24.1

## Changes

* The running header and footer fields are written with their results (`fill_running_fields()`): the header names the trial and report on opening rather than the template's placeholder, and the page-number fields carry a result region for Word to fill.
* The table of contents field alone is flagged for update, so Word fills its page numbers on opening without the document-wide prompt.

# bctu 0.24.0

## Changes

* Every rendered docx carries a provenance stamp in its document properties (`bctu <version>`, template, render time), read back with `report_provenance()`.
* New `check_report_layout()` lays a docx out with LibreOffice and reports orphaned captions, blank and sparse pages.
* The docx is rewritten with the zip package, so no `zip` binary is needed.
* Theme fonts and `font.size` are validated, as colours were; `n_pct()` checks its inputs.

# bctu 0.23.0

## Reports: tables

* `render_table()` turns `indent()` levels into label columns: a label is merged rightwards over the deeper label columns and a deeper label sits in its own column beneath it, in place of non-breaking-space indentation. `grid_table()` gains `levels`.
* `render_table()` gains `bold_rows` (a whole row in bold, for a primary outcome) and `bold_headings` (`FALSE` bolds only `bold_rows`). A cell may open with a `**label**`.
* A `::: footnote` fenced div straight after a table is its footnote block, set against the table.
* Captions keep with the start of their table (header row keep-with-next), figures sit inside their caption paragraph with lines kept together, and a report whose last content is landscape no longer ends with a blank page.

## Reports: document

* New `report_theme()`, a ggplot2-style theme for `trial_report`: elements `colour.accent`, `rule.colour`, `table.border.colour`, `table.header.fill`, `text.secondary.colour`, `link.colour`, `font.body`, `font.heading`, `font.title`, `font.code` and `font.size`, inheriting from one another, given in R or as the YAML `theme` key. Each colour is bound to its own Word theme slot, so a report also recolours from Word's Design menu; the post-processor writes the theme and the literal fallbacks other renderers read.
* `trial_report()` gains `template`, from a registry naming each shipped template's resources and theme mapping. `report_styles()` lists a template's styles by the name pandoc's `custom-style` takes.
* The table of contents opens filled with the headings, linked to their bookmarks, and Word no longer asks to update fields on opening (pandoc's update-on-open setting and every dirty-field flag, including the template's running-header fields, are removed). Page numbers appear on the first Update Field.

# bctu 0.22.5

## Changes

* A `::: footnote` fenced div straight after a table is styled as the table's footnote block (`FootnoteBlockText`), with no blank paragraph between them.
* The header row of every table gets keep-with-next, so a caption moves to the next page with the start of its table while long tables still flow.

# bctu 0.22.4

## Changes

* `render_table()` gains `bold_rows`: the rows named are shown in bold across every column, for example the primary outcome row of an outcome table.

# bctu 0.22.3

## Fixes

* The confidentiality page text is bold.
* Consecutive `::: landscape` sections merge into one landscape section, so the empty portrait section between them no longer prints a blank page.
* A page break next to a landscape section boundary is dropped, so a `\newpage` between chapters no longer adds a blank page.

# bctu 0.22.2

## Fixes

* Rendered trial reports no longer open with "unreadable content" in Word: a post-processor binds the title-page header and footer references to the relationship ids pandoc assigns, so the BCTU logo header reaches the title page.
* Figures inside `::: landscape` sections are widened to the landscape text width instead of staying at the portrait width.
* New `trial_report` YAML field `confidential`: its text is placed on its own page, centred, after the title page.
* Consecutive tables are separated by a blank paragraph.
* New `repair_report_docx()`, applied automatically by `trial_report()`.

# bctu 0.22.1

## Fixes

* `load_snapshot()` loads the haven namespace when a table carries labelled columns, so `==`, `ifelse()` and binding on those columns work whether or not the caller attached haven first.

# bctu 0.22.0

## Changes

* `save_dvr()`, `save_cdi()`, `run_data_report()` and `write_report_set()` take `split_by`, an ordered vector of grouping columns (outermost first) in place of `site_col`. Folders nest, one level per column, and every level gets its own workbook: `split_by = c("country", "site")` writes a workbook per country and, inside it, one per site. `site_col` is removed.
* `check_split_nesting()`: an inner group appearing under more than one outer group is an error before anything is written.
* `include_resolved` defaults to `TRUE`, so a compared report writes `full/`, `new/` and `resolved/` at every level.
* `summary.md` written beside `manifest.yml`: run identity, findings per check, and findings per group at every level, each as current / new / unchanged / resolved.
* `finding_group_labels()` and `write_report_summary()` exported.

## Fixes

* REDCap exports are typed from every row (`guess_max = Inf`). A sparsely completed field whose entered values all fell outside readr's 1000-row sample was read as logical, so `1` became `TRUE` and every other code was silently dropped.
* `redcap_type_empty_columns()`: a field with no data in an extract takes the type its dictionary entry implies (labelled code, number, date or text) instead of logical, so a column no longer changes type between snapshots once its first value is entered.
* `align_findings()`: `bind_findings()` fills a missing column with typed `NA`s and widens a column whose type differs between the two snapshots to text with a warning, instead of failing with `Can't convert <logical> to <labelled<double>>`.

# bctu 0.21.0

## Changes

* University of Birmingham colours restored from 0.14: `uob_palettes`, `uob_tint()`, `show_uob_palettes()`, `pal_uob()` and the discrete and continuous `scale_colour_uob()` / `scale_color_uob()` / `scale_fill_uob()` / `*_uob_c()` scales.
* `theme_bctu_report()` restored: the shared figure theme used by BCTU Word trial reports.
* New `count_by_month()`: monthly counts with every calendar month present, so months with no events show as `n = 0`.
* `pal_uob()` applies `alpha` as ggsci does, over 8-bit channels.

# bctu 0.21.0

## Changes

* New University of Birmingham colour scales in ggsci style: `pal_uob()`, `scale_colour_uob()`, `scale_color_uob()`, `scale_fill_uob()` and `uob_colours()`.

# bctu 0.20.0

## Changes

* `trial_report()` restored: the `bctu::trial_report` rmarkdown Word format with the bundled BCTU reference.docx, title-page Lua filter, logo and RStudio template.
* `fig_portrait()` and `fig_landscape()` restored, with the `"fig_portrait"`/`"fig_landscape"` knitr chunk templates registered on load.
* New report-table helpers for Rmd reports: `render_table()`, `grid_table()`, `col_widths()`, `banner_row()`, `indent()` and `n_pct()`.
* `render_table()` escapes a leading `-`, `+` or `*` in a cell so pandoc no longer drops it as a list bullet.
* New reason-map utilities: `squash_text()`, `update_reason_map()` and `map_clean()`.
* The bundled reference.docx and title_page.lua moved to `inst/rmarkdown/templates/report/resources/`; `render_report()` reads them there.
* `render_trial_report()` from 0.14 is not restored; use `render_report()` or `rmarkdown::render()`.

# bctu 0.19.0

## Changes

* `save_dvr()` and `save_cdi()` now compare by default: `before` defaults to
  `"penultimate"`, resolved to the snapshot immediately BEFORE `after` in its
  own store (so a rerun on an older snapshot compares against its
  predecessor, never against newer data), restoring the original package's
  `since = "penultimate"` behaviour lost in the rebuild. Resolution failures
  (a store with one snapshot, no resolvable store, or an earlier directory
  the package cannot read, e.g. a pre-rebuild snapshot without a manifest)
  fall back to an uncompared report with a message, never an error. An
  explicit `before = NULL` still means no comparison; an explicit snapshot
  or selector string is honoured. `save_cdi()` gains the full comparison
  interface (`before`, `status_output`, `include_resolved`) with the same
  defaults.

# bctu 0.18.3

## Fixes

* Right-aligned and centred table cells no longer render as in-cell code
  blocks. The grid writer visually aligned cells by left-padding them with
  spaces; grid cells are block-level markdown, so four or more leading spaces
  (any wide right-aligned column, e.g. an Expected count under a long
  heading) turned the cell into indented code. Content is now always placed
  left in the source cell and padded on the right; column alignment is
  carried, as it always was for the rendered output, by the colon markers on
  the header border line.

# bctu 0.18.2

## Fixes

* The styled BCTU Word title page is back: the rebuild had dropped the
  original package's `reference.docx` and `title_page.lua` (rmarkdown
  template resources), so migrated DOCX reports rendered with no title page
  and bare styles. Both are now bundled in `inst/report/` byte-identical to
  the originals, and `render_report()` gains `title_page` (default: on when
  the report has `meta`): the DOCX pass applies the Lua filter, maps `meta`
  onto the original template's YAML contract (`trial` to `trial-short-name`,
  `registration`, `report_type`, `subtype`, `trial_long_name`; other entries
  become metadata-table rows after an automatic render-date row), requests
  the Word TOC via the filter when `toc = TRUE`, and defaults the reference
  document to the bundled one when no `template` is given. PDF output is
  unchanged.

# bctu 0.18.1

## Improvements

* Run-log condition capture now works inside knitr / R Markdown renders: the
  handlers are installed through knitr's own `calling.handlers` chunk option
  (the route rlang uses), effective from the chunk after `start_log()`, and
  restored on `stop_log()`. The record's `condition_capture` field now names
  the route: `global` (top level), `knitr`, or `none` (called under
  `tryCatch()` or similar, where R refuses global handlers; investigated and
  confirmed unmanageable in general: every catching guard is itself the thing
  R refuses to run under, and registering from a task callback desyncs R's
  handler registry without ever taking effect).

# bctu 0.18.0

## New features

* Run logs: `start_log()` opens a recorded session (SAS `proc printto` /
  Stata `log using` style) and `stop_log()` closes it. Each run gets one
  directory under `<project dir>/log-output` (or an explicit `location`)
  holding a plain-text transcript (`run.log`: printed output, warnings,
  errors, messages, and the command echo), a structured YAML record
  (`run.yml`, schema `bctu-runlog/1`: who/when/where, R and package versions,
  snapshot identity and fingerprint, parameters, input and output file
  SHA-256s, status and tallies, and the transcript's own SHA-256), and a
  `run.msg` sidecar only when the run was not clean. An append-only
  `index.csv` lists every run; same-second collisions get a suffix, nothing
  is ever overwritten. The header is written to disc before any work runs,
  so even a killed session leaves the header and partial transcript; a
  `quit()` or session end without `stop_log()` records `interrupted`.
  Nothing about the console experience changes (split sink; conditions
  observed without muffling).
* `checkpoint()` now also writes its stamp into the open run log (transcript
  block plus a `checkpoints:` entry in the record) and, interactively with no
  log open, starts one with the defaults; the run's timestamp is that
  stamp. In non-interactive use with no log open it remains a pure stamp.
  `list_logs()` and `read_log()` read the run history back.

# bctu 0.17.5

## Fixes

* A table cell containing only dashes ("---", the common empty-cell
  placeholder) no longer typesets as a horizontal rule: block-level markdown
  reads a dashes-only line as a thematic break, so it is escaped and reaches
  the PDF as an em dash, as the old inline pipeline rendered it.

# bctu 0.17.4

## Fixes

* Grid-table cells are kept literal: pandoc parses grid cells as block-level
  markdown, so a label starting with a list marker ("1. ", "II. ", "a) "),
  bullet, heading or quote marker was typeset as that construct (an enumerate
  environment with its own spacing, indentation and renumbering). The marker
  punctuation is now backslash-escaped; a cell is data, never markup.

# bctu 0.17.3

## Changes

* PDF tables now go through the same pandoc grid-table pipeline as DOCX,
  instead of hand-rolled raw LaTeX. Pandoc typesets them as longtables with
  computed column widths, header wrapping and clean page breaks, restoring the
  layout the pre-rebuild (kable-based) reports had; the raw-LaTeX path
  produced non-breaking floats that overflowed the page and collided with
  following text. `render_table_latex()` and its escaping helper are removed
  (capability drop: LaTeX `\multicolumn` spanning headers; grid spanning
  header rows render equivalently in both formats).
* Leading spaces in table cells (hierarchy indentation, e.g. classification
  tiers) are preserved as no-break spaces in every output format; previously
  the PDF path dropped them, flattening indented tables.

# bctu 0.17.2

## Fixes

* Report metadata reaches pandoc again: the generated markdown's YAML block
  was joined with blank lines between its delimiters, so pandoc read it as a
  horizontal rule and dropped the metadata. Every rebuilt-renderer output
  (PDF and DOCX) therefore lost its title, and 0.17.1's `geometry` never
  applied (PDFs stayed portrait). The header is now contiguous and a test
  pins the exact block.

# bctu 0.17.1

## Fixes

* `render_report()` gains explicit page-layout options: `orientation`
  (`"portrait"`/`"landscape"`, PDF geometry), `margin` (PDF, default `"1in"`),
  `toc` and `number_sections` (all formats). The rebuilt renderer previously
  emitted only a bare title header, so migrated PDF reports (C-SAFE weekly)
  rendered portrait with no table of contents or section numbering; trial
  scripts can now state the layout the old Rmd headers carried. The layout is
  recorded in `report-manifest.yml`.

# bctu 0.17.0

## Changes

* Compared reports (`save_dvr()` with a `before` snapshot) now default to
  writing the change labelling as separate folders: `full/` (current findings,
  new and unchanged) and `new/`, with no `status` column in the written files.
  The previous presentation (a `status` column on `full/` plus an `update/`
  set of the changed rows) is available with `status_output = "column"`.
* Resolved findings are no longer written by default in either mode: a
  resolved row shows the before snapshot's data values, which are stale
  against the current extract. `include_resolved = TRUE` writes them (a
  `resolved/` folder set, or `resolved`-labelled rows in column mode).
  Per-check new/unchanged/resolved tallies are always recorded in the
  manifest, which also records the mode and whether resolved was included.

# bctu 0.16.6

## Fixes

* A DVP returning an explicitly named empty list (`setNames(list(),
  character(0))`, the documented placeholder for a trial with no checks
  defined yet) is accepted again and yields a zero-check report, as under the
  original package. A bare `list()` with `NULL` names is still rejected.

# bctu 0.16.5

## Fixes

* REDCap CSV parsing treats blank cells as `NA` again (`na = c("", "NA")`).
  0.15.0 to 0.16.4 disabled NA strings entirely, so blanks in character
  columns arrived as empty strings; any `is.na()`-based logic downstream (for
  example a repeat-instrument filter) then silently matched nothing. This also
  produced the spurious "parsing issues" warnings on typed columns. Snapshots
  fetched with affected versions should be re-taken.

# bctu 0.16.4

## Fixes

* REDCap labelling now covers checkbox columns: dictionary rows are matched to
  exported column names through the field-name export map (`exportFieldNames`),
  so `field___N` checkbox columns (and their No/Yes coding) are labelled the
  way the original package labelled them. Under 0.15.0 to 0.16.3 those columns
  were silently left as plain numeric, which made label-string checks return
  zero findings on affected trials; snapshots fetched with those versions carry
  the defective typing and should be re-taken.
* The standalone `checks_index.csv`/`.txt` files are no longer written by
  default: the index lives as the first tab of every workbook. `write_readable
  = TRUE` restores them alongside the per-check CSV/TXT copies.

# bctu 0.16.3

## Changed

* The DVR/CDI delivered record is the Excel workbook, always: `openxlsx` is
  required up front (plain-English error if missing; the silent skip-the-
  workbook fallback is gone) and the per-check CSV/TXT copies are no longer
  written by default. `write_readable = TRUE` (replacing `write_xlsx`) opts
  back in; `checks_index.csv`/`.txt` and the YAML manifest are always written.

# bctu 0.16.2

## Fixes and improvements

* On-disk layout restored to the house convention: each report writes to
  `<path>/<after snapshot id>/v<version>/` (a folder per data state, then a
  folder per controlled document version; unversioned reports write to
  `<path>/<after snapshot id>/`). The report id string, manifest and workbook
  names keep the full `DVR-v<version>-<snapshot id>` identity from 0.16.1.

# bctu 0.16.1

## Fixes and improvements

* Per-site split: a finding that carries `site_col` as one of its own columns
  is sited from that column directly (row by row), falling back to the snapshot
  id-to-site lookup only where it is missing. Trials whose datasets have no
  single id column can now split per site without a snapshot-format change.
* Report identity: the id and output directory are keyed by the data validated,
  `<KIND>-v<version>-<after snapshot id>` (e.g. `DVR-v0.5-2026-08-13T111608Z`),
  carrying the controlled document version when given. The run moment stays in
  the manifest as `created_utc`. A second run on the same snapshot and version
  gets an explicit `_N`-suffixed directory, never a silent overwrite; an
  unsaved after snapshot falls back to the run time as its id.

# bctu 0.16.0

## New features

* Per-check query text: `save_dvr()`/`save_cdi()`/`run_data_report()` gain
  `check_info` (data frame: `check`, `query`, optional free-text `section` and
  logical `critical`) and `query_column`. The engine writes a `checks_index`
  sheet first in every workbook (overall and per-site), `checks_index.csv`/`.txt`
  beside the findings in every output directory, prepends the query text as the
  first column of each finding row (added after the before/after comparison, so
  rewording a query never shows as new/resolved), and records query, section and
  criticality per check in the manifest. A DVP function can carry the same table
  itself via `attr(findings, "check_info")`; the explicit argument wins.

# bctu 0.15.0

Ground-up rebuild of the package, continuing the version sequence from 0.14.0.
Validated against the OCeAN weekly pipeline (snapshot, DVR, CDI, TMG report)
on 2026-08-13.

## New features

* `special_missing()` declares REDCap missing-data codes (e.g. `UNK`, `OTH`) and
  maps each to a single-letter tag. Pass it to `datasource_redcap(missing_codes =)`
  and the codes are converted at extraction time to native special missing values:
  each column keeps its natural type instead of coercing to character. Snapshots
  export them as Stata `.a` (`.dta`), SAS `.A` (`.sas7bdat`/`.xpt`, best effort), and
  always ship a `.sas` import script that recodes the readable CSV as the reliable
  path.
* `take_snapshot()` / `save_snapshot()` gain `tag =` and `labels =` to mark an
  extraction (e.g. `"DMC-2026-08"`). The tag is recorded on the snapshot and its
  tables, in the manifest, and flows into the DVR/CDI and report manifests. When a
  `tag` is given, bctu also creates an annotated `snap/<tag>` git tag.
* New snapshot payload formats: `"dta"`, `"sas7bdat"`, `"xpt"`, and `"sas"` (an
  import script), alongside `"rds"` and `"csv"`.

## Audit trail and safety

* Git history is the audit trail. Every `take_snapshot()`/`save_snapshot()` and
  every `delete_snapshot()` commits the snapshot metadata (the manifest only,
  never the payload) to the trial repository by default. A destroy is committed
  too, so removing a snapshot still leaves a git record (author, time, reason).
  Use `git = "off"` to skip, or `git = "record"` on save to write HEAD into the
  manifest without committing.
* One store resolver, `snapshot_store()`, which errors when no `bctu-project.yml`
  is found instead of silently falling back to the working directory. Pass an
  explicit `store =` to write outside a project. The former `snapshot_location()`
  is removed.
* Table names are validated before use as directory names (path-traversal guard).
* Same-second snapshots use a `-NN` id suffix instead of distorting the recorded
  time; the manifest is written atomically; `verify_snapshot()` reports `ok = FALSE`
  when there is nothing to verify; loading warns whenever it falls back off the rds.

## Data sources

* REDCap responses are guarded: an HTTP 200 error body can no longer be snapshotted
  as data. Network and toolchain errors are reported in plain English, and `httr2`
  is checked before use with an install hint.
* `datasource_redcap()` gains a per-project keyring `service =`.

## Fixes

* Marker fields are validated with plain-English errors; malformed YAML is reported
  clearly. `has_credential()` no longer warns as a side effect.
