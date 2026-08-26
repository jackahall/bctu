# Prior art: R execution-logging packages for regulated statistical programming

Retrieved 2026-08-18 by web search, CRAN pages, and GitHub source inspection (raw file fetches
via `gh api`). All facts below are cited to a source fetched this run; anything not directly
confirmed is marked UNVERIFIED.

## 1. logrx (pharmaverse)

- Repo: https://github.com/pharmaverse/logrx
- CRAN: https://cran.r-project.org/package=logrx
- pkgdown site: https://pharmaverse.github.io/logrx/
- README/comparison article: https://pharmaverse.github.io/examples/logging/logging.html

**Maintainer / org**: Nathan Kosiba (creator/maintainer); other authors Thomas Bermudez, Ben
Straub, Michael Rimler, Nicholas Masel, Sam Parmar; copyright holder / funder GSK / Atorus JPT.
Part of the `pharmaverse` GitHub org (48 stars, 788 commits on main, 38 open issues at time of
fetch). Source: GitHub repo page and DESCRIPTION file fetched this run.

**License**: MIT + file LICENSE. Source: DESCRIPTION (raw fetch).

**Last release**: CRAN version 0.4.0, published 2025-05-05; GitHub main branch is at 0.4.0.9000
(dev). Source: CRAN index page and DESCRIPTION fetched this run.

**What a log contains** (from the comparison article and axecute.R/log.R source): file hash
(via `digest`), full `sessioninfo::session_info()`, masked-function detection, package
dependencies used by the script, optional `lintr` results, captured stdout/console output,
captured messages, captured warnings, and captured errors. Log is written as a plain-text `.log`
file plus an optional `.rds` object (`include_rds`).

**API**: single entry point `logrx::axecute(file, log_name, log_path, include_rds,
quit_on_error, to_report, show_repo_url, extra_info, ...)`, callable from the command line via
`Rscript -e "logrx::axecute('script.R')"`. There is a separate `logrxaddin` package providing an
RStudio Addin wrapper. Source: `R/axecute.R` raw fetch.

**Capture mechanism (verified from source, not just README)**: `axecute()` calls
`run_safely_loudly(file)`, which wraps `purrr::safely()` around a `loudly()` helper
(`R/utils-execution.R`). `loudly()` opens an in-memory file connection, calls
`sink(temp, split = TRUE)`, then runs the script's code inside `withCallingHandlers()` with
warning/message handlers that accumulate messages into R vectors, and reads the sunk stream back
with `readLines()`. **This all happens in the same R process that called `axecute()`**: there
is no subprocess isolation (no `callr`, no `Rscript` fork). Source: `R/utils-execution.R`,
`R/axecute.R` raw fetch, this run.

**Wraps arbitrary scripts?** Yes for a single `.R` or `.Rmd` file per call (the `examples()` in
the roxygen block show both). It does not orchestrate multi-script batch runs itself; that is
whirl's niche.

**File format**: plain-text `.log` (human-readable), optional `.rds` snapshot of the log object,
optional `.msg`-style unapproved-function tracking (see the "Logging Unapproved Package and
Function Use" vignette).

**Validation / pharmaverse status**: Listed as a pharmaverse package; pharmaverse's guidance
(fetched via search) frames logrx's function-level tracking as a tool that *feeds* a risk
assessment workflow (pairs with `riskmetric`/`riskassessment`/`risk.assessr`), not itself a
"validated" package with a certification badge. CRAN has current checks passing (implied by
listing) but no coverage percentage was retrievable this run, UNVERIFIED.

**Known limitations**: because capture is in-process (`sink()` + `withCallingHandlers`), a
script that calls `quit()`, crashes the R session, or corrupts global state before returning can
leave the log incomplete or unwritten (`axecute()` does explicitly `quit(status = 1)` itself
after a caught error in non-interactive mode, so it manages *its own* fatal exit, but a genuine
segfault or a script that calls `q()` internally bypasses the log-write step). No isolation from
the calling session's existing loaded packages/options (unlike whirl's fresh `callr` session).

## 2. whirl (Novo Nordisk Open Source)

- Repo: https://github.com/NovoNordisk-OpenSource/whirl
- CRAN: https://cran.r-project.org/package=whirl
- pkgdown site: https://novonordisk-opensource.github.io/whirl/

**Maintainer / org**: Aksel Thomsen (oath@novonordisk.com), maintainer; eight further
contributors (Lovemore Gakava, Cervan Girard, Kristian Trøjelsgaard, Steffen Falgreen Larsen,
Vladimir Obucina, Michael Svingel, Skander Mulder, Oliver Lundsgaard); copyright holder Novo
Nordisk A/S. Source: DESCRIPTION raw fetch.

**License**: Apache License (>= 2). Source: DESCRIPTION and CRAN index page, both fetched this
run.

**Last release**: CRAN version 0.3.2, published 2026-01-21; GitHub main branch DESCRIPTION shows
dev version 0.3.2.9003. 252 commits on main at time of fetch. Source: CRAN index page, GitHub
repo fetch, this run.

**What a log contains**: HTML report with script source, execution status (success/warning/
error), printed output, session info, package versions (via `renv::dependencies()` on both the
target script and whirl's own dummy Quarto document), start timestamp, and, Linux only, file
I/O tracking via `strace`. Also computes and stores an MD5 hash of the script
(`tools::md5sum()`). Source: `R/whirl_r_session.R` raw fetch (`wrs_log_script`, `wrs_create_log`).

**API**: `whirl::run(input, n_workers, steps, ...)` where `input` is one or more script paths,
a glob, or a YAML config file defining sequential/parallel batch steps (multi-script project
orchestration, not just single-script). Source: GitHub fetch + DESCRIPTION.

**Capture mechanism (verified from source)**: whirl builds a `whirl_r_session` R6 class that
**inherits from `callr::r_session`** (`R/whirl_r_session.R`), i.e. each script actually runs in
a genuinely separate, freshly spawned R subprocess, isolated from the calling session. Within
that subprocess, whirl does not use `sink()` itself; instead it drives
`quarto::quarto_render()` on an internal "dummy.qmd" Quarto document with `execute_params`
pointing at the target script, so **Quarto/knitr's own chunk-execution and output-capture
machinery** does the actual output capture, and whirl post-processes the rendered markdown plus
saved `.rds` metadata (script content, timings, package deps, session info, strace output where
enabled) into the final HTML/JSON/Markdown log via a second `quarto_render()` pass over
`log.qmd`. This is a materially different, heavier mechanism than logrx's in-process `sink()`.

**Wraps arbitrary scripts?** Yes: R, R Markdown, Quarto scripts, or Python scripts (via
`reticulate`, per Imports), individually or as a YAML-defined batch/pipeline. Broader scope than
logrx.

**File format**: primary output HTML; also supports GFM/CommonMark/Markua markdown and JSON
(`out_formats` option), each written per script plus a batch summary log. Source:
`wrs_create_logs()` in `R/whirl_r_session.R`.

**System requirements**: external Quarto CLI installation (`SystemRequirements: Quarto command
line tool` in DESCRIPTION); this is a genuine deployment dependency beyond CRAN's normal R
package install, and file-tracking via `strace` is Linux-only (confirmed via CRAN check results
page: strace tests are skipped on CRAN, and the file-tracking feature is documented as
Linux-only). Source: WebSearch of CRAN check results, this run, UNVERIFIED to the level of
reading the actual check log line by line, but consistent across two independent fetched
sources.

**Validation / pharmaverse status**: NOT part of the `pharmaverse` GitHub org; it is a
Novo Nordisk open-source package (own org `NovoNordisk-OpenSource`), referenced *by* the
pharmaverse examples/comparison page as a third option alongside logr and logrx, but not listed
as a pharmaverse-maintained package. UNVERIFIED whether it appears in any formal
pharma-consortium "validated package" list, no such badge or list was found this run.

**Known limitations**: hard runtime dependency on the external Quarto CLI (not just an R
package: a system binary must be installed and on PATH); file-access tracking works on Linux
only; heavier and slower than logrx or logr because every script execution is a subprocess plus
two Quarto renders; younger, smaller ecosystem footprint (252 commits vs logrx's 788, narrower
adoption evidence found this run).

## 3. logr (the "sassy" family)

- Repo: https://github.com/dbosak01/logr
- CRAN: https://cran.r-project.org/package=logr
- Site: https://logr.r-sassy.org

**Maintainer / org**: David Bosak (author/creator/maintainer), Rikard Isaksson (contributor).
Independent author, not a pharma-consortium org. Source: DESCRIPTION raw fetch.

**License**: CC0 (public-domain dedication). Source: DESCRIPTION and CRAN index, both fetched
this run.

**Last release**: CRAN version 1.3.9, published 2025-03-26 (GitHub master DESCRIPTION reports
1.4.0 as the dev/GitHub version; CRAN and GitHub have diverged, both figures are directly
fetched this run, not reconciled further).

**What a log contains**: whatever the programmer explicitly passes to `log_print()`, variables,
data frames, messages, plus an automatically generated header (log file name, working
directory, user, timestamp) and footer. Optional `autolog = TRUE` auto-logs dplyr/tidyr/sassy
verb calls. On error, a traceback is written; errors and the last warning always propagate to
the log; a **separate `.msg` file** is created only when errors/warnings occur, as a
filesystem-visible "something went wrong" flag for batch runs. Source: `R/logr.R` package-level
roxygen docs, raw fetch, this run.

**API**: three calls placed manually in the script body: `log_open(file_name, logdir, ...)` at
the top, `log_print(x)` as needed throughout, `log_close()` at the end. Global on/off via
`options(logr.on=)`. Source: `R/logr.R`, this run.

**Capture mechanism**: explicitly documented as "a wrapper to the base R `sink()` function",
confirmed by the package's own roxygen description ("This package provides the functionality of
`sink`, but in much more user-friendly way"). Unlike logrx, capture is not automatic/comprehensive:
the programmer decides what gets logged by calling `log_print()`; there is no equivalent of
logrx's blanket console/output capture around the whole script, so anything printed by a
sub-function without going through `log_print` (or an autolog-covered verb) is not captured.

**Wraps arbitrary scripts?** No: it instruments a script the programmer authors, rather than
wrapping/executing an arbitrary pre-existing script from the outside. This is the fundamental
API difference from logrx/whirl: logr is a library called *from inside* the script; logrx and
whirl are runners called *from outside* the script.

**File format**: plain-text `.log`, plus a plain-text `.msg` file only on error/warning.

**Validation / pharmaverse status**: Not a pharmaverse package; independent "sassy" family
(logr, fmtr, libr, procs) aimed at SAS-migrating analysts. Referenced by the pharmaverse
comparison page as the "manual control" alternative to logrx/whirl, not as a competitor within
pharmaverse governance.

**Known limitations**: manual instrumentation means completeness depends entirely on programmer
discipline: a log_open/log_print/log_close pattern that a reviewer must trust was applied
consistently; no automatic session-info/package-hash/file-hash capture equivalent to
logrx/whirl unless the programmer explicitly logs it; no subprocess isolation.

## 4. General-purpose loggers (contrast only)

- **futile.logger** (https://cran.r-project.org/package=futile.logger): log4j-style leveled
  logging with package-scoped namespaces. Built for ongoing application/service logging
  (DEBUG/INFO/WARN/ERROR streams), not for producing a single deterministic execution record of
  one script run: no session info, no package hashes, no file capture, no concept of "the log
  of this run" as a regulatory artefact. Wrong tool for a trial-programming audit record.
- **logger** (https://github.com/daroczig/logger): a lighter, more modern futile.logger-alike
  with pluggable appenders/layouts. Same category problem: a flexible message-severity stream,
  not an execution-provenance record; would need substantial custom wiring (session info,
  hashing, output capture) to reach parity with logrx, at which point you have rebuilt logrx.
- **lgr** (R6-based, log4j-inspired): same category again, structured, leveled, appender-based
  logging for long-running processes/apps. None of the three general loggers capture
  console/warning/error output automatically, session info, or file hashes by default; all three
  exist to let a programmer *emit* structured messages, not to produce a self-contained
  reproducibility record of a script execution. Not competitive with logrx/whirl/logr for the
  bctu use case; not investigated further at source level (contrast paragraph only, per brief).

## 5. Other candidates considered

- **tidylog** (https://github.com/elbersb/tidylog): confirmed (README fetch) to be a dplyr/tidyr
  verb-wrapper that prints row-count/column-change feedback per pipeline step. It is a data-step
  narrator, not an execution/session logger: no session info, no file hashing, no error
  capture, no log file of its own. Out of scope for "execution logging" proper; noted only
  because it is sometimes grouped with logr's `autolog` feature (logr's autolog literally
  instruments the same dplyr/tidyr verbs tidylog does).
- **timber**: searched for, no R package of this name matching purpose-built clinical execution
  logging was found this run, UNVERIFIED / not located.
- No other purpose-built clinical-execution-logging R package was found beyond logrx, whirl, and
  logr; the pharmaverse examples site (https://pharmaverse.github.io/examples/logging/logging.html)
  itself treats these three as the complete comparison set as of this run.

## Comparison table

| | logrx | whirl | logr |
|---|---|---|---|
| Org | pharmaverse (GSK/Atorus funded) | Novo Nordisk Open Source | independent (dbosak01) |
| License | MIT + file LICENSE | Apache-2.0 | CC0 |
| Latest CRAN release | 0.4.0 (2025-05-05) | 0.3.2 (2026-01-21) | 1.3.9 (2025-03-26) |
| Wraps arbitrary script? | yes, one file per call | yes, single or batch/YAML | no, instruments script from inside |
| Capture mechanism | in-process `sink()` + `withCallingHandlers` | subprocess (`callr::r_session`) + Quarto render | in-process `sink()` wrapper |
| Session/package info | full `sessioninfo::session_info()` | `renv::dependencies()` + session info | not automatic (autolog only for dplyr/tidyr) |
| Script hash | yes (`digest`) | yes (`tools::md5sum()`) | no |
| File I/O tracking | no | yes, Linux only (`strace`) | no |
| External system dependency | none | Quarto CLI required | none |
| Log format | plain text `.log` (+ optional `.rds`) | HTML (+ optional MD/JSON) | plain text `.log` (+ `.msg` on error) |
| Batch/multi-script orchestration | no | yes (native) | no |
| Automatic completeness | high (blanket capture) | high (blanket capture, isolated) | low (manual, programmer-controlled) |

## Assessment for bctu

**License compatibility**: bctu's own `LICENSE` file states "Copyright BCTU. Internal use.",
not an OSI-approved license, i.e. bctu is an internal/proprietary-use package, not one bctu is
redistributing under an open license. MIT (logrx), Apache-2.0 (whirl), and CC0 (logr) are all
maximally permissive; none impose copyleft or attribution obligations that would be incompatible
with an internal-use `Imports`/`Suggests` dependency. No license blocker to depending on any of
the three. (Apache-2.0 carries a patent grant clause and a NOTICE-file propagation requirement
if bctu ever redistributes whirl's source: irrelevant for a plain `Imports` dependency, relevant
only if bctu vendors whirl's code.)

**Which to depend on, vendor, imitate, or ignore**:

- **logrx**: the strongest fit if bctu wants "wrap an existing analysis script and get a
  regulatory-grade log" as a bolt-on feature. It is a lightweight `Imports` (no external
  binaries), pharmaverse-governed (GSK/Atorus-funded, more mature commit/star history than
  whirl), and its API (`axecute()`) is a single obvious call consistent with bctu's
  "one-obvious-call happy path" convention. The robustness concern is real: in-process `sink()`
  capture means a script that hard-crashes R produces no log; bctu would need to document that
  gap (or wrap `axecute()` itself in a subprocess) if it depends on logrx for anything
  audit-critical. Verdict: reasonable to **depend on** for a specific documented use case (log a
  single script run), not to imitate wholesale.
- **whirl**: technically the most robust design (subprocess isolation, file-hash, session info,
  batch orchestration) but the Quarto CLI system dependency is a genuine deployment cost for
  non-R-expert BCTU users who are not necessarily running full Quarto/TeX toolchains, and the
  Linux-only strace file-tracking means Windows users (BCTU statisticians commonly are) get a
  degraded log silently. Given bctu's audience (occasional R users, mixed OS), adding a Quarto
  dependency purely for logging is disproportionate. Verdict: **ignore as a dependency**; its
  subprocess-isolation *idea* is worth imitating in bctu's own audit-ledger code (bctu already
  keeps an append-only YAML audit ledger per the project CLAUDE.md, so the mechanism, not the
  package, is the useful borrow) if bctu ever needs to log arbitrary external script runs.
- **logr**: manual, SAS-style, programmer-driven. Doesn't match bctu's stated principle
  ("explicit configuration, no ambient state" is fine, but bctu wants automatic, tamper-evident
  audit records, not "trust the programmer called log_print enough times"). Verdict: **ignore**;
  wrong completeness model for an audit ledger, though its plain-text log format and `.msg`
  error-flag idea are cheap and could be imitated for a human-readable status file convention.
- **General loggers (futile.logger/logger/lgr)**: **ignore** entirely for this purpose; they
  solve a different problem (application-level leveled logging), not single-run provenance
  capture.

**Strongest robustness concerns** (falsification pass): (1) logrx's in-process capture is a
single point of failure against R crashing or a script calling `q()`/`quit()` mid-run: a
malicious or buggy script can suppress its own log; this is the sharpest objection against
depending on logrx for anything where an adversarial or badly-behaved script must be assumed.
(2) whirl's two-stage Quarto-render architecture (render the script, then render the log)
means a Quarto/pandoc version mismatch between environments could change log formatting or fail
outright, which is a bad property for a "reproducible record" whose own generation isn't fully
self-contained in R. (3) None of the three packages produce a cryptographically chained/
append-only ledger (bctu's own audit ledger already goes further than any of them by that
measure): none is a drop-in replacement for bctu's existing audit-ledger design; at best each
is a component for logging a single script's execution, not the ledger itself.
