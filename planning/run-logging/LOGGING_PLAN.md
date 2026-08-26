# Run logging for bctu: plan (not yet implemented)

Status: IMPLEMENTED in bctu 0.18.0 (2026-08-18, commit e26f361): phases 1 to 3
(record core, `start_log()`/`stop_log()`, `checkpoint()` integration,
`list_logs()`/`read_log()`), with one design change forced by R: the
handlers-on-stack limitation is handled by stack inspection with graceful
degradation (see `handler_frames_on_stack()`), not by deferred registration
(task-callback registration of global handlers proved silently ineffective).
Still open: phase 4 (orchestrator adoption, Q4), phase 5 (vignette), the
deferred script-runner, and questions Q2/Q3.

Date: 2026-08-18. Research basis: two retrieval-based reports in this folder,
`APPENDIX_regulatory_requirements.md` (regulatory sources, pinpoint-cited) and
`APPENDIX_prior_art.md` (R package survey, source-level verification). Every
regulatory claim below cites those appendices; nothing is from memory.

## 1. Purpose

When a statistical program runs (a scheduled pipeline, a DVR build, an ad hoc
analysis script), the run should leave a complete, contemporaneous,
tamper-evident record on disc as part of the run history: who ran what, when,
on which data, with which software, producing which outputs, with what
warnings and errors. This is the SAS `proc printto` / Stata `log using` working
pattern, upgraded to meet the regulatory expectations below. The intended flow,
as specified by Jack:

1. start the log;
2. checkpoint: timestamp everything with all of the information;
3. run the code;
4. close it all off, and the record becomes part of the run history.

Primary usage is INTERACTIVE sessions (Jack, 2026-08-18): BCTU statisticians
work in a live R session, not by wrapping scripts from the outside. The design
is therefore interactive-first; a script-runner shape is a possible later
phase, not v1. The package also already exports `checkpoint()` (a provenance
stamp embedded in snapshot manifests); this feature reuses it rather than
adding a near-duplicate verb.

The feature must be easy and obvious for occasional R users, tie in with the
bctu package, and also work standalone (usable outside a bctu project; nothing
proprietary about the record format).

## 2. What the regulators require (distilled)

Full sourcing: `APPENDIX_regulatory_requirements.md`. The load-bearing points:

| # | Requirement | Source (pinpoint) |
|---|---|---|
| R1 | Record who acted, when, and where applicable why | 21 CFR 11.10(e); ICH E6(R3) glossary "Audit trail"; EMA 2023 s.6.2.1; MHRA 2018 s.6.13 |
| R2 | Records secure, computer-generated, time-stamped; never obscure or overwrite prior entries | 21 CFR 11.10(e); ICH E6(R3) 4.2.2(b); EMA s.6.2.1; MHRA s.6.13 |
| R3 | Statistical analysis of patient data IS "data processing"; retained records must allow reconstruction of ALL processing activities, reported or not | MHRA s.6.9 (the single most on-point clause found) |
| R4 | User-defined parameters of a run traceable to raw data, with attribution | MHRA s.6.9 |
| R5 | Repeated or modified re-runs must remain visible, never silently overwritten, so parameters cannot be tuned toward a favourable result | MHRA s.6.9 |
| R6 | Unambiguous timestamps, UTC named explicitly | ICH E6(R3) 4.2.2(d) |
| R7 | Human-readable records the investigator, sponsor and inspector can review | EMA s.6.2.1 |
| R8 | Retention at least as long as the underlying record; retrievable throughout | 21 CFR 11.10(e); ICH E6(R3) 4.2.7; MHRA 6.11.2 |
| R9 | ALCOA(+/++) is the quality frame: attributable, legible, contemporaneous, original, accurate, complete, consistent, enduring, available, traceable | FDA DI Q&A Q1a; MHRA definitions; EMA s.4.5 |
| R10 | Software name, version and OS documented for submitted analyses (ADRG); program source submitted as ASCII text | FDA Study Data Technical Conformance Guide s.4.1.2.10 |
| R11 | Risk-based validation of the system itself | ICH E6(R3) 4.3.4; 21 CFR 11.10(a) |

Honest gaps in the retrieval (flagged, not filled from memory): ICH E9 was not
retrieved this run; no PHUSE white paper on log checking could be fetched (DNS
failure for lexjansen.com); ICH E6(R3) contains no explicit "reproducibility"
requirement for statistical programs (verified by full-text grep). The
execution log as a named artefact is industry GxP practice (whirl README,
pharmaverse comparison page) rather than a verbatim regulatory requirement;
what the regulations require is the reconstructability and attribution that
such a log provides (R3, R4, R5).

Design consequences: the run record must capture user, UTC timestamps, inputs
(data identity, ideally hashes), parameters, software versions and OS, outputs
with hashes, and errors/warnings (R1, R4, R9, R10). It must be append-only and
collision-suffixed, never overwritten (R2, R5; bctu's DVR `_N` convention
already follows this). It must be plain text (R7; bctu convention: metadata is
YAML, never binary). And a hard program crash must still leave a record, or at
minimum never leave a silently absent one (R3: reconstruction of ALL activities).

## 3. Prior art and the adopt-vs-build decision

Full survey: `APPENDIX_prior_art.md` (three purpose-built packages read at
source level). Summary:

| | logrx (pharmaverse) | whirl (Novo Nordisk) | logr (sassy) |
|---|---|---|---|
| Model | wrap a script from outside, one call | batch runner, subprocess + Quarto | manual log_open/log_print/log_close inside the script |
| Capture | in-process sink() + handlers | callr subprocess + Quarto render | in-process sink(), programmer-driven |
| Session info, hashes | yes (sessioninfo, digest) | yes (+ Linux-only strace file tracking) | no |
| External deps | none | Quarto CLI binary | none |
| License | MIT | Apache-2.0 | CC0 |
| CRAN | 0.4.0, 2025-05-05 | 0.3.2, 2026-01-21 | 1.3.9, 2025-03-26 |

Decision, per Jack's directives (do not reinvent the wheel where a stable,
robust, justified package exists; but primary usage is interactive sessions):

**None of the three packages fits the interactive-first shape, so v1 is a
small bctu-native record layer built on primitives the package already has; a
logrx dependency enters only if the later script-runner phase is wanted.**
Justification trail:

- logrx is the right wheel for wrapping a SCRIPT FILE from outside
  (`axecute("file.R")`), and remains the pick for that shape if it is ever
  built. It has no interactive mode: its capture machinery is built around
  executing a file's parsed expressions, so it cannot serve a live session.
- whirl is a batch runner (subprocess + Quarto CLI, Linux-only file tracking);
  even further from interactive use, and disproportionate as a dependency. Its
  subprocess-isolation idea is only relevant to the later runner phase.
- logr IS interactive-shaped (log_open/log_print/log_close in a live session)
  and is the ergonomic model to imitate, but its completeness depends on the
  programmer calling log_print (fails R3), and it captures no session info or
  hashes. Borrowed: the open/close ergonomics and the `.msg` sidecar that
  exists only when a run had errors or warnings. Not depended on: what bctu
  needs beyond sink() management is exactly the part logr lacks.
- The record layer itself is small because bctu already has the primitives:
  `checkpoint()` (user/host/versions/UTC stamp), `snapshot_fingerprint()`,
  `sha256_file()`, `iso8601()`/`utc_now()`, and YAML manifest writing. Adding
  a transcript sink plus a run.yml around them is less code than an adapter
  for a package that fits at an angle. This is not wheel-reinvention: for the
  interactive shape there is no wheel (the survey's conclusion, appendix
  section "Assessment"), and where one exists (script running, logrx) the plan
  defers to it.

## 4. User-facing design (API first)

Naming follows the package rule: clear verbs a non-R user can guess. TWO
functions, one record format, and the existing `checkpoint()` reused rather
than duplicated. No logrx dependency in v1.

### 4.1 The interactive happy path (the primary shape)

Naming per Jack (2026-08-18): `start_log()` / `stop_log()`.

```r
bctu::start_log()                  # defaults are enough for the common case
# ... the work, exactly as today ...
bctu::checkpoint()                 # any time: stamp goes to console AND log
# ... more work ...
bctu::stop_log()
```

Decisions adopted (Jack, 2026-08-18):

- `start_log(location = NULL, name = NULL, snapshot = NULL, params = NULL,
  inputs = NULL)`. `location` defaults to `<project dir>/log-output` when a
  bctu project marker resolves; outside a project it must be given (error in
  plain English naming the argument). `name` defaults to the running script's
  file name when the session is a script (`Rscript`/`R -f`, detected from
  `commandArgs()`), else `interactive-session`; the run id is
  `<name>-<iso8601>`.
- One time source: the run id's timestamp IS the initial checkpoint's
  `created_utc` (the header embeds that same stamp; no second clock read), in
  the filesystem-safe UTC form the snapshot ids already use.
- `checkpoint()` always writes its stamp to the open log (transcript block
  plus a structured `checkpoints:` entry in `run.yml`) whenever a log is
  open; its return value, its use inside snapshot manifests, and every other
  behaviour are unchanged.
- `checkpoint()` with NO open log, in an INTERACTIVE session, auto-starts one
  with the defaults above and announces it; so the lazy path into a recorded
  session is just calling `checkpoint()`. In non-interactive use and in
  internal package calls (`take_snapshot()` embeds `checkpoint()` in every
  manifest) it remains a pure stamp and never creates log directories as a
  side effect. CONFIRM: this interactive-only guard is my reading of "start a
  new session if one isn't started" against "no other behaviour needs to
  change"; without it, every pipeline snapshot would silently open a log.

Hard invariant: NOTHING the user sees in the interactive session changes.
Same prompts, same command echo, same printed output, same warnings and
errors in the same places. The only visible additions are one announced line
at start (the resolved record path) and one at close (the record location).

- `start_log()` (with the signature in 4.1) does, in order: (1) writes the `run.yml` header FIRST, state
  `started` (crash survivorship, 4.4): `checkpoint()` stamp, session/package
  info, snapshot identity (id, tag, SHA-256 via `snapshot_fingerprint()`),
  `inputs` hashes, `params` verbatim (R4); (2) opens the transcript with
  `sink(con, split = TRUE)`, which DUPLICATES stdout: console unchanged, copy
  to `run.log`; (3) registers `globalCallingHandlers()` for messages,
  warnings and errors, which observe WITHOUT muffling: each condition still
  reaches the console exactly as before, while its text is appended to the
  transcript and tallied. The message sink (`sink(type = "message")`) is
  explicitly ruled out: base R cannot split the message stream, so it would
  divert warnings off the console and break the invariant; (4) registers a
  task callback appending each top-level command (deparsed) to the
  transcript, giving the Stata-style commands-plus-output log; callbacks are
  invisible to the session. Returns the run-log handle.
- `stop_log(outputs = NULL)` removes the handlers and callback and
  closes the sink (console behaviour was never altered, so nothing visibly
  changes back), then records end time, elapsed, status (clean / warnings /
  errors / interrupted) from the tallies, an inventory with SHA-256 of
  `outputs` files or directories when given, the SHA-256 of `run.log` itself,
  finalises `run.yml`, appends the index line, and writes the `.msg` sidecar
  if the run was not clean.
- Honest limits, documented in the vignette: input that is not a top-level
  expression (e.g. `readline()` responses) and graphics are not captured;
  this is a transcript of commands, output and conditions, like Stata's
  `log using`, not a screen recording.

Mid-run stamps need no third verb: `checkpoint()` already exists, and calling
it (printed) lands in the transcript with everything it stamps; when a run log
is open, its timing block in `run.yml` is simply start and end. If experience
shows a structured mid-run stamp is genuinely needed, `checkpoint()` can later
gain an optional `log =` argument; that is deferred, not designed now
(parsimony, and Jack's instruction not to duplicate checkpoints).

- Two open logs at once is a loud error naming the open one; the handle is
  explicit everywhere (no ambient state). `stop_log()` with no open log
  errors in plain English.

### 4.2 Later, optional: the script-runner shape (NOT v1)

If the scheduled pipelines ever want an R-level wrapper (they already have
bash-level logs), `run_with_log("script.R")` = header-first parent + callr
child + `logrx::axecute()` in the child (logrx as Suggests). Deferred until
wanted; recorded here so the earlier research is not lost.

### 4.3 Reading the history back

```r
bctu::list_logs("log-output")        # one row per run: id, label, user, start, status
bctu::read_log("log-output", "interactive-session-2026-08-18T070018Z")
```

Plain data frame out; no special print magic required for v1.

### 4.4 Crash survivorship

`start_log()` writes the header record (state `started`) and begins the
transcript BEFORE any work runs, and the sink flushes line by line, so even a
session killed outright leaves the header, the transcript up to death, and an
index entry that never reached `closed`; the absence of a close is itself
evidence (R3). Where R allows a handler to run (error, quit, session end), a
registered finaliser stamps `status: interrupted` into `run.yml`. Documented
honestly in the vignette: an in-process record cannot beat kill -9 beyond the
header-plus-partial-transcript guarantee; if that ever matters operationally,
the deferred runner shape (4.2) is the answer.

### 4.5 The record set on disc (one directory per run)

```
log-output/
  index.csv                          # append-only, one line per run
  interactive-session-2026-08-18T070018Z/
    run.log                          # full console transcript (plain text)
    run.yml                          # the structured record (YAML, human-readable)
    run.msg                          # present ONLY if warnings/errors/crash (logr borrow)
```

- Run id = UTC timestamp + slug of the label; a same-second collision gets the
  existing `-NN` suffix convention. Nothing is ever overwritten (R2, R5): a
  re-run of the same label is a new directory, and `index.csv` keeps both
  visible in order.
- `run.yml` fields: schema id (`bctu-runlog/1`), run id, label, operator, host,
  OS, R version, bctu and logrx versions, UTC start/end, status, params,
  checkpoints (list), inputs (snapshot ids, tags, fingerprints; file hashes),
  outputs (path, size, SHA-256), warnings/errors (count + first lines), and
  the SHA-256 of `run.log` itself, so transcript tampering is detectable
  (tamper-evidence within the means of a filesystem record; true
  access-control security is the filing system's job and is documented as
  such, not claimed).
- Everything plain text (R7; house rule: metadata is YAML, never binary).

### 4.6 bctu tie-in and standalone use

- In a bctu project (bctu-project.yml found), `log_dir` defaults to a declared
  `run_logs:` key if present; otherwise it must be given explicitly and the
  resolved path is announced. No silent working-directory fallback.
- `take_snapshot()`, `save_dvr()`, `save_cdi()`, `render_report()` gain nothing
  in v1; they are simply run UNDER a log. A later phase may let them notice an
  open run log and cross-reference their manifests into it (open question Q3).
- Standalone: no project marker, no keyring, no snapshot needed; `inputs` can
  be plain file paths. The record format carries no bctu-only semantics, so
  another unit or package could adopt it. Not proprietary.

## 5. What we deliberately do NOT build

- No leveled DEBUG/INFO logging framework (futile.logger territory); wrong
  problem.
- No Quarto/HTML rendering of logs (whirl territory); plain text is the
  inspectable artefact (R7). A pretty viewer can come later, read-only.
- No cryptographic signature chain in v1; SHA-256 of transcript and outputs in
  a YAML the filing system protects is proportionate (R11 risk-based), and the
  existing snapshot audit ledger remains the ledger of record for data events.
- No `log_checkpoint()` or any second checkpoint verb: the existing
  `checkpoint()` is the stamping primitive and the transcript captures its
  printed output; extending it with a `log =` argument is explicitly deferred.
- No logrx (or any new) dependency in v1: the interactive record layer is
  built on primitives bctu already has; logrx enters only with the deferred
  script-runner shape, as Suggests.
- No script-runner in v1 (section 4.2): interactive-first, per Jack.

## 6. Implementation phases (after approval only)

1. **Core record**: run id, record directory, `run.yml` writer (schema
   `bctu-runlog/1`), append-only `index.csv`, collision suffixing. Pure
   functions on existing bctu primitives, no new dependency.
2. **Interactive pair**: `start_log()` and `stop_log()`; header-first
   discipline; split sink management; `checkpoint()` embedded in the header;
   `.msg` sidecar; interrupted-state finaliser. (Depends on 1.)
3. **History readers**: `list_logs()`, `read_log()`.
4. **Orchestrator adoption**: the three trial run_all_linux.R scripts wrap
   their work in `start_log()`/`stop_log()` (inline, no runner
   needed); the bash-level logs in run_scheduled.sh stay (they capture
   docker/mail plumbing), the R-level run record becomes the regulatory
   artefact in the trial folder.
5. **Docs**: vignette "Recording an analysis run" written for the
   non-R-expert audience; NEWS; roxygen with plain-English errors.
6. **Deferred, only if wanted later**: `run_with_log()` script runner on
   logrx (Suggests), per section 4.2.

Each phase lands as its own gated, tagged patch release per house rules
(pkg-gate clean before any bump; one coherent commit per phase).

## 7. Validation and testing plan

- Unit tests per phase; the crash tests are the load-bearing ones: a child R
  session (test harness via callr) that opens a run log and is killed mid-run
  must leave the `started` header, the partial transcript, and no `closed`
  state; a session that errors then closes must record `status: errors`; a
  second run with an identical label must produce a suffixed directory and
  BOTH index lines (R5 test).
- A round-trip test: run known code under a log, then reconstruct from
  `run.yml` alone what ran, on what inputs, producing which outputs, and
  verify the recorded SHA-256s against the files (R3 in miniature).
- Requirements trace: a test file mapping R1..R11 to the tests that exercise
  them, kept next to the tests, so the risk-based validation story (R11) is a
  document we can hand an inspector rather than an assertion.
- Sink hygiene tests: nested sinks (a user's own sink() inside a run),
  close-without-open, open-twice, and console restoration after every exit
  path; sink bugs are the classic failure of this design and get their own
  test file.

## 8. Open questions for Jack (blocking implementation)

- Q1 RESOLVED (2026-08-18): `start_log()` / `stop_log()`; location defaults
  to `<project dir>/log-output`; names default to script name or
  `interactive-session` plus the initial checkpoint's iso8601 stamp.
- Q2 Should `log-output/` be added to the standard trial structure
  (trial-structure module), and should `bctu-project.yml` support an optional
  override key for it?
- Q3 Should snapshot/DVR/report calls auto-attach their manifests to an open
  run log (later phase), or is being captured in the transcript enough?
- Q4 Do the trial orchestrators adopt the pair in the same release (phase 4
  now), or after a bedding-in period on one trial?
