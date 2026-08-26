---
title: Regulatory requirements for execution logs / run records of statistical programs (CTIMPs)
---

Retrieval method note: every quote below was extracted from the primary document itself, either
via WebFetch (rendered readable text) or, where WebFetch could not parse the PDF, by downloading
the PDF with curl and converting it locally with pdftotext, then grepping/reading the extracted
text. Local text extracts are kept at
`/tmp/claude-1000/-home-jack-projects-bctu-ng-bctu/936fbae8-43b7-4e0a-81f5-a9bcdd0816a6/scratchpad/pdfs/`
for re-verification.

## 1. FDA 21 CFR Part 11 (electronic records; audit trail provisions)

- Title: 21 CFR Part 11, Electronic Records; Electronic Signatures
- URL: https://www.ecfr.gov/current/title-21/chapter-I/subchapter-A/part-11 (retrieved via eCFR
  Versioner API, https://www.ecfr.gov/api/versioner/v1/full/2026-08-14/title-21.xml?part=11)
- Retrieval status: RETRIEVED (eCFR web page itself blocked automated WebFetch with a
  redirect/anti-bot page; full regulatory text obtained instead from the eCFR public XML API,
  same governmental source, current as of 2026-08-14)

Key text, Subpart B, Section 11.10 "Controls for closed systems":

> "(e) Use of secure, computer-generated, time-stamped audit trails to independently record the
> date and time of operator entries and actions that create, modify, or delete electronic
> records. Record changes shall not obscure previously recorded information. Such audit trail
> documentation shall be retained for a period at least as long as that required for the subject
> electronic records and shall be available for agency review and copying."

Also relevant:

> "(a) Validation of systems to ensure accuracy, reliability, consistent intended performance,
> and the ability to discern invalid or altered records."

> "(c) Protection of records to enable their accurate and ready retrieval throughout the records
> retention period."

> "(k)(2) Revision and change control procedures to maintain an audit trail that documents
> time-sequenced development and modification of systems documentation."

Companion industry guidance, retrieved and read: **FDA Guidance for Industry, "Part 11,
Electronic Records; Electronic Signatures - Scope and Application"** (2003).
URL: https://www.fda.gov/media/75414/download. Retrieval status: RETRIEVED (curl download,
pdftotext).

> "The Agency intends to exercise enforcement discretion regarding specific part 11 requirements
> related to computer-generated, time-stamped audit trails (§ 11.10(e), (k)(2) and any
> corresponding requirement in § 11.30). Persons must still comply with all applicable predicate
> rule requirements related to documentation of, for example, date ... time, or sequencing of
> events, as well as any requirements for ensuring that changes to records do not obscure
> previous entries." (p.6)

This narrows blanket "audit trail always required" readings of Part 11: FDA does not enforce
11.10(e)/(k)(2) as such, but a predicate rule (e.g. a CGMP record-keeping requirement) can still
mandate an equivalent contemporaneous, non-obscuring record of who/when/what.

## 2. FDA "Data Integrity and Compliance With Drug CGMP: Questions and Answers"

- Title: Data Integrity and Compliance With Drug CGMP: Questions and Answers, Guidance for
  Industry
- Date: final guidance (December 2018 per FDA's guidance database)
- URL: https://www.fda.gov/media/119267/download
- Retrieval status: RETRIEVED (curl download, pdftotext; WebFetch could not render the PDF's
  binary stream directly, so curl + pdftotext was used as the retrieval path)

Q1a, "What is data integrity?":

> "Data integrity refers to the completeness, consistency, and accuracy of data. Complete,
> consistent, and accurate data should be attributable, legible, contemporaneously recorded,
> original or a true copy, and accurate (ALCOA)."

> "Data integrity is critical throughout the CGMP data life cycle, including in the creation,
> modification, processing, maintenance, archival, retrieval, transmission, and disposition of
> data after the record's retention period ends."

Q1b, "What is metadata?":

> "Metadata for a particular piece of data could include a date/time stamp documenting when the
> data were acquired, a user ID of the person who conducted the test or analysis that generated
> the data, the instrument ID used to acquire the data, material status data, the material
> identification number, and audit trails."

Q1c, "What is an audit trail?":

> "Audit trail means a secure, computer-generated, time-stamped electronic record that allows for
> reconstruction of the course of events relating to the creation, modification, or deletion of
> an electronic record. For example, the audit trail for a high performance liquid chromatography
> (HPLC) run should include the user name, date/time of the run, the integration parameters used,
> and details of a reprocessing, if any. Documentation should include change justification for
> the reprocessing."

Q7/Q8, audit trail review:

> "Personnel responsible for record review under CGMP should review the audit trails that capture
> changes to ... production and control records ..." and review frequency should be set "using
> knowledge of your processes and risk assessment."

Note: this guidance is CGMP-specific (drug manufacturing), not CTIMP-analysis-specific, but it is
FDA's canonical definition source for ALCOA / audit trail / data integrity terminology that is
carried into GCP-context documents (e.g. the MHRA and EMA guidances below cite the same
construct).

## 3. ICH E6(R3) Good Clinical Practice (2025, in force)

- Title: ICH Harmonised Guideline, Good Clinical Practice (GCP) E6(R3)
- Date: Step 4 version, 23 January 2025; reference EMA/CHMP/ICH/135/1995
- URL: https://www.ema.europa.eu/en/documents/scientific-guideline/ich-e6-r3-guideline-good-clinical-practice-gcp-step-5_en.pdf
- Retrieval status: RETRIEVED (WebFetch returned the file as opaque binary; the PDF was still
  saved by the tool and converted locally with pdftotext, which produced fully readable text)

Section 4.2.2, "Relevant metadata, including audit trails":

> "(a)(ii) Systems are designed to permit data changes in such a way that the initial data entry
> and any subsequent changes or deletions are documented, including, where appropriate, the
> reason for the change;"

> "(b) Ensuring that audit trails, reports and logs are not disabled. Audit trails should not be
> modified except in rare circumstances ... and only if a log of such action and justification is
> maintained;"

> "(d) Ensuring that the automatic capture of date and time of data entries or transfer are
> unambiguous (e.g., coordinated universal time (UTC));"

Section 4.2.4, "Data corrections":

> "Corrections should be attributed to the person or computerised system making the correction,
> justified and supported by source records around the time of original entry and performed in a
> timely manner."

Section 4.2.6, "Finalisation of data sets prior to analysis":

> "(c) Data extraction and determination of data analysis sets should take place in accordance
> with the planned statistical analysis and should be documented."

Section 4.3, "Computerised systems" (4.3.1 procedures, 4.3.3 security, 4.3.4 validation):

> "4.3.4(a) The responsible party is responsible for the validation status of the system
> throughout its life cycle. The approach to validation of computerised systems should be based
> on a risk assessment ..."

Glossary definition of "Audit trail":

> "Metadata records that allow the appropriate evaluation of the course of events by capturing
> details on actions (manual or automated) performed relating to information and data collection
> and, where applicable, to activities in computerised systems. The audit trail should show
> activities, initial entry and changes to data fields or records, by whom, when and, where
> applicable, why. In computerised systems, the audit trail should be secure, computer-generated
> and time stamped."

No hit for "reproducib*" anywhere in the document (checked by full-text grep); E6(R3) does not
use the word "reproducibility" in relation to statistical analyses. The closest content is
4.2.6(c) above (documentation of the analysis-set derivation process) and the general data
governance/audit trail machinery, which support reconstruction of how a dataset was produced but
is not phrased as a reproducibility requirement for statistical programs specifically.

## 4. EMA Guideline on computerised systems and electronic data in clinical trials (2023)

- Title: Guideline on computerised systems and electronic data in clinical trials
- Date: 9 March 2023 (reference EMA/INS/GCP/112288/2023, GCP Inspectors Working Group)
- URL: https://www.ema.europa.eu/en/documents/regulatory-procedural-guideline/guideline-computerised-systems-and-electronic-data-clinical-trials_en.pdf
- Retrieval status: RETRIEVED (same pattern as above: curl-equivalent download by the fetch tool,
  local pdftotext conversion)

Section 6.2.1, "Audit trail":

> "An audit trail should be enabled for the original creation and subsequent modification of all
> electronic data. In computerised systems, the audit trail should be secure, computer generated
> and timestamped."

> "Entries in the audit trail should be protected against change, deletion, and access
> modification (e.g. edit rights, visibility rights). The audit trail should be stored within the
> system itself. The responsible investigator, sponsor, and inspector should be able to review and
> comprehend the audit trail and therefore audit trails should be in a human-readable format."

> "The audit trail should show the initial entry and the changes (value - previous and current)
> specifying what was changed (field, data identifiers) by whom (username, role, organisation),
> when (date/timestamp) and, where applicable, why (reason for change)."

Section 4.5, "ALCOA++ principles":

> "ALCOA++: attributable, legible, contemporaneous, original, accurate, complete, [consistent,
> enduring, available, traceable]" (definitions table, line 269; full spelled-out list of the
> "++" elements confirmed against the definitions table entry)

> "All data collected or generated in the context of a clinical trial should fulfil ALCOA++
> principles." (section 4.5 opening line)

Retention (multiple sections, e.g. 6.8/6.9 referenced internally):

> "The investigator and sponsor should be aware of the required retention periods for clinical
> trial data and essential documents, including metadata. Retention periods should respect the
> data protection principle of storage limitation."

Validation:

> "Validation ... The approach to validation should be based on a risk assessment that takes into
> consideration [intended use, criticality]." (section on Validation, line ~254-257)

This guideline is the most detailed of the six sources on the mechanics of an audit trail (who,
what field, before/after value, when, why) but is written for clinical-data-capture systems (EDC,
eCOA, etc.), not statistical-analysis program execution specifically; it is nonetheless the
closest EU regulatory articulation of "what a change/action record must capture."

## 5. MHRA 'GXP' Data Integrity Guidance and Definitions (2018)

- Title: 'GXP' Data Integrity Guidance and Definitions, Revision 1
- Date: March 2018
- URL: https://assets.publishing.service.gov.uk/media/5aa2b9ede5274a3e391e37f3/MHRA_GxP_data_integrity_guide_March_edited_Final.pdf
- Retrieval status: RETRIEVED (same pattern: fetch tool saved binary PDF, pdftotext conversion
  used locally for verbatim text)

Section 6.9, "Data Processing" (defines "data processing" as including *"statistical analysis of
individual patient data to present trends"*, directly on point for statistical programs):

> "A sequence of operations performed on data to extract, present or obtain information in a
> defined format. Examples might include: statistical analysis of individual patient data to
> present trends or conversion of a raw electronic signal to a chromatogram and subsequently a
> calculated numerical result"

> "There should be adequate traceability of any user-defined parameters used within data
> processing activities to the raw data, including attribution to who performed the activity."

> "Audit trails and retained records should allow reconstruction of all data processing
> activities regardless of whether the output of that processing is subsequently reported or
> otherwise used for regulatory or business purposes. If data processing has been repeated with
> progressive modification of processing parameters this should be visible to ensure that the
> processing parameters are not being manipulated to achieve a more desirable result."

Section 6.11.2, "True copy" (bears on retaining run outputs and reconstructibility of a data
processing run):

> "the data retention process must be shown to include verified copies of all raw data, metadata,
> relevant audit trail and result files, any variable software/system configuration settings
> specific to each record, and all data processing runs (including methods and audit trails)
> necessary for reconstruction of a given raw data set."

Section 6.13, "Audit Trail":

> "The audit trail is a form of metadata containing information associated with actions that
> relate to the creation, modification or deletion of GXP records. ... An audit trail facilitates
> the reconstruction of the history of such events relating to the record regardless of its
> medium, including the 'who, what, when and why' of the action."

> "It should be possible to associate all data and changes to data with the persons making those
> changes, and changes should be dated and time stamped (time and time zone where applicable). The
> reason for any change, should also be recorded."

Definitions table (ALCOA):

> "ALCOA: Acronym referring to Attributable, Legible, Contemporaneous, Original and Accurate."
> "ALCOA+: Acronym referring to Attributable, Legible, Contemporaneous, Original and Accurate
> 'plus' Complete, Consistent, Enduring, and Available."

MHRA notes it deliberately uses "ALCOA" not "ALCOA+" in its own running text (section 3.10), on
the basis that the "+" attributes were "historically regarded as" implicit in ALCOA already; the
guidance nonetheless defines both terms.

This is the single most directly relevant source of the six: section 6.9 explicitly names
"statistical analysis of individual patient data" as an example of "data processing," and
requires (i) traceability of user-defined parameters to raw data with attribution of who ran it,
(ii) audit trails/retained records sufficient to reconstruct all data processing activities
(reported or not), and (iii) visibility of repeated/modified processing runs so parameters cannot
be manipulated toward a favourable result.

## 6. FDA Study Data Technical Conformance Guide (analysis program submission expectations)

- Title: Study Data Technical Conformance Guide, Technical Specifications Document
- Date: current version dated June 2026 in the retrieved PDF (revision history in the document
  shows the "Software Programs" subsection, 4.1.2.10, has been revised repeatedly since 2015)
- URL: https://www.fda.gov/media/153632/download
- Retrieval status: RETRIEVED (curl download, pdftotext)

Section 4.1.2.10, "Software Programs":

> "Sponsors should provide the source code used to create all ADaM datasets, tables, and figures
> associated with primary and secondary efficacy analyses. Sponsors should submit source code in
> single byte ASCII text format. Files with MS Windows executable extensions (.cmd, .com, and
> .exe) should NOT be submitted."

> "Furthermore, sponsors should submit the source code used to generate additional information
> included in Section 14 CLINICAL STUDIES of the Prescribing Information, if applicable. The
> specific software utilized (version and operating system) should be specified in the ADRG."

This is FDA's stated expectation for the *analysis program source code itself* (submitted as
plain text) plus a requirement that software name, version, and operating system be recorded in
the Analysis Data Reviewer's Guide (ADRG). It does not, on the text retrieved, require submission
of program *execution logs* as such, only the program source and a metadata statement of the
software/version/OS used to run it.

ICH E9 ("Statistical Principles for Clinical Trials") was in scope of the brief but not
separately retrieved this run: NOT-RETRIEVED. (E9 is principally about analysis methodology, not
run-record/log requirements, so it was deprioritised in favour of the higher-yield sources above;
flagging rather than citing from memory.)

## 7. PHUSE / industry material on analysis program execution logs and log checking

- Title: whirl - "Log Execution of Scripts" (R package, Novo Nordisk Open Source, CRAN)
- URL (README): https://cran.r-project.org/web/packages/whirl/readme/README.html
- URL (pharmaverse comparison page): https://pharmaverse.github.io/examples/logging/logging.html
- Retrieval status: RETRIEVED (WebFetch rendered both pages successfully as HTML/markdown)

whirl README:

> "A log from script execution is in many pharmaceutical companies a GxP requirement, and the
> whirl package honors this requirement by generating a log..."

> Log contents: "status (did the script run with any error or warnings), the actual code itself,
> date and time of execution, the environment the script was executed under (session info), and
> information about packages versions that was utilized."

pharmaverse logging comparison page (comparing logr, logrx, whirl, three R packages purpose-built
for GxP-context script-execution logging):

> Captured fields across the three packages include: User Name, Log Start Time, Log End Time, R
> Version, Machine, Operating System, Other Packages/session information, Program Run Time
> Information, Errors and Warnings; logrx additionally captures a file hash and masked-function
> information for code traceability.

Caveat: these are industry-tooling documentation pages (an R package README and a comparison
vignette maintained by the pharmaverse open-source community), not a PHUSE-published white paper
or regulatory text. A direct PHUSE white paper specifically titled around "analysis program
execution logs" or "log checking" was searched for (WebSearch) but not successfully retrieved
this run: the lexjansen.com PHUSE paper repository was unreachable (`getaddrinfo ENOTFOUND
www.lexjansen.com`, a DNS failure from this environment) and no substitute PHUSE-hosted PDF was
located and read. Status for a PHUSE-authored white paper specifically: **NOT-RETRIEVED.** The
"Risk-based Program Validation for Clinical Trial Analysis and Reporting" PHUSE paper
(lexjansen.com/phuse-us/2019/sp/SP06.pdf) was identified by WebSearch as a plausible match but
could not be fetched (same DNS failure), so no content from it is reported here.

---

## Distilled requirements table

| Requirement | Source(s) | Pinpoint |
|---|---|---|
| A run/change record must capture who acted, when, and (where applicable) why | FDA Part 11; FDA Data Integrity Q&A; ICH E6(R3); EMA computerised systems guideline; MHRA GXP guidance | 21 CFR 11.10(e); FDA Q&A Q1c; ICH E6(R3) glossary "Audit trail"; EMA guideline s.6.2.1; MHRA s.6.13 |
| Audit trail/log must be secure, computer-generated, and time-stamped, and not obscure/overwrite prior data | FDA Part 11; FDA Data Integrity Q&A; ICH E6(R3); EMA guideline; MHRA GXP guidance | 21 CFR 11.10(e); FDA Q&A Q1c; ICH E6(R3) s.4.2.2(b), glossary; EMA s.6.2.1; MHRA s.6.13 |
| "Data processing" explicitly includes statistical analysis of patient data, and audit trails/retained records must allow reconstruction of ALL data processing activities, reported or not | MHRA GXP guidance | s.6.9 "Data Processing" |
| User-defined parameters used in an analysis run must be traceable to raw data, with attribution of who performed the activity | MHRA GXP guidance | s.6.9 |
| Repeated/re-run processing with modified parameters must remain visible (not silently overwritten), to prevent manipulation toward a favourable result | MHRA GXP guidance | s.6.9 |
| Data extraction and determination of analysis sets must be documented in accordance with the pre-specified statistical analysis | ICH E6(R3) | s.4.2.6(c) |
| Computerised systems used in a trial (including, by extension, analysis systems) must be validated, risk-based, appropriate to intended use | ICH E6(R3); EMA computerised systems guideline; FDA Part 11 | ICH E6(R3) s.4.3.4; EMA guideline "Validation" section; 21 CFR 11.10(a) |
| Audit trails must not be disable-able by normal users; admin disabling must itself be logged | ICH E6(R3); EMA guideline; MHRA GXP guidance | ICH E6(R3) s.4.2.2(b); EMA s.6.2.1; MHRA s.6.13 |
| Records/audit trail documentation retained at least as long as the underlying record's retention period | FDA Part 11 | 21 CFR 11.10(e) |
| Data/metadata/audit trail protected and retrievable throughout the retention period; retention periods must be defined and documented | ICH E6(R3); EMA guideline; MHRA GXP guidance | ICH E6(R3) s.4.2.7; EMA guideline retention section; MHRA s.6.11.2 |
| ALCOA / ALCOA+ / ALCOA++ (attributable, legible, contemporaneous, original, accurate [+complete, consistent, enduring, available (+traceable)]) is the governing data-quality frame that a run record's contents are judged against | FDA Data Integrity Q&A; MHRA GXP guidance; EMA computerised systems guideline | FDA Q&A Q1a; MHRA definitions table and s.3.10; EMA guideline s.4.5 |
| Analysis program source code itself (not the execution log) is a required regulatory submission artefact for ADaM/TLF-generating programs; software name, version, and OS must be documented (in the ADRG) | FDA Study Data Technical Conformance Guide | s.4.1.2.10 "Software Programs" |
| Contemporaneous execution of critical steps must be enforced by system design | MHRA GXP guidance | s.6.12 "Computerised system transactions" |
| Industry practice treats a script-execution log (status, code, timestamp, session info incl. R/software version, package versions) as a de facto GxP deliverable for statistical programs, even though this was not found stated in a primary regulatory text this run | whirl README; pharmaverse logging comparison page | whirl README "GxP requirement" statement; pharmaverse logging.html field list |
| Whether a regulator (FDA Part 11 enforcement-discretion guidance) treats 11.10(e) audit trails as strictly mandatory | FDA Part 11 Scope and Application guidance | p.6, "Audit Trail" section: enforcement discretion on 11.10(e)/(k)(2), subject to predicate-rule requirements still applying |
| Reproducibility of statistical analyses as an explicit named requirement | ICH E6(R3) | NOT FOUND: full-text search for "reproducib*" in the retrieved E6(R3) text returned zero hits |
| PHUSE-published white paper content on execution-log checking specifically | PHUSE (lexjansen.com) | NOT-RETRIEVED: DNS failure (`getaddrinfo ENOTFOUND www.lexjansen.com`) prevented fetching any lexjansen.com-hosted PHUSE paper |
| ICH E9 statistical-principles content on run records | ICH E9 | NOT-RETRIEVED this run |
