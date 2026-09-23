################################################################################
#                                                                              #
#   Package:      bctu                                                         #
#   Script:       report_layout.R                                              #
#   Description:  check_report_layout(): lay a rendered docx out with          #
#                 LibreOffice and report the page faults a reader would see    #
#                 (orphaned captions, blank pages, sparse pages).              #
#                                                                              #
#   Author:       Jack Hall                                                    #
#   Email:        j.a.hall.1@bham.ac.uk                                        #
#                                                                              #
################################################################################

#' Check the page layout of a rendered report
#'
#' Converts the docx to PDF with LibreOffice (`soffice` on the PATH, or the
#' path in `options(bctu.soffice = )`) and reads it page by page with
#' `pdftotext`, flagging a caption left as the last line of a page (its table
#' or figure fell to the next page), a blank page, and a page with fewer
#' than `sparse` lines of body text. LibreOffice is a proxy for Word: it
#' ignores keep-with-next before a table and substitutes fonts, so a flagged
#' caption whose docx carries the keep flags may be its artefact.
#'
#' @param path Path to the docx.
#' @param sparse Pages with fewer body lines than this are reported.
#' @return A data frame with one row per finding: `page`, `orientation`,
#'   `lines`, `issue` and `first` (the first body line). Zero rows means no
#'   finding. `NULL`, with a message, when LibreOffice or pdftotext is not
#'   available.
#' @examples
#' \dontrun{
#' check_report_layout("report.docx")
#' }
#' @export
check_report_layout <- function(path, sparse = 6L) {
  if (!file.exists(path)) cli::cli_abort("No file at {.file {path}}.")
  soffice <- path.expand(getOption("bctu.soffice", Sys.which("soffice")))
  path <- normalizePath(path)
  if (!nzchar(soffice) || !nzchar(Sys.which("pdftotext")) || !nzchar(Sys.which("pdfinfo"))) {
    cli::cli_inform("check_report_layout() needs LibreOffice (soffice), pdftotext and pdfinfo; none run.")
    return(invisible(NULL))
  }
  out <- tempfile("bctu-layout-")
  dir.create(out)
  on.exit(unlink(out, recursive = TRUE), add = TRUE)
  system2(soffice, c(paste0("-env:UserInstallation=file://", file.path(out, "profile")),
                     "--headless", "--convert-to", "pdf", "--outdir", shQuote(out), shQuote(path)),
          stdout = FALSE, stderr = FALSE)
  pdf <- list.files(out, "\\.pdf$", full.names = TRUE)
  if (!length(pdf)) cli::cli_abort("LibreOffice did not convert {.file {path}}.")
  info <- system2("pdfinfo", shQuote(pdf[1]), stdout = TRUE)
  n <- as.integer(sub("^Pages:\\s+", "", grep("^Pages:", info, value = TRUE)))
  rows <- lapply(seq_len(n), function(p) {
    text <- system2("pdftotext", c("-layout", "-f", p, "-l", p, shQuote(pdf[1]), "-"), stdout = TRUE)
    size <- system2("pdfinfo", c("-f", p, "-l", p, shQuote(pdf[1])), stdout = TRUE)
    width <- as.numeric(sub(".*size:\\s+([0-9.]+) x.*", "\\1", grep("size:", size, value = TRUE)[1]))
    body <- trimws(text[nzchar(trimws(text))])
    body <- body[!grepl("^Page [0-9]+ of [0-9]+$", body)][-1]  # drop the running header line
    issue <- c(
      if (!length(body)) "blank page",
      if (length(body) && grepl("^(Table|Figure) [0-9]+:", body[length(body)])) "caption orphaned at the foot of the page",
      if (length(body) && length(body) < sparse) "sparse page"
    )
    if (!length(issue)) return(NULL)
    data.frame(page = p, orientation = if (isTRUE(width > 700)) "landscape" else "portrait",
               lines = length(body), issue = paste(issue, collapse = "; "),
               first = if (length(body)) substr(body[1], 1, 60) else "", stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  if (is.null(out)) data.frame(page = integer(), orientation = character(), lines = integer(),
                               issue = character(), first = character(), stringsAsFactors = FALSE)
  else out
}
