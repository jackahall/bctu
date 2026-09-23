################################################################################
#                                                                              #
#   Package:      bctu                                                         #
#   Script:       docx_fields.R                                                #
#   Description:  Write the results of the running header and footer fields  #
#                 (STYLEREF, IF, PAGE, NUMPAGES) so the header reads          #
#                 correctly before Word ever recomputes it.                    #
#                                                                              #
#   Author:       Jack Hall                                                    #
#   Email:        j.a.hall.1@bham.ac.uk                                        #
#                                                                              #
################################################################################

#' Fill the fields of a header or footer part
#'
#' A template's running header names the report through `STYLEREF` fields,
#' which Word only recomputes on an update, so a document opened without one
#' shows the template's own cached words. This evaluates each field the way
#' Word would and writes the result into the part: `STYLEREF` takes the
#' first paragraph of the named style, `IF` compares its two quoted
#' operands with `=` or `<>`, and `PAGE` and `NUMPAGES` are given a result
#' region for Word to fill at layout. Nested fields are evaluated inside the
#' instruction of the field that holds them. A field of any other kind keeps
#' its cached result.
#'
#' @param xml The header or footer part text.
#' @param document The document.xml text the `STYLEREF` fields refer to.
#' @return The part text with the field results written.
#' @keywords internal
fill_running_fields <- function(xml, document) {
  positions <- gregexpr("<w:r(?: [^>]*)?>(?:(?!</w:r>).)*</w:r>", xml, perl = TRUE)[[1]]
  if (positions[1] == -1L) return(xml)
  runs <- regmatches(xml, list(positions))[[1]]
  kind <- ifelse(grepl('fldCharType="begin"', runs), "begin",
          ifelse(grepl('fldCharType="separate"', runs), "separate",
          ifelse(grepl('fldCharType="end"', runs), "end",
          ifelse(grepl("<w:instrText", runs), "instr", "text"))))
  if (!any(kind == "begin")) return(xml)

  text_of <- function(run) xml_decode(paste(
    regmatches(run, gregexpr("(?<=>)[^<]*(?=</w:(?:t|instrText)>)", run, perl = TRUE))[[1]], collapse = ""))
  run_props <- function(run) {
    props <- regmatches(run, regexpr("<w:rPr>.*?</w:rPr>", run))
    if (length(props)) props else ""
  }

  # ---- Parse the fields into a tree ----
  parse_field <- function(begin) {
    node <- list(begin = begin, separate = NA_integer_, end = NA_integer_, instr = list())
    i <- begin + 1L
    while (i <= length(runs)) {
      if (kind[i] == "begin") {
        child <- parse_field(i)
        if (is.na(node$separate)) node$instr <- c(node$instr, list(child))
        i <- child$end + 1L
        next
      }
      if (kind[i] == "separate") node$separate <- i
      if (kind[i] == "instr" && is.na(node$separate)) node$instr <- c(node$instr, list(text_of(runs[i])))
      if (kind[i] == "end") { node$end <- i; return(node) }
      i <- i + 1L
    }
    cli::cli_abort("A field in a header or footer part has no end.")
  }

  # ---- Evaluate and rewrite each field ----
  evaluate <- function(node) {
    instr <- paste(vapply(node$instr, function(piece) if (is.list(piece)) evaluate(piece) else piece, character(1)), collapse = "")
    instr <- trimws(instr)
    if (grepl("^STYLEREF\\b", instr)) return(style_text(document, field_argument(instr)))
    if (grepl("^IF\\b", instr)) return(field_if(instr))
    if (grepl("^(PAGE|NUMPAGES)\\b", instr)) return("1")
    NA_character_
  }
  render <- function(node) {
    value <- evaluate(node)
    instr_runs <- character()
    i <- node$begin
    children <- Filter(is.list, node$instr)
    child_at <- vapply(children, function(child) child$begin, integer(1))
    stop_at <- if (is.na(node$separate)) node$end else node$separate
    while (i < stop_at) {
      if (i %in% child_at) {
        child <- children[[match(i, child_at)]]
        instr_runs <- c(instr_runs, render(child))
        i <- child$end + 1L
      } else {
        instr_runs <- c(instr_runs, runs[i])
        i <- i + 1L
      }
    }
    if (is.na(value)) return(paste(c(instr_runs, runs[stop_at:node$end]), collapse = ""))
    props <- run_props(runs[node$begin])
    paste(c(instr_runs,
            paste0("<w:r>", props, '<w:fldChar w:fldCharType="separate"/></w:r>'),
            paste0("<w:r>", props, '<w:t xml:space="preserve">', xml_encode(value), "</w:t></w:r>"),
            runs[node$end]), collapse = "")
  }

  replacement <- runs
  i <- 1L
  while (i <= length(runs)) {
    if (kind[i] != "begin") { i <- i + 1L; next }
    node <- parse_field(i)
    replacement[i] <- render(node)
    replacement[seq.int(i + 1L, node$end)] <- ""
    i <- node$end + 1L
  }
  regmatches(xml, list(positions)) <- list(replacement)
  xml
}

field_argument <- function(instr) {
  argument <- sub("^[A-Z]+\\s+", "", instr)
  argument <- sub("\\s*\\\\\\*.*$", "", argument)
  gsub('^"|"$', "", trimws(argument))
}

field_if <- function(instr) {
  parts <- regmatches(instr, gregexpr('"[^"]*"|<>|=', instr))[[1]]
  if (length(parts) < 5L) return(NA_character_)
  operand <- function(part) gsub('^"|"$', "", part)
  equal <- identical(operand(parts[1]), operand(parts[3]))
  hold <- if (parts[2] == "=") equal else !equal
  operand(if (hold) parts[4] else parts[5])
}

style_text <- function(document, style) {
  paragraph <- regmatches(document, regexpr(
    paste0('<w:p>(?:(?!</w:p>).)*<w:pStyle w:val="', style, '"\\s*/>(?:(?!</w:p>).)*</w:p>'), document, perl = TRUE))
  if (!length(paragraph)) return("")
  xml_decode(paste(regmatches(paragraph, gregexpr("(?<=>)[^<]*(?=</w:t>)", paragraph, perl = TRUE))[[1]], collapse = ""))
}

xml_decode <- function(x) {
  x <- gsub("&lt;", "<", x, fixed = TRUE)
  x <- gsub("&gt;", ">", x, fixed = TRUE)
  x <- gsub("&quot;", '"', x, fixed = TRUE)
  gsub("&amp;", "&", x, fixed = TRUE)
}

xml_encode <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}
