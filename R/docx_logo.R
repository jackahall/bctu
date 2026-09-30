################################################################################
#                                                                              #
#   Package:      bctu                                                         #
#   Script:       docx_logo.R                                                  #
#   Description:  Places the trial logo in the title-page header of a rendered #
#                 trial report, mirroring the BCTU logo: right-aligned to the  #
#                 margin the BCTU logo sits at, centred on its vertical        #
#                 midline, scaled to fit a width by height box.                #
#                                                                              #
#   Author:       Jack Hall                                                    #
#   Email:        j.a.hall.1@bham.ac.uk                                        #
#                                                                              #
################################################################################

INCH_EMU <- 914400

#' A YAML length in inches
#'
#' @param x A string such as `"2in"`, `"5cm"`, `"40mm"` or `"144pt"`; a bare
#'   number is inches.
#' @param key The YAML key, named in errors.
#' @return The length in inches.
#' @noRd
logo_length_in <- function(x, key) {
  parts <- regmatches(x, regexec("^\\s*([0-9]*\\.?[0-9]+)\\s*(in|cm|mm|pt)?\\s*$", x))[[1]]
  if (!length(parts))
    cli::cli_abort("{.field {key}}: {.val {x}} must be a number with unit in, cm, mm or pt.")
  per_unit <- c("in" = 1, cm = 1 / 2.54, mm = 1 / 25.4, pt = 1 / 72)
  as.numeric(parts[2]) * per_unit[[if (nzchar(parts[3])) parts[3] else "in"]]
}

#' Pixel size of a PNG, GIF or JPEG image, read from its header
#'
#' @param path Path to the image.
#' @return A list with `width`, `height` (pixels) and `ext` (`"png"`,
#'   `"gif"` or `"jpeg"`).
#' @noRd
image_size_px <- function(path) {
  bytes <- readBin(path, "raw", file.size(path))
  int <- function(b, big = TRUE) sum(as.integer(if (big) b else rev(b)) * 256^((length(b) - 1):0))
  if (length(bytes) > 24 && identical(bytes[1:8], as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a))))
    return(list(width = int(bytes[17:20]), height = int(bytes[21:24]), ext = "png"))
  if (length(bytes) > 10 && rawToChar(bytes[1:3]) == "GIF")
    return(list(width = int(bytes[7:8], big = FALSE), height = int(bytes[9:10], big = FALSE), ext = "gif"))
  if (length(bytes) > 4 && identical(bytes[1:2], as.raw(c(0xff, 0xd8)))) {
    i <- 3L
    while (i + 8L <= length(bytes) && bytes[i] == as.raw(0xff)) {
      marker <- as.integer(bytes[i + 1L])
      if (marker %in% c(0xc0:0xc3, 0xc5:0xc7, 0xc9:0xcb, 0xcd:0xcf))
        return(list(width = int(bytes[i + 7:8]), height = int(bytes[i + 5:6]), ext = "jpeg"))
      i <- i + 2L + int(bytes[i + 2:3])
    }
  }
  cli::cli_abort("{.field trial-logo}: {.file {path}} is not a PNG, GIF or JPEG image bctu can read.")
}

#' Add the trial logo to the title-page header
#'
#' The title page's header (`header2.xml`) holds the BCTU logo as a
#' page-anchored drawing. The trial logo is added beside it as a second
#' drawing: scaled to fit `width` by `height` inches with its aspect ratio
#' kept (the height never exceeds the BCTU logo's), its right edge as far
#' from the right page edge as the BCTU logo's left edge is from the left,
#' and its vertical centre on the BCTU logo's.
#'
#' @param work The unzipped docx folder.
#' @param logo Path to the image.
#' @param width,height The box to fit, in inches.
#' @return `work`, invisibly.
#' @noRd
add_header_logo <- function(work, logo, width, height) {
  if (!file.exists(logo))
    cli::cli_abort("{.field trial-logo}: no image at {.file {logo}} (relative paths are read from the Rmd's folder).")
  size <- image_size_px(logo)
  hdr_path <- file.path(work, "word", "header2.xml")
  rels_path <- file.path(work, "word", "_rels", "header2.xml.rels")
  hdr <- readChar(hdr_path, file.size(hdr_path), useBytes = TRUE)
  num <- function(pattern) as.numeric(regmatches(hdr, regexec(pattern, hdr))[[1]][2])
  bctu_left <- num('<wp:positionH relativeFrom="page"><wp:posOffset>([0-9]+)</wp:posOffset>')
  bctu_top  <- num('<wp:positionV relativeFrom="page"><wp:posOffset>([0-9]+)</wp:posOffset>')
  bctu_h    <- num('<wp:extent cx="[0-9]+" cy="([0-9]+)"/>')
  if (anyNA(c(bctu_left, bctu_top, bctu_h)))
    cli::cli_abort("The title-page header has no page-anchored BCTU logo to align the trial logo with.")
  doc_path <- file.path(work, "word", "document.xml")
  document <- readChar(doc_path, file.size(doc_path), useBytes = TRUE)
  page_w <- as.numeric(regmatches(document, regexec('<w:pgSz w:w="([0-9]+)"', document))[[1]][2]) * TWIP_EMU

  scale <- min(width * INCH_EMU / size$width, min(height * INCH_EMU, bctu_h) / size$height)
  cx <- round(size$width * scale)
  cy <- round(size$height * scale)
  left <- round(page_w - bctu_left - cx)
  top  <- round(bctu_top + (bctu_h - cy) / 2)

  media <- paste0("trial-logo.", size$ext)
  file.copy(logo, file.path(work, "word", "media", media), overwrite = TRUE)
  rels <- readChar(rels_path, file.size(rels_path), useBytes = TRUE)
  rels <- sub("</Relationships>", paste0(
    '<Relationship Id="rIdTrialLogo" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/image" Target="media/',
    media, '"/></Relationships>'), rels, fixed = TRUE)
  writeChar(rels, rels_path, eos = NULL, useBytes = TRUE)

  types_path <- file.path(work, "[Content_Types].xml")
  types <- readChar(types_path, file.size(types_path), useBytes = TRUE)
  if (!grepl(paste0('Extension="', size$ext, '"'), types, fixed = TRUE))
    types <- sub("<Default ", paste0('<Default Extension="', size$ext, '" ContentType="image/', size$ext, '"/><Default '),
                 types, fixed = TRUE)
  writeChar(types, types_path, eos = NULL, useBytes = TRUE)

  drawing <- sprintf(paste0(
    '<w:r><w:drawing><wp:anchor distT="0" distB="0" distL="0" distR="0" simplePos="0" relativeHeight="1" behindDoc="0" ',
    'locked="0" layoutInCell="1" allowOverlap="1"><wp:simplePos x="0" y="0"/>',
    '<wp:positionH relativeFrom="page"><wp:posOffset>%d</wp:posOffset></wp:positionH>',
    '<wp:positionV relativeFrom="page"><wp:posOffset>%d</wp:posOffset></wp:positionV>',
    '<wp:extent cx="%d" cy="%d"/><wp:effectExtent l="0" t="0" r="0" b="0"/><wp:wrapNone/>',
    '<wp:docPr id="2" name="Trial Logo"/><wp:cNvGraphicFramePr><a:graphicFrameLocks noChangeAspect="1"/></wp:cNvGraphicFramePr>',
    '<a:graphic><a:graphicData uri="http://schemas.openxmlformats.org/drawingml/2006/picture"><pic:pic>',
    '<pic:nvPicPr><pic:cNvPr id="2" name="%s"/><pic:cNvPicPr/></pic:nvPicPr>',
    '<pic:blipFill><a:blip r:embed="rIdTrialLogo"/><a:stretch><a:fillRect/></a:stretch></pic:blipFill>',
    '<pic:spPr><a:xfrm><a:off x="0" y="0"/><a:ext cx="%d" cy="%d"/></a:xfrm><a:prstGeom prst="rect"><a:avLst/></a:prstGeom></pic:spPr>',
    '</pic:pic></a:graphicData></a:graphic></wp:anchor></w:drawing></w:r>'),
    as.integer(left), as.integer(top), as.integer(cx), as.integer(cy), media, as.integer(cx), as.integer(cy))
  hdr <- sub("</w:drawing></w:r>", paste0("</w:drawing></w:r>", drawing), hdr, fixed = TRUE)
  writeChar(hdr, hdr_path, eos = NULL, useBytes = TRUE)
  invisible(work)
}
