# ---------------------------------------------------------------------------
# Format-agnostic report tables
# ---------------------------------------------------------------------------
# A report table is ONE structured object that describes a table completely:
# the body data (kept in its original types so QC can read the real numbers),
# a column specification (which columns, their labels, their alignment),
# optional spanning group headers, optional full-width banner rows, and a
# caption. It is NOT tied to Word, LaTeX or HTML.
#
# Rendering to a specific target is a separate, explicit step. One renderer,
# `render_table_markdown`, emits a pandoc grid table that pandoc typesets for
# every output format (DOCX styles, PDF longtable with computed column widths
# and wrapping), so the outputs can never drift apart and the underlying
# numbers are always retrievable for checking with `report_table_data()`.

# --- constructor -----------------------------------------------------------
#' Build a format-agnostic report table
#'
#' Returns a single structured object that the grid-table renderer consumes for
#' every output format (Word and PDF), so a table is described once and
#' rendered the same way everywhere. The body data keeps its original types, so
#' the exact numbers behind the report are always retrievable with
#' [report_table_data()].
#'
#' @param data A data frame: the body of the table (one row per table row).
#' @param columns Which columns to show and their headings. One of:
#'   * `NULL` (default) — show every column of `data`, heading = column name;
#'   * a character vector of column names — show those columns, in that order,
#'     heading = column name;
#'   * a *named* character vector — names are columns of `data`, values are the
#'     headings shown in the table.
#' @param caption Optional table caption (a single string).
#' @param group_headers Optional spanning headers shown above the column
#'   headings. Either a data frame with columns `label` and `span`, or a named
#'   integer vector (names = labels, values = number of columns spanned). The
#'   spans must add up to the number of displayed columns, left to right.
#' @param banner_rows Optional full-width rows inserted into the body (for
#'   example a group separator). A list of `list(label = , after = )` entries,
#'   or a data frame with columns `label` and `after`, where `after` is the body
#'   row number the banner follows (`0` = before the first row).
#' @param align Optional column alignment: a character vector, one of
#'   `"left"`, `"right"`, `"center"` per displayed column. If `NULL` (default),
#'   numeric columns are right-aligned and all others left-aligned.
#' @return A `bctu_report_table` object.
#' @examples
#' data <- data.frame(id = 1:3, age = c(45, 62, 38), sex = c("F", "M", "F"))
#' report_table(data, caption = "Baseline characteristics")
#' @export
report_table <- function(data,
                         columns = NULL,
                         caption = NULL,
                         group_headers = NULL,
                         banner_rows = NULL,
                         align = NULL) {
  if (!is.data.frame(data))
    cli::cli_abort("{.arg data} must be a data frame.")
  if (!is.null(caption) && !is_string(caption))
    cli::cli_abort("{.arg caption} must be a single string or NULL.")

  spec <- resolve_table_columns(data, columns, align)
  body <- data[, spec$name, drop = FALSE]
  names(body) <- spec$name
  rownames(body) <- NULL

  groups  <- normalise_group_headers(group_headers, nrow(spec))
  banners <- normalise_banner_rows(banner_rows, nrow(body))

  structure(
    list(
      body          = body,        # original types kept for QC
      columns       = spec,        # data.frame: name, label, align
      group_headers = groups,      # NULL or data.frame: label, span
      banner_rows   = banners,     # NULL or data.frame: label, after
      caption       = caption
    ),
    class = c("bctu_report_table", "list")
  )
}

#' The underlying (unformatted) data behind a report table
#'
#' Returns the exact body data frame, in its original types, so QC can check the
#' real numbers rather than re-parsing rendered text.
#' @param x A `bctu_report_table`.
#' @return A data frame (the displayed columns, original types).
#' @export
report_table_data <- function(x) {
  if (!inherits(x, "bctu_report_table"))
    cli::cli_abort("{.arg x} must be a {.cls bctu_report_table}.")
  x$body
}

#' @export
print.bctu_report_table <- function(x, ...) {
  cli::cli_rule("bctu report table")
  if (!is.null(x$caption)) cli::cli_text("{.emph {x$caption}}")
  cli::cli_text("{nrow(x$body)} row{?s} x {nrow(x$columns)} column{?s}: {x$columns$label}")
  if (!is.null(x$group_headers))
    cli::cli_text("group headers: {x$group_headers$label} (spans {x$group_headers$span})")
  if (!is.null(x$banner_rows))
    cli::cli_text("banner rows: {x$banner_rows$label}")
  invisible(x)
}

# --- column / header / banner resolution -----------------------------------
#' Resolve the `columns`/`align` arguments into a tidy column specification
#' @keywords internal
resolve_table_columns <- function(data, columns, align) {
  if (is.null(columns)) {
    name  <- names(data)
    label <- names(data)
  } else if (is.character(columns)) {
    name  <- unname(columns)
    label <- if (!is.null(names(columns))) unname(columns) else name
    if (!is.null(names(columns))) name <- names(columns)
  } else {
    cli::cli_abort("{.arg columns} must be NULL or a (optionally named) character vector.")
  }
  missing_cols <- setdiff(name, names(data))
  if (length(missing_cols))
    cli::cli_abort(c("These columns are not in {.arg data}: {.val {missing_cols}}.",
                     "i" = "Available columns: {.val {names(data)}}."))
  # Duplicate names silently mis-map values (data[, "x"] returns the first "x"),
  # so reject them rather than display the wrong column.
  if (anyDuplicated(name))
    cli::cli_abort(c("The displayed columns include duplicate names: {.val {unique(name[duplicated(name)])}}.",
                     "i" = "Rename columns to be unique before tabulating."))
  dup_src <- intersect(name, names(data)[duplicated(names(data))])
  if (length(dup_src))
    cli::cli_abort(c("{.arg data} has duplicate columns named {.val {dup_src}}.",
                     "i" = "Column selection by name is ambiguous; rename them to be unique."))

  if (is.null(align)) {
    align <- vapply(name, function(nm) if (is.numeric(data[[nm]])) "right" else "left",
                    character(1))
  } else {
    if (length(align) != length(name))
      cli::cli_abort("{.arg align} must have one entry per displayed column ({length(name)}).")
    bad_align <- setdiff(align, c("left", "right", "center"))
    if (length(bad_align))
      cli::cli_abort(c("{.arg align} values must be {.val left}, {.val right} or {.val center}.",
                       "x" = "Invalid: {.val {unique(bad_align)}}."))
  }
  data.frame(name = name, label = label, align = unname(align),
             stringsAsFactors = FALSE)
}

#' Normalise the `group_headers` argument into a data frame of label + span
#' @keywords internal
normalise_group_headers <- function(group_headers, n_columns) {
  if (is.null(group_headers)) return(NULL)
  if (is.numeric(group_headers)) {
    if (is.null(names(group_headers)))
      cli::cli_abort("A numeric {.arg group_headers} must be named (names = labels).")
    group_headers <- data.frame(label = names(group_headers),
                                span = as.integer(unname(group_headers)),
                                stringsAsFactors = FALSE)
  }
  if (!is.data.frame(group_headers) ||
      !all(c("label", "span") %in% names(group_headers)))
    cli::cli_abort("{.arg group_headers} must be a data frame with {.field label} and {.field span}, or a named integer vector.")
  group_headers$span <- as.integer(group_headers$span)
  if (any(is.na(group_headers$span) | group_headers$span < 1L))
    cli::cli_abort(c("Each group header span must be a whole number of at least 1.",
                     "x" = "Got: {.val {group_headers$span}}."))
  if (sum(group_headers$span) != n_columns)
    cli::cli_abort(c("Group header spans must add up to the number of columns ({n_columns}).",
                     "x" = "They add up to {sum(group_headers$span)}."))
  group_headers[, c("label", "span")]
}

#' Normalise the `banner_rows` argument into a data frame of label + after
#' @keywords internal
normalise_banner_rows <- function(banner_rows, n_rows) {
  if (is.null(banner_rows)) return(NULL)
  if (is.list(banner_rows) && !is.data.frame(banner_rows)) {
    banner_rows <- do.call(rbind, lapply(banner_rows, function(b)
      data.frame(label = b$label, after = as.integer(b$after),
                 stringsAsFactors = FALSE)))
  }
  if (!is.data.frame(banner_rows) ||
      !all(c("label", "after") %in% names(banner_rows)))
    cli::cli_abort("{.arg banner_rows} must be a list of list(label=, after=) or a data frame with {.field label} and {.field after}.")
  banner_rows$after <- as.integer(banner_rows$after)
  if (any(banner_rows$after < 0L | banner_rows$after > n_rows))
    cli::cli_abort("Each banner {.field after} must be between 0 and the number of rows ({n_rows}).")
  banner_rows[, c("label", "after")]
}

# --- shared cell formatting ------------------------------------------------
#' Format one body column to display strings (numbers kept readable, NA blank)
#' @keywords internal
format_table_column <- function(col) {
  out <- if (is.numeric(col)) format(col, trim = TRUE, justify = "none")
         else as.character(col)
  out[is.na(col)] <- ""
  out
}

#' The body as a character matrix of display strings (shared by all renderers)
#' @keywords internal
report_table_display_body <- function(x) {
  cells <- lapply(x$columns$name, function(nm) format_table_column(x$body[[nm]]))
  m <- matrix(unlist(cells, use.names = FALSE),
              nrow = nrow(x$body), ncol = nrow(x$columns))
  colnames(m) <- x$columns$name
  m
}

# ---------------------------------------------------------------------------
# Renderer 1: pandoc grid table (the DOCX path)
# ---------------------------------------------------------------------------
# A pandoc grid table. Column widths are computed from the widest cell in each
# column (header, group header, banner and every body cell), so a token longer
# than its heading widens the column instead of breaking the alignment. Group
# headers and banners are rendered as spanning cells (pandoc treats a content
# row that omits the internal `|` as a cell spanning those columns).

#' Render a report table as a pandoc grid table (every output format)
#'
#' @param x A `bctu_report_table`.
#' @return A single string containing the pandoc grid table (and its caption).
#' @examples
#' data <- data.frame(id = 1:3, age = c(45, 62, 38))
#' tbl <- report_table(data, caption = "Baseline characteristics")
#' cat(render_table_markdown(tbl))
#' @export
render_table_markdown <- function(x) {
  if (!inherits(x, "bctu_report_table"))
    cli::cli_abort("{.arg x} must be a {.cls bctu_report_table}.")
  body   <- report_table_display_body(x)
  body[] <- vapply(as.vector(body), escape_grid_text, character(1))
  labels <- vapply(x$columns$label, escape_grid_text, character(1), USE.NAMES = FALSE)
  aligns <- x$columns$align
  n_col  <- ncol(body)
  group_headers <- x$group_headers
  if (!is.null(group_headers))
    group_headers$label <- vapply(group_headers$label, escape_grid_text, character(1), USE.NAMES = FALSE)
  banner_rows <- x$banner_rows
  if (!is.null(banner_rows))
    banner_rows$label <- vapply(banner_rows$label, escape_grid_text, character(1), USE.NAMES = FALSE)

  widths <- vapply(seq_len(n_col), function(j)
    max(nchar(labels[j]), if (nrow(body)) max(nchar(body[, j])) else 0L, 1L),
    integer(1))

  # widen for spanning group headers so their label always fits
  if (!is.null(group_headers)) {
    start <- 1L
    for (i in seq_len(nrow(group_headers))) {
      span <- group_headers$span[i]
      cols <- start:(start + span - 1L)
      inner <- sum(widths[cols]) + 3L * span - 3L
      deficit <- nchar(group_headers$label[i]) - inner
      if (deficit > 0L) widths[cols[span]] <- widths[cols[span]] + deficit
      start <- start + span
    }
  }
  # widen for full-width banner rows
  if (!is.null(banner_rows)) {
    inner_all <- sum(widths) + 3L * n_col - 3L
    deficit <- max(nchar(banner_rows$label)) - inner_all
    if (deficit > 0L) widths[n_col] <- widths[n_col] + deficit
  }

  lines <- character(0)
  add <- function(...) lines <<- c(lines, ...)

  add(grid_border_line(widths, "-"))
  if (!is.null(group_headers)) {
    segs <- Map(function(l, s) list(text = l, span = s, align = "center"),
                group_headers$label, group_headers$span)
    add(grid_content_line(segs, widths))
    add(grid_border_line(widths, "-"))
  }
  header_segs <- Map(function(l, a) list(text = l, span = 1L, align = a), labels, aligns)
  add(grid_content_line(header_segs, widths))
  add(grid_border_line(widths, "=", aligns))

  banner_after <- if (is.null(banner_rows)) integer(0) else banner_rows$after
  emit_banners <- function(after_row) {
    idx <- which(banner_after == after_row)
    for (k in idx) {
      add(grid_content_line(list(list(text = banner_rows$label[k],
                                      span = n_col, align = "left")), widths))
      add(grid_border_line(widths, "-"))
    }
  }
  emit_banners(0L)
  if (nrow(body) == 0L) {
    # no data rows: emit one blank body row so the grid table keeps a valid
    # header/body/closing-border structure instead of ending at the "=" line
    blank_segs <- Map(function(a) list(text = "", span = 1L, align = a), aligns)
    add(grid_content_line(blank_segs, widths))
    add(grid_border_line(widths, "-"))
  } else {
    for (r in seq_len(nrow(body))) {
      row_segs <- Map(function(v, a) list(text = v, span = 1L, align = a),
                      body[r, ], aligns)
      add(grid_content_line(row_segs, widths))
      add(grid_border_line(widths, "-"))
      emit_banners(r)
    }
  }

  table <- paste(lines, collapse = "\n")
  if (!is.null(x$caption))
    table <- paste0(table, "\n\nTable: ", escape_grid_text(x$caption))
  table
}

#' Alias of [render_table_markdown()] naming the grid-table target explicitly
#' @param x A `bctu_report_table`.
#' @return A pandoc grid table string.
#' @export
render_table_gridtable <- function(x) render_table_markdown(x)

#' A grid-table border line (`+---+`); alignment colons on the `=` header line
#' @keywords internal
grid_border_line <- function(widths, char, aligns = NULL) {
  segs <- vapply(seq_along(widths), function(j) {
    seg <- strrep(char, widths[j] + 2L)
    a <- if (is.null(aligns)) "none" else aligns[j]
    if (a %in% c("left", "center"))
      substr(seg, 1L, 1L) <- ":"
    if (a %in% c("right", "center"))
      substr(seg, nchar(seg), nchar(seg)) <- ":"
    seg
  }, character(1))
  paste0("+", paste(segs, collapse = "+"), "+")
}

#' Escape characters that would corrupt pandoc grid-table structure
#'
#' Collapses embedded newlines to a space (a raw newline would split a cell
#' across grid-table lines without a border) and backslash-escapes "|" (a
#' literal pipe inside a cell would read as a spurious column boundary).
#' @keywords internal
escape_grid_text <- function(text) {
  text <- as.character(text)
  text <- gsub("\r\n|\r|\n", " ", text)
  text <- gsub("|", "\\|", text, fixed = TRUE)
  # Leading spaces are hierarchy indentation (e.g. classification tiers).
  # Pandoc strips ordinary leading whitespace when it parses the cell, so each
  # leading space becomes a no-break space (U+00A0), which every output format
  # renders at space width.
  indent <- attr(regexpr("^ +", text), "match.length")
  has <- indent > 0L
  text[has] <- paste0(strrep("\u00a0", indent[has]),
                      substr(text[has], indent[has] + 1L, nchar(text[has])))
  # A cell is data, never markup. Grid-table cells are parsed as block-level
  # markdown, so a leading list marker ("1. ", "II. ", "a) "), bullet, heading
  # or quote marker would typeset the cell as that construct (e.g. an
  # enumerate environment with its own spacing and renumbering). Backslash-
  # escape the marker punctuation so the text stays literal.
  text <- sub("^([0-9]+|[IVXLCDMivxlcdm]+|[A-Za-z])([.)]) ", "\\1\\\\\\2 ", text)
  text <- sub("^(\\([A-Za-z0-9]+)\\) ", "\\1\\\\) ", text)
  text <- sub("^([-+*>]) ", "\\\\\\1 ", text)
  text <- sub("^(#+ )", "\\\\\\1", text)
  text
}

#' A grid-table content line, supporting cells that span several columns
#' @keywords internal
grid_content_line <- function(segments, widths) {
  out <- "|"
  col <- 1L
  for (seg in segments) {
    span <- seg$span
    cols <- col:(col + span - 1L)
    inner <- sum(widths[cols]) + 3L * span - 3L
    out <- paste0(out, " ", pad_text(seg$text, inner, seg$align), " |")
    col <- col + span
  }
  out
}

#' Pad text to a fixed width with a given alignment
#' @keywords internal
pad_text <- function(text, width, align = "left") {
  text <- as.character(text)
  gap <- max(0L, width - nchar(text))
  switch(align,
    right  = paste0(strrep(" ", gap), text),
    center = paste0(strrep(" ", gap %/% 2L), text, strrep(" ", gap - gap %/% 2L)),
    paste0(text, strrep(" ", gap)))
}

