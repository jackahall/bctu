################################################################################
#                                                                              #
#   Package:      bctu                                                         #
#   Script:       report_helpers.R                                             #
#   Description:  Data-frame helpers for Rmd report tables: indentation,       #
#                 n/N (%) cells, and pandoc grid tables with wrapped cells.    #
#                                                                              #
#   Author:       Jack Hall                                                    #
#   Email:        j.a.hall.1@bham.ac.uk                                        #
#                                                                              #
################################################################################

NBSP <- "\u00a0"

#' Indent table text with non-breaking spaces
#'
#' Prefixes text with four non-breaking spaces per level. Pandoc and
#' [strwrap()] strip ordinary leading spaces, so indentation made of plain
#' spaces disappears in a rendered table; non-breaking spaces survive.
#'
#' @param x A character vector.
#' @param levels Number of indentation levels (4 spaces each). Default 1.
#' @return `x` with the indentation prefixed.
#' @examples
#' indent("Male")
#' indent("Aged 65 or over", levels = 2)
#' @export
indent <- function(x, levels = 1L) {
  paste0(strrep(NBSP, 4L * levels), x)
}

#' Format counts as "n/N (p%)"
#'
#' @param n Numerator counts.
#' @param N Denominator counts. Where `N` is 0 the result is `"n/N"` with no
#'   percentage.
#' @param digits Decimal places for the percentage. Default 0.
#' @return A character vector.
#' @examples
#' n_pct(c(3, 0), c(12, 0))
#' n_pct(5, 7, digits = 1)
#' @export
n_pct <- function(n, N, digits = 0L) {
  pct <- formatC(100 * n / N, format = "f", digits = digits)
  ifelse(N == 0, paste0(n, "/", N), paste0(n, "/", N, " (", pct, "%)"))
}

#' Column widths for a grid table
#'
#' Each column is as wide as its longest header or cell line (cells may hold
#' several lines separated by `"\n"`), kept between 8 characters and the cap.
#'
#' @param df A data frame of character cells.
#' @param caps Maximum width: one number for every column, or one per column.
#' @return An integer vector of widths, one per column.
#' @examples
#' col_widths(data.frame(Group = c("A", "B"), n = c("10", "12")), caps = 20)
#' @export
col_widths <- function(df, caps) {
  MIN_WIDTH <- 8L
  if (!length(caps) %in% c(1L, ncol(df)))
    cli::cli_abort("{.arg caps} must be one number or one per column ({ncol(df)}).")
  caps <- rep_len(as.integer(caps), ncol(df))
  longest <- vapply(seq_along(df), function(j) {
    lines <- unlist(strsplit(c(names(df)[j], as.character(df[[j]])), "\n", fixed = TRUE))
    max(0L, nchar(lines))
  }, integer(1))
  pmin(pmax(longest, MIN_WIDTH), caps)
}

#' Wrap one cell to a column width
#'
#' Each `"\n"`-separated field is wrapped to `width`, and fields are separated
#' by a blank line, so pandoc renders them as paragraphs within the cell.
#' @param text A single string.
#' @param width Column width in characters.
#' @return A character vector of lines.
#' @keywords internal
wrap_grid_cell <- function(text, width) {
  fields <- strsplit(text, "\n", fixed = TRUE)[[1]]
  if (!length(fields)) return("")
  wrapped <- lapply(fields, function(f) {
    lines <- strwrap(f, width = width + 1L)
    if (length(lines)) lines else ""
  })
  lines <- unlist(lapply(wrapped, c, ""))
  lines[-length(lines)]
}

#' Build a pandoc grid table from a data frame
#'
#' Renders a data frame of character cells as a pandoc grid table. Cell text
#' is wrapped to the column width; a `"\n"` in a cell starts a new paragraph
#' within the cell. The header row is the column names. A column whose
#' wrapped text has a word longer than its width is widened to fit it.
#'
#' @param df A data frame of character cells.
#' @param widths Column widths in characters, one per column (see
#'   [col_widths()]).
#' @param span_rows `NULL`, or a logical vector with one value per row: rows
#'   marked `TRUE` are rendered as one full-width cell holding the text of
#'   the first column.
#' @return A single string holding the grid table.
#' @examples
#' df <- data.frame(Group = c("A", "B"), n = c("10", "12"))
#' cat(grid_table(df, widths = col_widths(df, caps = 20)))
#' @export
grid_table <- function(df, widths, span_rows = NULL) {
  n_col <- ncol(df)
  if (length(widths) != n_col)
    cli::cli_abort("{.arg widths} must have one value per column ({n_col}).")
  span_rows <- span_rows %||% rep(FALSE, nrow(df))
  if (length(span_rows) != nrow(df))
    cli::cli_abort("{.arg span_rows} must have one value per row ({nrow(df)}).")
  cells <- rbind(names(df), as.matrix(df))
  span_rows <- c(FALSE, span_rows)
  widths <- as.integer(widths)

  wrapped <- lapply(seq_len(nrow(cells)), function(i)
    if (span_rows[i]) list(wrap_grid_cell(cells[i, 1], sum(widths) + 3L * (n_col - 1L)))
    else Map(wrap_grid_cell, cells[i, ], widths))
  widths <- fit_grid_widths(wrapped, span_rows, widths)

  rule <- grid_border_line(widths, "-")
  body <- if (nrow(df)) unlist(lapply(wrapped[-1], function(row) c(grid_row_lines(row, widths), rule)))
          else c(grid_content_line(rep(list(list(text = "", span = 1L)), n_col), widths), rule)
  paste(c(rule, grid_row_lines(wrapped[[1]], widths), grid_border_line(widths, "="), body),
        collapse = "\n")
}

#' Widen grid-table columns so every wrapped line fits
#'
#' Each column is widened to its longest wrapped line; the last column is
#' widened further when a full-width span row needs more room.
#' @param wrapped A list of rows, each a list of wrapped cell lines (one
#'   element for a span row).
#' @param span_rows Logical, one value per row of `wrapped`.
#' @param widths Column widths in characters.
#' @return The widened column widths.
#' @keywords internal
fit_grid_widths <- function(wrapped, span_rows, widths) {
  n_col <- length(widths)
  for (j in seq_len(n_col))
    widths[j] <- max(widths[j], nchar(unlist(lapply(wrapped[!span_rows], `[[`, j))))
  deficit <- max(0L, nchar(unlist(wrapped[span_rows]))) - (sum(widths) + 3L * (n_col - 1L))
  if (deficit > 0L) widths[n_col] <- widths[n_col] + deficit
  widths
}

#' The grid-table content lines for one row
#'
#' @param row A list of wrapped cell lines, one element per column, or a
#'   single element for a row spanning every column.
#' @param widths Column widths in characters.
#' @return A character vector of content lines.
#' @keywords internal
grid_row_lines <- function(row, widths) {
  span <- if (length(row) == 1L) length(widths) else 1L
  vapply(seq_len(max(lengths(row))), function(k)
    grid_content_line(lapply(row, function(l)
      list(text = if (k <= length(l)) l[k] else "", span = span)), widths),
    character(1))
}

#' A banner row for a report table
#'
#' A one-row data frame with the same columns as `template`, holding `text`
#' in the first column and blanks elsewhere. [render_table()] renders such a
#' row as a full-width bold banner when the table has indented rows.
#'
#' @param template A data frame whose columns the row copies.
#' @param text The banner text.
#' @return A one-row data frame of character columns.
#' @examples
#' tab <- data.frame(Characteristic = indent("Male"), Total = "10")
#' rbind(banner_row(tab, "Sex"), tab)
#' @export
banner_row <- function(template, text) {
  row <- as.data.frame(as.list(rep("", ncol(template))), col.names = names(template),
                       check.names = FALSE, stringsAsFactors = FALSE)
  row[[1]] <- as.character(text)
  row
}

#' Escape a leading list marker in table text
#'
#' Inserts a backslash before a `-`, `+` or `*` at the start of each
#' `"\n"`-separated field (after any non-breaking-space indentation), so
#' pandoc keeps it as text instead of reading a list item.
#' @param x A character vector.
#' @return `x` with leading markers escaped.
#' @keywords internal
escape_list_marker <- function(x) {
  vapply(x, function(s) {
    fields <- strsplit(s, "\n", fixed = TRUE)[[1]]
    fields <- sub("^((?:\u00a0)*)([-+*])", "\\1\\\\\\2", fields, perl = TRUE)
    paste(fields, collapse = "\n")
  }, character(1), USE.NAMES = FALSE)
}

#' Render a data frame as a report table for an Rmd
#'
#' Converts a data frame to a pandoc grid table ready for `cat()` in a chunk
#' with `results = "asis"`. Cells are converted to text with `NA` shown blank,
#' and a leading `-`, `+` or `*` is escaped so pandoc does not read it as a
#' list bullet. When any first-column cell is indented with [indent()], the
#' rows that are not indented are shown in bold, and those with no values in
#' the other columns become full-width banner rows.
#'
#' @param df A data frame.
#' @param caps Maximum column widths in characters (see [col_widths()]).
#'   Default 44 for the first column and 20 for the rest.
#' @param caption Optional table caption.
#' @param col_names Optional column headings, one per column. Default the
#'   column names of `df`.
#' @param full_width When `TRUE` (default), a table narrower than 96
#'   characters has its column widths scaled up in proportion, so Word
#'   stretches it to the full page width.
#' @return A single string holding the table (and caption).
#' @examples
#' tab <- data.frame(Characteristic = c("Sex", indent(c("Male", "Female"))),
#'                   Total = c("", n_pct(c(6, 4), c(10, 10))))
#' cat(render_table(tab, caption = "Baseline characteristics"))
#' @export
render_table <- function(df, caps = NULL, caption = NULL, col_names = NULL,
                         full_width = TRUE) {
  FULL_WIDTH_CHARS <- 96L
  if (!is.data.frame(df)) cli::cli_abort("{.arg df} must be a data frame.")
  n_col <- ncol(df)
  cells <- as.data.frame(lapply(df, function(col) {
    out <- as.character(col)
    out[is.na(out)] <- ""
    escape_list_marker(out)
  }), check.names = FALSE, stringsAsFactors = FALSE)
  if (!is.null(col_names)) {
    if (length(col_names) != n_col)
      cli::cli_abort("{.arg col_names} must have one name per column ({n_col}).")
    names(cells) <- col_names
  }

  span_rows <- NULL
  if (any(startsWith(cells[[1]], NBSP))) {
    heading <- !startsWith(cells[[1]], NBSP) & nzchar(cells[[1]])
    cells[[1]][heading] <- vapply(strsplit(cells[[1]][heading], "\n", fixed = TRUE),
                                  function(f) paste0("**", f, "**", collapse = "\n"),
                                  character(1))
    no_values <- if (n_col > 1L) rowSums(cells[-1] != "") == 0L else rep(TRUE, nrow(cells))
    span_rows <- heading & no_values
  }

  widths <- col_widths(cells, caps %||% c(44L, rep(20L, n_col - 1L)))
  total <- sum(widths) + 3L * n_col + 1L
  if (full_width && total < FULL_WIDTH_CHARS)
    widths <- ceiling(widths * (FULL_WIDTH_CHARS - 3L * n_col - 1L) / sum(widths))

  table <- grid_table(cells, widths, span_rows)
  if (!is.null(caption)) table <- paste0("Table: ", caption, "\n\n", table)
  table
}
