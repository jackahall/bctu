################################################################################
#                                                                              #
#   Package:      bctu                                                         #
#   Script:       reason_maps.R                                                #
#   Description:  Reason maps: CSV files mapping raw free-text reasons to      #
#                 hand-cleaned text, kept up to date as new reasons appear.    #
#                                                                              #
#   Author:       Jack Hall                                                    #
#   Email:        j.a.hall.1@bham.ac.uk                                        #
#                                                                              #
################################################################################

#' Collapse whitespace in free text
#'
#' Replaces every run of whitespace (spaces, tabs, line breaks) with a single
#' space and trims both ends. `NA` stays `NA`.
#'
#' @param x A character vector.
#' @return A character vector the same length as `x`.
#' @examples
#' squash_text("  Moved\r\n away  from   area ")
#' @export
squash_text <- function(x) {
  trimws(gsub("\\s+", " ", x, perl = TRUE))
}

#' One string per row of key columns, used to match keys between tables
#'
#' `NA` keys match as blank text.
#' @param keys A data frame of key columns.
#' @return A character vector with one value per row.
#' @keywords internal
reason_map_row_keys <- function(keys) {
  cols <- lapply(keys, function(col) {
    out <- as.character(col)
    out[is.na(out)] <- ""
    out
  })
  do.call(paste, c(unname(cols), sep = "\u001f"))
}

#' Add new raw reasons to a reason map CSV
#'
#' A reason map is a CSV file with one row per distinct raw reason (the key
#' columns) and a `clean` column holding the hand-cleaned text. This reads the
#' map at `path` (or starts an empty one), appends a row with a blank `clean`
#' value for every key row not already in it, writes the file back and returns
#' the full map. Existing rows and their `clean` values are kept as they are.
#' Fill in the blank `clean` cells by hand, then use [map_clean()].
#'
#' @param path Path to the reason map CSV. Its folder is created if needed.
#' @param keys A data frame of key columns, for example one column `raw`, or
#'   `category` and `raw`. The last column holds the raw text.
#' @param verbose Verbosity: `0` silent, `1` or more reports the number of
#'   rows added and the file path.
#' @return The full reason map as a data frame of character columns.
#' @examples
#' path <- file.path(tempdir(), "withdrawal_reasons.csv")
#' update_reason_map(path, data.frame(raw = c("moved away", "too busy")))
#' @export
update_reason_map <- function(path, keys, verbose = 2L) {
  if (!is.data.frame(keys) || !ncol(keys))
    cli::cli_abort("{.arg keys} must be a data frame with at least one key column.")
  if ("clean" %in% names(keys))
    cli::cli_abort("{.arg keys} must not have a column called {.val clean}; that name is the map's cleaned-text column.")
  key_names <- names(keys)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  if (file.exists(path)) {
    map <- utils::read.csv(path, colClasses = "character", check.names = FALSE,
                           na.strings = character(0), fileEncoding = "UTF-8")
    missing <- setdiff(c(key_names, "clean"), names(map))
    if (length(missing))
      cli::cli_abort(c("The reason map {.file {path}} has no column{?s} {.val {missing}}.",
                       "i" = "Its columns must be the key columns ({.val {key_names}}) and {.val clean}."))
  } else {
    map <- as.data.frame(stats::setNames(rep(list(character(0)), length(key_names) + 1L),
                                         c(key_names, "clean")),
                         check.names = FALSE)
  }

  keys <- as.data.frame(lapply(keys, as.character), check.names = FALSE)
  keys <- keys[!duplicated(reason_map_row_keys(keys)), , drop = FALSE]
  new_rows <- keys[!reason_map_row_keys(keys) %in% reason_map_row_keys(map[key_names]), ,
                   drop = FALSE]
  if (nrow(new_rows)) {
    new_rows$clean <- ""
    extra <- setdiff(names(map), names(new_rows))
    new_rows[extra] <- ""
    map <- rbind(map, new_rows[names(map)])
  }
  rownames(map) <- NULL

  utils::write.csv(map, path, row.names = FALSE, na = "", fileEncoding = "UTF-8")
  if (verbose >= 1L)
    cli::cli_alert_info("Reason map {.file {path}}: {nrow(new_rows)} new row{?s} added, {nrow(map)} in total.")
  map
}

#' Look up cleaned reasons in a reason map
#'
#' Matches each row of `keys` to the reason map on the key columns and
#' returns the map's `clean` text. Where the map has no row for a key, or its
#' `clean` cell is blank, the raw text (the last key column) is returned
#' unchanged.
#'
#' @param keys A data frame of key columns, as passed to
#'   [update_reason_map()].
#' @param map A reason map, as returned by [update_reason_map()].
#' @return A character vector with one value per row of `keys`.
#' @examples
#' path <- file.path(tempdir(), "withdrawal_reasons.csv")
#' keys <- data.frame(raw = c("moved away", "too busy"))
#' map <- update_reason_map(path, keys)
#' map$clean[1] <- "Relocated"
#' map_clean(keys, map)
#' @export
map_clean <- function(keys, map) {
  if (!is.data.frame(keys) || !ncol(keys))
    cli::cli_abort("{.arg keys} must be a data frame with at least one key column.")
  missing <- setdiff(c(names(keys), "clean"), names(map))
  if (length(missing))
    cli::cli_abort("{.arg map} has no column{?s} {.val {missing}}.")
  clean <- map$clean[match(reason_map_row_keys(keys), reason_map_row_keys(map[names(keys)]))]
  raw <- as.character(keys[[ncol(keys)]])
  filled <- !is.na(clean) & nzchar(clean)
  raw[filled] <- clean[filled]
  raw
}
