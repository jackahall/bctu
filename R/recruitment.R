################################################################################
#                                                                              #
#   Package:      bctu                                                         #
#   Script:       recruitment.R                                                #
#   Description:  Monthly counts with every calendar month present, for        #
#                 recruitment figures and tables.                              #
#                                                                              #
#   Author:       Jack Hall                                                    #
#   Email:        j.a.hall.1@bham.ac.uk                                        #
#                                                                              #
################################################################################

#' Count dates by calendar month, filling months with no events
#'
#' Floors each date to the first of its month and counts them, with a row
#' for every month from `from` to `to` inclusive. Months where nothing
#' happened get `n = 0` rather than being left out, so a recruitment plot
#' or table shows the real gaps. Missing dates are dropped. Dates outside
#' `from` to `to` are not counted.
#'
#' @param dates A Date vector (or anything [as.Date()] accepts).
#' @param from First month to report. Default the earliest date.
#' @param to Last month to report. Default the latest date. Set it to the
#'   data-freeze month to run the series to the freeze.
#' @returns A data frame with one row per month: `month` (Date, the first of
#'   the month), `n`, `cum_n` and `cum_pct` (100 * `cum_n` / total counted).
#' @examples
#' count_by_month(as.Date(c("2026-01-15", "2026-03-02", "2026-03-20")))
#' @export
count_by_month <- function(dates, from = min(dates), to = max(dates)) {
  dates <- as.Date(dates)
  dates <- dates[!is.na(dates)]
  if (!length(dates))
    cli::cli_abort("{.arg dates} has no non-missing dates to count.")
  first_of_month <- function(x) as.Date(format(as.Date(x), "%Y-%m-01"))
  from <- first_of_month(from)
  to <- first_of_month(to)
  if (is.na(from) || is.na(to))
    cli::cli_abort("{.arg from} and {.arg to} must each be a single date.")
  if (to < from)
    cli::cli_abort("{.arg to} ({to}) is before {.arg from} ({from}).")
  months <- seq(from, to, by = "month")
  counted <- first_of_month(dates)
  counted <- counted[counted >= from & counted <= to]
  n <- as.integer(table(factor(format(counted, "%Y-%m"), levels = format(months, "%Y-%m"))))
  cum_n <- cumsum(n)
  data.frame(month = months, n = n, cum_n = cum_n,
             cum_pct = if (sum(n)) 100 * cum_n / sum(n) else rep(NA_real_, length(n)))
}
