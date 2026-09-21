test_that("count_by_month fills months with no events", {
  out <- count_by_month(as.Date(c("2026-01-15", "2026-03-02", "2026-03-20")))
  expect_equal(out$month, as.Date(c("2026-01-01", "2026-02-01", "2026-03-01")))
  expect_equal(out$n, c(1L, 0L, 2L))
  expect_equal(out$cum_n, c(1L, 1L, 3L))
  expect_equal(out$cum_pct, c(100 / 3, 100 / 3, 100))
})

test_that("to extends the series past the last date", {
  out <- count_by_month(as.Date(c("2026-01-15", "2026-02-02")), to = as.Date("2026-05-20"))
  expect_equal(nrow(out), 5L)
  expect_equal(out$n, c(1L, 1L, 0L, 0L, 0L))
  expect_equal(out$cum_n, c(1L, 2L, 2L, 2L, 2L))
  expect_equal(out$cum_pct, c(50, 100, 100, 100, 100))
})

test_that("from and to clip the window", {
  out <- count_by_month(as.Date(c("2025-12-01", "2026-01-15", "2026-02-02")),
                        from = as.Date("2026-01-10"), to = as.Date("2026-02-28"))
  expect_equal(out$month, as.Date(c("2026-01-01", "2026-02-01")))
  expect_equal(out$n, c(1L, 1L))
})

test_that("missing dates are dropped and an all-missing input errors", {
  out <- count_by_month(as.Date(c("2026-01-15", NA, "2026-01-20")))
  expect_equal(out$n, 2L)
  expect_error(count_by_month(as.Date(c(NA, NA))), "no non-missing dates")
  expect_error(count_by_month(as.Date("2026-03-01"), to = as.Date("2026-01-01")), "is before")
})
