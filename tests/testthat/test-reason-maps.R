test_that("squash_text collapses whitespace runs and keeps NA", {
  expect_equal(squash_text(c("  a\r\n b\t\tc ", NA)), c("a b c", NA))
})

test_that("update_reason_map creates the CSV on the first call", {
  path <- file.path(withr::local_tempdir(), "maps", "reasons.csv")
  map <- update_reason_map(path, data.frame(raw = c("moved", "busy", "moved")),
                           verbose = 0L)
  expect_true(file.exists(path))
  expect_equal(names(map), c("raw", "clean"))
  expect_equal(map$raw, c("moved", "busy"))
  expect_equal(map$clean, c("", ""))
})

test_that("update_reason_map appends only new keys and keeps filled clean values", {
  path <- file.path(withr::local_tempdir(), "reasons.csv")
  keys <- data.frame(category = c("withdrawal", "withdrawal"), raw = c("moved", "busy"))
  map <- update_reason_map(path, keys, verbose = 0L)
  map$clean[map$raw == "moved"] <- "Relocated"
  utils::write.csv(map, path, row.names = FALSE)

  more <- data.frame(category = c("withdrawal", "death", "withdrawal"),
                     raw = c("moved", "busy", "NA"))
  map2 <- update_reason_map(path, more, verbose = 0L)
  expect_equal(nrow(map2), 4L)
  expect_equal(map2$raw, c("moved", "busy", "busy", "NA"))
  expect_equal(map2$category, c("withdrawal", "withdrawal", "death", "withdrawal"))
  expect_equal(map2$clean, c("Relocated", "", "", ""))
  expect_equal(nrow(update_reason_map(path, more, verbose = 0L)), 4L)
})

test_that("update_reason_map rejects a map without the key columns", {
  path <- file.path(withr::local_tempdir(), "reasons.csv")
  update_reason_map(path, data.frame(raw = "a"), verbose = 0L)
  expect_error(update_reason_map(path, data.frame(reason = "a"), verbose = 0L),
               "has no column")
})

test_that("map_clean returns clean text where filled and the raw text otherwise", {
  keys <- data.frame(category = c("w", "w", "d"), raw = c("moved", "busy", "other"))
  map <- data.frame(category = c("w", "w"), raw = c("moved", "busy"),
                    clean = c("Relocated", ""))
  expect_equal(map_clean(keys, map), c("Relocated", "busy", "other"))
})
