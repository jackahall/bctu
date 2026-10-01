test_that("trial_slides builds a deck on the UoB template", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  skip_if_not_installed("ggplot2")
  dir <- withr::local_tempdir()
  tab <- data.frame(Item = c("Section", indent(c("First", "Second"))), n = c("", "1", "2"), check.names = FALSE)
  plot <- ggplot2::ggplot(data.frame(x = 1:3, y = 1:3), ggplot2::aes(x, y)) + ggplot2::geom_point()
  out <- trial_slides(file.path(dir, "deck.pptx"), "TEST", "Subtitle",
                      list(slide_section("Part"), slide_content("A figure", plot),
                           slide_content("A table", tab, note = "A note.")))
  deck <- officer::read_pptx(out)
  expect_identical(length(deck), 4L)
  expect_identical(officer::slide_summary(deck, index = 4)$text[grepl("A table|A note", officer::slide_summary(deck, index = 4)$text)],
                   c("A table", "A note."))
  big <- data.frame(Item = paste("Row", 1:40), n = "1")
  long <- officer::read_pptx(trial_slides(file.path(dir, "big.pptx"), "TEST", NULL, list(slide_content("Long", big))))
  expect_gt(length(long), 2L)                       # the title slide plus continuation slides
  expect_true("Long (continued)" %in% officer::slide_summary(long, index = 3)$text)
  expect_error(slide_content("Bad", 1:3), "ggplot or a data frame")
  expect_error(slide_content("Small", big, font_size = 10), "at least 14")
})
