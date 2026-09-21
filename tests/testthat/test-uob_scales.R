test_that("uob_palettes keeps the names and values trial code relies on", {
  expect_equal(names(uob_palettes$deep),
               c("blue", "orange", "green", "purple", "red", "turquoise", "pink", "grey"))
  expect_equal(unname(uob_palettes$deep),
               c("#0057BF", "#CF4527", "#007838", "#501D83",
                 "#C20019", "#00788E", "#DB0661", "#3C3C3B"))
  expect_equal(uob_palettes$categorical, uob_palettes$standard)
  expect_equal(unname(uob_palettes$cool),
               c("#0057BF", "#00788E", "#007838", "#501D83"))
})

test_that("pal_uob returns palette colours in order and validates its arguments", {
  expect_equal(pal_uob("deep")(8), unname(uob_palettes$deep))
  expect_equal(pal_uob()(3), unname(uob_palettes$standard)[1:3])
  expect_equal(pal_uob("deep", reverse = TRUE)(1), "#3C3C3B")
  expect_error(pal_uob("teal"), "Unknown palette")
  expect_error(pal_uob("deep", alpha = 0), "must be a single number")
  expect_error(pal_uob("deep", alpha = 1.5), "must be a single number")
})

test_that("alpha adds the alpha channel", {
  expect_equal(pal_uob("deep", alpha = 0.5)(1), "#0057BF7F")
  expect_equal(pal_uob("deep", alpha = 1)(1), "#0057BF")
})

test_that("more colours than the palette holds are interpolated", {
  nine <- pal_uob("deep")(9)
  expect_length(nine, 9L)
  expect_false(anyNA(nine))
  expect_equal(nine[1], "#0057BF")
  expect_equal(nine[9], "#3C3C3B")
})

test_that("uob_tint moves a colour toward white", {
  expect_equal(uob_tint("#0057BF", 1), "#0057BF")
  expect_equal(uob_tint("#0057BF", 0), "#FFFFFF")
})

test_that("the discrete scales build and give the palette colours", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(g = factor(c("a", "b", "c")), y = 1:3)
  built <- ggplot2::ggplot_build(
    ggplot2::ggplot(df, ggplot2::aes(g, y, fill = g)) +
      ggplot2::geom_col() + scale_fill_uob("deep"))
  expect_equal(built$data[[1]]$fill, c("#0057BF", "#CF4527", "#007838"))
  built_default <- ggplot2::ggplot_build(
    ggplot2::ggplot(df, ggplot2::aes(g, y, colour = g)) +
      ggplot2::geom_point() + scale_colour_uob())
  expect_equal(built_default$data[[1]]$colour, unname(uob_palettes$standard)[1:3])
  expect_identical(scale_colour_uob, scale_color_uob)
})

test_that("the continuous scales build", {
  skip_if_not_installed("ggplot2")
  expect_s3_class(scale_fill_uob_c(), "ggproto")
  expect_s3_class(scale_color_uob_c("mono_red"), "ggproto")
})

test_that("theme_bctu_report returns a ggplot theme", {
  skip_if_not_installed("ggplot2")
  th <- theme_bctu_report()
  expect_s3_class(th, "theme")
  expect_equal(th$legend.position, "bottom")
  expect_s3_class(theme_bctu_report(base_size = 15), "theme")
})
