test_that("geom_curve_dual works with minimal input", {
  df <- data.frame(x = 1, y = 1, xend = 2, yend = 2)

  p <- ggplot2::ggplot(df) +
    geom_curve_dual(aes(x = x, y = y, xend = xend, yend = yend))

  expect_silent(ggplot2::ggplot_build(p))
})

test_that("geom_curve_dual applies custom stroke colors", {
  df <- data.frame(x = 1, y = 1, xend = 3, yend = 3)

  p <- ggplot2::ggplot(df) +
    geom_curve_dual(
      aes(x = x, y = y, xend = xend, yend = yend),
      color1 = "yellow", color2 = "blue"
    )

  built <- ggplot2::ggplot_build(p)
  expect_s3_class(built$plot$layers[[1]]$geom, "GeomCurveDual")
})

test_that("geom_curve_dual skips rows with NA gracefully", {
  df <- data.frame(
    x = c(1, NA),
    y = c(1, 2),
    xend = c(3, 4),
    yend = c(3, NA)
  )

  p <- ggplot2::ggplot(df) +
    geom_curve_dual(aes(x = x, y = y, xend = xend, yend = yend))

  expect_silent(ggplot2::ggplot_build(p))
})

test_that("geom_curve_dual throws error when xend or yend are missing", {
  df <- data.frame(x = 1, y = 1)

  p <- ggplot2::ggplot(df) +
    geom_curve_dual(aes(x = x, y = y))  # Missing xend, yend

  expect_error(ggplot2::ggplot_build(p), regexp = "xend|yend|missing")  # Error should happen here
})

