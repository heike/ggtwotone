test_that("geom_lm_dual works with simple linear data", {
  df <- data.frame(x = 1:10, y = 2 * (1:10) + rnorm(10))

  p <- ggplot2::ggplot(df, aes(x, y)) +
    ggplot2::geom_point() +
    geom_lm_dual(data = df, mapping = aes(x = x, y = y))

  expect_silent(ggplot2::ggplot_build(p))
})

test_that("geom_lm_dual produces a GeomSegmentDual layer", {
  df <- data.frame(x = 1:10, y = 1:10 + rnorm(10))

  p <- ggplot2::ggplot(df, aes(x, y)) +
    geom_lm_dual(data = df, mapping = aes(x = x, y = y))

  layers <- ggplot2::ggplot_build(p)$plot$layers
  expect_s3_class(layers[[1]]$geom, "GeomSegmentDual")
})

test_that("geom_lm_dual warns on invalid base_color", {
  df <- data.frame(x = 1:10, y = 1:10 + rnorm(10))

  expect_warning(
    ggplot2::ggplot(df, aes(x, y)) +
      geom_lm_dual(data = df, mapping = aes(x = x, y = y),
                   base_color = "notacolor"),
    regexp = "Could not convert base color.*notacolor"
  )
})

test_that("geom_lm_dual handles NA values in data", {
  df <- data.frame(x = c(1:5, NA), y = c(2:6, NA))
  expect_silent(
    ggplot2::ggplot(df, aes(x, y)) +
      geom_lm_dual(data = df, mapping = aes(x = x, y = y))
  )
})
