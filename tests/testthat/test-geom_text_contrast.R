test_that("geom_text_contrast works with basic grayscale backgrounds", {
  df <- data.frame(
    x = 1:3,
    y = 1,
    label = LETTERS[1:3],
    fill = c("#000000", "#999999", "#FFFFFF")
  )

  p <- ggplot2::ggplot(df, aes(x, y)) +
    ggplot2::geom_tile(aes(fill = fill), width = 1, height = 1) +
    geom_text_contrast(
      aes(label = label),
      background = df$fill,
      size = 5
    )

  expect_silent(ggplot2::ggplot_build(p))
})

test_that("geom_text_contrast handles NA values in background", {
  df <- data.frame(
    x = 1:3,
    y = 1,
    label = LETTERS[1:3],
    fill = c("#000000", NA, "#FFFFFF")
  )

  p <- ggplot2::ggplot(df, aes(x, y)) +
    ggplot2::geom_tile(aes(fill = fill)) +
    geom_text_contrast(
      aes(label = label),
      background = df$fill
    )

  expect_silent(ggplot2::ggplot_build(p))
})

test_that("geom_text_contrast works with custom text color set in aes", {
  df <- data.frame(
    x = 1:2,
    y = 1,
    label = LETTERS[1:2],
    fill = c("#000000", "#FFFFFF"),
    color = c("red", "blue")
  )

  p <- ggplot2::ggplot(df, aes(x, y, color = color)) +
    geom_text_contrast(
      aes(label = label),
      background = df$fill
    )

  expect_silent(ggplot2::ggplot_build(p))
})
