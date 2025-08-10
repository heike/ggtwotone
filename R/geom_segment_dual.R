# Grob for drawing a line segment in two (contrasting) colors side by side
two_color_segment_grob <- function(x0, y0, x1, y1, col1, col2, lwd, lineend, arrow, arrow.fill) {
  # Adjust dy by aspect ratio to compensate for scaling difference
  dx <- x1 - x0
  dy <- (y1 - y0)

  len <- sqrt(dx^2 + dy^2)
  perp_x <- -dy / len
  perp_y <- dx / len


  offset_amt <- 0.8*lwd / 4 # add 10percent to avoid gaps due to rounding problems with pixels
  offset_x <-perp_x * offset_amt
  offset_y <-perp_y * offset_amt

  right_line <- segmentsGrob(
    x0 = unit(x0, "npc") - unit(offset_x, "pt"),
    y0 = unit(y0, "npc") - unit(offset_y, "pt"),
    x1 = unit(x1, "npc") - unit(offset_x, "pt"),
    y1 = unit(y1, "npc") - unit(offset_y, "pt"),
    gp = gpar(col = col1, lwd = lwd / 2, lineend = lineend, fill = arrow.fill %||% col1),
    arrow = arrow
  )
  left_line <- segmentsGrob(
    x0 = unit(x0, "npc") + unit(offset_x, "pt"),
    y0 = unit(y0, "npc") + unit(offset_y, "pt"),
    x1 = unit(x1, "npc") + unit(offset_x, "pt"),
    y1 = unit(y1, "npc") + unit(offset_y, "pt"),
    gp = gpar(col = col2, lwd = lwd / 2, lineend = lineend, fill = arrow.fill %||% col2),
    arrow = arrow
  )
  grid::gTree(children = grid::gList(right_line, left_line))
}

GeomSegmentDual <- ggplot2::ggproto(
  "GeomSegmentDual", ggplot2::Geom,
  required_aes = c("x", "y", "xend", "yend"),
  default_aes = ggplot2::aes(
    color1 = "black",
    color2 = "white",
    linewidth = ggplot2::from_theme(linewidth),
    linetype = ggplot2::from_theme(linetype),
    alpha = NA
  ),
  draw_key = ggplot2::draw_key_path,

  draw_panel = function(data, panel_params, coord,
                        lineend = "butt", arrow = NULL, arrow.fill = NULL) {

    coords <- coord$transform(data, panel_params)
    browser()

    grobs <- lapply(seq_len(nrow(coords)), function(i) {
      row <- coords[i, , drop = FALSE]
      # Convert linewidth to lwd (pt) for grid
      lwd <- row$linewidth * .pt

      two_color_segment_grob(
        x0 = row$x, y0 = row$y,
        x1 = row$xend, y1 = row$yend,
        col1 = row$colour1,
        col2 = row$colour2,
        lwd = lwd,
        lineend = lineend,
        arrow = arrow,
        arrow.fill = arrow.fill
      )
    })

    grobTree(children = do.call(gList, grobs))
  }
)

#' Dual-Stroke Line Segments with Vertical Offset
#'
#' Draws two vertically offset line segments with separate colors,
#' for visibility on varied backgrounds.
#'
#' @title Dual-Stroke Line Segments with Vertical Offset
#' @description Draws two vertically offset line segments with separate colors,
#' for visibility on varied backgrounds.
#'
#' @inheritParams ggplot2::geom_segment
#' @param color1 Color for the top (upward-shifted) stroke.
#' @param color2 Color for the bottom (downward-shifted) stroke.
#' @param linewidth Width of each line stroke (in mm).
#'
#'
#' @examples
#' # Simple black background test
#' ggplot(data.frame(x = 1, xend = 2, y = 1, yend = 2),
#'        aes(x = x, y = y, xend = xend, yend = yend)) +
#'   geom_segment_dual(color1 = "white", color2 = "black", linewidth = 2) +
#'   theme_void() +
#'   theme(panel.background = element_rect(fill = "gray20"))
#'
#' # Dual-stroke diagonal lines crossing contrasting backgrounds
#' bg <- data.frame(
#'   xmin = c(0, 5),
#'   xmax = c(5, 10),
#'   ymin = 0,
#'   ymax = 5,
#'   fill = c("black", "white")
#' )
#'
#' line_data <- data.frame(
#'   x = c(1, 9),
#'   y = c(1, 1),
#'   xend = c(9, 1),
#'   yend = c(4, 4),
#'   color1 = c("#D9D9D9", "#D9D9D9"),  # light stroke
#'   color2 = c("#333333", "#333333")   # dark stroke
#' )
#'
#' ggplot() +
#'   geom_rect(data = bg,
#'   aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill),
#'   inherit.aes = FALSE) +
#'   scale_fill_identity() +
#'   geom_segment_dual(
#'     data = line_data,
#'     aes(x = x, y = y, xend = xend, yend = yend),
#'     color1 = line_data$color1,
#'     color2 = line_data$color2,
#'     linewidth = 1,
#'     inherit.aes = FALSE
#'   ) +
#'   theme_void() +
#'   coord_fixed() +
#'   ggtitle("Two Diagonal Dual-Stroke Lines in Opposite Directions")
#'
#' # Multiple dual-stroke segments with arrowheads and grouping
#' df <- data.frame(
#'   x = c(1, 2, 3),
#'   xend = c(2, 3, 4),
#'   y = c(1, 2, 1),
#'   yend = c(2, 1, 2),
#'   color1 = rep("white", 3),
#'   color2 = rep("black", 3),
#'   group = factor(c("A", "B", "C"))
#' )
#'
#' ggplot(df) +
#'   geom_segment_dual(
#'     aes(x = x, y = y, xend = xend, yend = yend, group = group),
#'     color1 = df$color1,
#'     color2 = df$color2,
#'     linewidth = 1,
#'     arrow = arrow(length = unit(0.15, "inches"), type = "closed")
#'   ) +
#'   coord_fixed() +
#'   theme_dark()
#' @export
geom_segment_dual <- function(mapping = NULL, data = NULL,
                              stat = "identity", position = "identity",
                              color1 = NULL, color2 = NULL, linewidth = NULL,
                              lineend = "butt",
                              ..., arrow = NULL, arrow.fill = NULL,
                              na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  layer(
    geom = GeomSegmentDual, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      arrow = arrow,
      arrow.fill = arrow.fill,
      lineend = lineend,
      linewidth = linewidth,
      color1 = color1,
      color2 = color2,
      na.rm = na.rm,
      ...
    )
  )
}
