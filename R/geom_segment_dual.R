GeomSegmentDual <- ggplot2::ggproto("GeomSegmentDual", ggplot2::Geom,
                           required_aes = c("x", "y", "xend", "yend"),
                           default_aes = ggplot2::aes(
                             color1 = "black",
                             color2 = "white",
                             linewidth = 0.5,
                             linetype = 1,
                             alpha = NA
                           ),
                           draw_key = ggplot2::draw_key_path,

                           draw_panel = function(self, data, panel_params, coord, arrow = NULL,
                                                 arrow.fill = NULL, lineend = "butt", linejoin = "round",
                                                 na.rm = FALSE) {


                             coords <- coord$transform(data, panel_params)

                             pt_width = grid::convertWidth(unit(.5 * .pt, "pt"), "npc", valueOnly = TRUE)
                             pt_height = grid::convertHeight(unit(.5 * .pt, "pt"), "npc", valueOnly = TRUE)

                            coords <- coords |> dplyr::mutate(
                              dx = (xend - x),
                              dy = (yend - y),
                              len = sqrt(dx^2+dy^2),
                              angle = asin(dy/len),
                              offset_x = linewidth/2*pt_width*sin(angle),
                              offset_y = linewidth/2*pt_height*cos(angle)
                            )

                             data1 <- coords  # top stroke
                             data2 <- coords  # bottom stroke
                             data1$x    <- data1$x + data1$offset_x
                             data1$xend <- data1$xend + data1$offset_x
                             data1$y    <- data1$y - sign(data1$dx)*data1$offset_y
                             data1$yend <- data1$yend - sign(data1$dx)* data1$offset_y
                             data2$x    <- data2$x - data2$offset_x
                             data2$xend <- data2$xend - data2$offset_x
                             data2$y    <- data2$y + sign(data1$dx)* data2$offset_y
                             data2$yend <- data2$yend + sign(data1$dx)* data2$offset_y
                             coords1 <- data1 #coord$transform(data1, panel_params)
                             coords2 <- data2 #coord$transform(data2, panel_params)


                             alpha1 <- if (all(is.na(coords1$alpha))) NULL else coords1$alpha
                             alpha2 <- if (all(is.na(coords2$alpha))) NULL else coords2$alpha

                             # Bottom stroke (darker, drawn first and slightly thicker)
                             s2 <- grid::segmentsGrob(
                               x0 = coords2$x, x1 = coords2$xend,
                               y0 = coords2$y, y1 = coords2$yend,
                               gp = grid::gpar(
                                 col = coords2$colour2,
                                 lwd = (coords2$linewidth * 0.5) * .pt,
                                 lty = coords2$linetype,
                                 alpha = alpha2,
                                 lineend = lineend
                               ),
                               arrow = arrow
                             )

                             # Top stroke (lighter)
                             s1 <- grid::segmentsGrob(
                               x0 = coords1$x, x1 = coords1$xend,
                               y0 = coords1$y, y1 = coords1$yend,
                               gp = grid::gpar(
                                 col = coords1$colour1,
                                 lwd = (coords1$linewidth * 0.5) * .pt,
                                 lty = coords1$linetype,
                                 alpha = alpha1,
                                 lineend = lineend
                               ),
                               arrow = arrow
                             )

                             grid::gList(s2, s1)
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
#' library(ggplot2)
#' # Simple black background test
#' ggplot(data.frame(x = 1, xend = 2, y = 1, yend = 2),
#'        aes(x = x, y = y, xend = xend, yend = yend)) +
#'   geom_segment_dual(color1 = "white", color2 = "black", linewidth = 1.5) +
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
                              ..., arrow = NULL, arrow.fill = NULL,
                              lineend = "butt", linejoin = "round",
                              na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  layer(
    geom = GeomSegmentDual, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      arrow = arrow,
      arrow.fill = arrow.fill,
      lineend = lineend,
      linewidth = linewidth,
      linejoin = linejoin,
      color1 = color1,
      color2 = color2,
      na.rm = na.rm,
      ...
    )
  )
}

