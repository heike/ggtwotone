# Grob for drawing a line segment in two (contrasting) colours side by side
two_colour_segment_grob <- function(x0, y0, x1, y1, col1, col2, lwd, lineend, arrow, arrow.fill) {
  # consider current view port size for the initial creation
  # very differently shaped view ports are going to show subtle differences
  dx <-  grid::convertWidth(unit(x1 - x0, "npc"), "pt", valueOnly = TRUE)
  dy <- grid::convertHeight(unit((y1 - y0) , "npc"), "pt", valueOnly = TRUE)

  len <- sqrt(dx^2 + dy^2)
  perp_x <- -dy / len   # in data scale
  perp_y <- dx / len


  offset_amt <- 0.75 * lwd / 4 # in points # add an additional 25% to avoid gaps
  offset_x <- unit(perp_x * offset_amt,  "pt") # works for a square display
  offset_y <- unit(perp_y * offset_amt,  "pt")


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
  non_missing_aes = c("linetype", "linewidth", "colour"),

  default_aes = ggplot2::aes(
    colour = ggplot2::from_theme(colour),
    colour2 = adjust_contrast_pair("#303030")$light,
    linewidth = ggplot2::from_theme(linewidth),
    linetype = ggplot2::from_theme(linetype),
    alpha = NA
  ),
  draw_key = function (data, params, size) {
    if (is.null(data$linetype)) {
      data$linetype <- 0
    }

    pair <- adjust_contrast_pair(data$colour)
    grob <- two_colour_segment_grob(0.1, 0.5, 0.9, 0.5,
                col1 = alpha(pair$dark %||% data$fill %||% "black", data$alpha),
                col2 = alpha(pair$light %||% data$fill %||% "black", data$alpha),
             #   fill = alpha(params$arrow.fill %||% pair$dark %||% data$fill %||% "black", data$alpha),
                arrow.fill = params$arrow.fill,
                lwd = .pt*(data$linewidth %||% 0.5),
            #    lty = data$linetype %||% 1,
                lineend = params$lineend %||% "butt",
              arrow = params[["arrow"]])
    if (!is.null(params[["arrow"]])) {
      angle <- deg2rad(params[["arrow"]]$angle)
      length <- convertUnit(params[["arrow"]]$length, "cm",
                            valueOnly = TRUE)
      attr(grob, "width") <- cos(angle) * length * 1.25
      attr(grob, "height") <- sin(angle) * length * 2
    }
    grob
  },

  draw_panel = function(self, data, panel_params, coord,
                        lineend = "butt", arrow = NULL, arrow.fill = NULL) {
    coords <- coord$transform(data, panel_params)

    #vp_width_in <- convertWidth(unit(1, "npc"), "in", valueOnly = TRUE)
    #vp_height_in <- convertHeight(unit(1, "npc"), "in", valueOnly = TRUE)


    grobs <- lapply(seq_len(nrow(coords)), function(i) {
      row <- coords[i, , drop = FALSE]
      # Convert linewidth to lwd (pt) for grid
      lwd <- row$linewidth * .pt
      pair <- adjust_contrast_pair(row$colour)

      two_colour_segment_grob(
        x0 = row$x, y0 = row$y,
        x1 = row$xend, y1 = row$yend,
        col1 = alpha(pair$dark, row$alpha),
        col2 = alpha(pair$light, row$alpha),
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
#' Draws two vertically offset line segments with separate colours,
#' for visibility on varied backgrounds.
#'
#' @title Dual-Stroke Line Segments with Vertical Offset
#' @description Draws two vertically offset line segments with separate colours,
#' for visibility on varied backgrounds.
#'
#' @inheritParams ggplot2::geom_segment
#' @param colour1 colour for the top (upward-shifted) stroke.
#' @param colour2 colour for the bottom (downward-shifted) stroke.
#' @param linewidth Width of each line stroke (in mm).
#' @param aspect_ratio Aspect ratio hint (currently unused by the grob logic but reserved for future layout tuning).
#'
#'
#' @examples
#' # Simple black background test
#' ggplot(data.frame(x = 1, xend = 2, y = 1, yend = 2),
#'        aes(x = x, y = y, xend = xend, yend = yend)) +
#'   geom_segment_dual(colour1 = "white", colour2 = "black", linewidth = 2) +
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
#'   colour1 = c("#D9D9D9", "#D9D9D9"),  # light stroke
#'   colour2 = c("#333333", "#333333")   # dark stroke
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
#'     colour1 = line_data$colour1,
#'     colour2 = line_data$colour2,
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
#'   colour1 = rep("white", 3),
#'   colour2 = rep("black", 3),
#'   group = factor(c("A", "B", "C"))
#' )
#'
#' ggplot(df) +
#'   geom_segment_dual(
#'     aes(x = x, y = y, xend = xend, yend = yend, group = group),
#'     colour1 = df$colour1,
#'     colour2 = df$colour2,
#'     linewidth = 1,
#'     arrow = arrow(length = unit(0.15, "inches"), type = "closed")
#'   ) +
#'   coord_fixed() +
#'   theme_dark()
#' @export
geom_segment_dual <- function(mapping = NULL, data = NULL,
                              stat = "identity", position = "identity",
                            #  colour1 = NULL, colour2 = NULL,
                            linewidth = NULL,
                              lineend = "butt", aspect_ratio = 1,
                              ..., arrow = NULL, arrow.fill = NULL,
                              na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  layer(
    geom = "segment_dual", mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      arrow = arrow,
      arrow.fill = arrow.fill,
      lineend = lineend,
      linewidth = linewidth,
   #   colour1 = colour1,
   #    colour2 = colour2,
      na.rm = na.rm,
      ...
    )
  )
}
