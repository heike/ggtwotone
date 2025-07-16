GeomTextContrast <- ggplot2::ggproto("GeomTextContrast", ggplot2::Geom,
                            required_aes = c("x", "y", "label", "background"),

                            default_aes = ggplot2::aes(
                              colour = NA,
                              size = 3.88,
                              angle = 0,
                              hjust = 0.5,
                              vjust = 0.5,
                              alpha = NA,
                              family = "",
                              fontface = 1,
                              lineheight = 1.2
                            ),

                            draw_key = ggplot2::draw_key_text,

                            draw_panel = function(self, data, panel_params, coord,
                                                  parse = FALSE, na.rm = FALSE,
                                                  method = "auto", contrast = 4.5) {

                              if (!requireNamespace("colorspace", quietly = TRUE)) {
                                stop("Package 'colorspace' is required for contrast-aware text.")
                              }

                              data <- transform(data,
                                                colour = vapply(
                                                  seq_len(nrow(data)),
                                                  function(i) {
                                                    bg <- data$background[i]
                                                    base <- if (!is.na(data$colour[i])) data$colour[i] else "#666666"
                                                    cp <- tryCatch(
                                                      adjust_contrast_pair(base, contrast = contrast, method = method, background = bg),
                                                      error = function(e) list(light = "#FFFFFF", dark = "#000000")
                                                    )
                                                    ifelse(colorspace::contrast_ratio(cp$light, bg) > colorspace::contrast_ratio(cp$dark, bg),
                                                           cp$light, cp$dark)
                                                  },
                                                  character(1)
                                                )
                              )

                              coords <- coord$transform(data, panel_params)

                              alpha_val <- if (all(is.na(coords$alpha))) NULL else coords$alpha

                              grid::textGrob(
                                label = coords$label,
                                x = coords$x, y = coords$y,
                                default.units = "native",
                                hjust = coords$hjust, vjust = coords$vjust,
                                rot = coords$angle,
                                gp = grid::gpar(
                                  col = coords$colour,
                                  fontsize = coords$size * .pt,
                                  fontface = coords$fontface,
                                  fontfamily = coords$family,
                                  lineheight = coords$lineheight,
                                  alpha = alpha_val
                                )
                              )
                            }
)

#' Contrast-Aware Text Geom
#'
#' Automatically adjusts text color for readability on varying background colors using WCAG or APCA contrast.
#'
#' @rdname geom_text_contrast
#' @inheritParams ggplot2::geom_text
#' @param background A character vector of background fill colors (hex codes), used for contrast computation.
#' @param method Contrast method to use: `"WCAG"`, `"APCA"`, or `"auto"`.
#' @param contrast Threshold to ensure between text and background (defaults to 4.5).
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' # Grayscale A–E tiles: testing contrast
#' df <- data.frame(
#'   x = 1:5,
#'   y = 1,
#'   label = LETTERS[1:5],
#'   fill = c("#000000", "#222222", "#666666", "#DDDDDD", "#FFFFFF")
#' )
#'
#' ggplot(df, aes(x, y)) +
#'   geom_tile(aes(fill = fill), width = 1, height = 1) +
#'   geom_text_contrast(
#'     aes(label = label),
#'     background = df$fill,
#'     size = 7
#'   ) +
#'   scale_fill_identity() +
#'   coord_fixed() +
#'   theme_void() +
#'   labs(title = "Contrast-Aware Text on Varying Backgrounds")
#'
#' # Simulated region × risk category
#' df_risk <- expand.grid(
#'   region = LETTERS[1:6],
#'   zone = paste0("Z", 1:6)
#' )
#'
#' df_risk$risk_level <- sample(
#'   c("Low", "Moderate", "High", "Critical", "Severe", "Extreme"),
#'   size = nrow(df_risk), replace = TRUE
#' )
#' df_risk$label <- paste(df_risk$region, df_risk$zone)
#'
#' risk_colors <- c(
#'   "Low" = "gray80",
#'   "Moderate" = "skyblue",
#'   "High" = "orange",
#'   "Critical" = "firebrick",
#'   "Severe" = "darkred",
#'   "Extreme" = "navy"
#' )
#'
#' df_risk$fill_color <- risk_colors[df_risk$risk_level]
#'
#' ggplot(df_risk, aes(x = region, y = zone, fill = risk_level)) +
#'   geom_tile(color = "white") +
#'   geom_text_contrast(
#'     aes(label = label),
#'     background = df_risk$fill_color,
#'     size = 3,
#'     fontface = "bold"
#'   ) +
#'   scale_fill_manual(values = risk_colors) +
#'   labs(
#'     title = "Simulated Risk Map (Auto Contrast Labels)",
#'     fill = "Risk Level"
#'   ) +
#'   theme_minimal()
geom_text_contrast <- function(mapping = NULL, data = NULL, stat = "identity",
                               position = "identity", ..., method = "auto",
                               contrast = 4.5, na.rm = FALSE,
                               show.legend = NA, inherit.aes = TRUE, background = NULL) {
  layer(
    geom = GeomTextContrast, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      method = method,
      contrast = contrast,
      na.rm = na.rm,
      background = background,
      ...
    )
  )
}


