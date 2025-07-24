#' Dual-Tone Curved Function Lines
#'
#' Draws a function (e.g., density or mathematical curve) using perceptually offset dual-stroke curved line segments.
#'
#' @param fun A function to evaluate (e.g., `dnorm`, `dt`).
#' @param xlim Range of x-values to evaluate over (numeric vector of length 2).
#' @param n Number of segments to compute (default: 201).
#' @param curvature,angle,ncp Passed to underlying `geom_curve_dual` segments.
#' @param color1 Top stroke color (typically light).
#' @param color2 Bottom stroke color (typically dark). If NULL, will be auto-computed for contrast.
#' @param background Background color used for contrast calculation (default: `"#000000"`).
#' @param contrast_method Either "apca", "wcag", or "auto".
#' @param offset Perpendicular offset between strokes.
#' @param linewidth Stroke width for the top line.
#' @param args List of arguments passed to `fun` (for example, list(df = 1) for `dt`).
#' @param ... Additional arguments passed to `geom_curve_dual()`.
#'
#' @return A `ggplot2` layer with curved segments.
#'
#' @rdname geom_curve_dual_function
#'
#' @examples
#' library(ggplot2)
#' pair1 <- adjust_contrast_pair("#FFFFFF", background = "black", method = "APCA")
#' pair2 <- adjust_contrast_pair("#AAAAAA", background = "black", method = "APCA")
#'
#' ggplot() +
#'   geom_curve_dual_function(
#'     fun = dnorm,
#'     xlim = c(-5, 5),
#'     color1  = pair1$light,
#'     color2 = pair1$dark,
#'     offset = 0.003,
#'     linewidth = 1
#'   ) +
#'   geom_curve_dual_function(
#'     fun = dt,
#'     args = list(df = 1),
#'     xlim = c(-5, 5),
#'     color1  = pair2$light,
#'     color2 = pair2$dark,
#'     offset = 0.003,
#'     linewidth = 2
#'   ) +
#'   theme_dark()
#'
#' @export
geom_curve_dual_function <- function(fun,
                                     xlim = c(-3, 3),
                                     n = 701,
                                     curvature = 0,
                                     angle = 90,
                                     ncp = 20,
                                     color1 = "#FFFFFF",
                                     color2 = NULL,
                                     background = "#000000",
                                     contrast_method = "APCA",
                                     offset = 0.002,
                                     linewidth = 1.2,
                                     args = list(),
                                     ...) {
  x_vals <- seq(xlim[1], xlim[2], length.out = n)

  # Evaluate the function
  y_vals <- tryCatch({
    do.call(fun, c(list(x_vals), args))
  }, error = function(e) {
    warning("Could not evaluate function with given args: ", e$message)
    rep(NA, length(x_vals))
  })

  # Remove any non-finite points BEFORE segmenting
  keep <- is.finite(x_vals) & is.finite(y_vals)
  x_vals <- x_vals[keep]
  y_vals <- y_vals[keep]

  if (length(x_vals) < 2) {
    warning("Not enough finite values to draw curve.")
    return(ggplot2::geom_blank())
  }

  # Segment into line segments
  data <- data.frame(
    x    = head(x_vals, -1),
    y    = head(y_vals, -1),
    xend = tail(x_vals, -1),
    yend = tail(y_vals, -1)
  )

  # Remove any rows with NA or Inf (just in case)
  data <- tidyr::drop_na(data)

  if (!is.null(color1) && is.null(color2)) {
    pair <- adjust_contrast_pair(color1, background = background, method = contrast_method, quiet = TRUE)
    color2 <- pair$dark
  }

  geom_curve_dual(
    data = data,
    mapping = aes(x = x, y = y, xend = xend, yend = yend),
    curvature = curvature,
    angle = angle,
    ncp = ncp,
    color1 = color1,
    color2 = color2,
    offset = offset,
    linewidth = linewidth,
    ...
  )
}
