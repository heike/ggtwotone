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
#' @param smooth Use smooth dual-stroke curves (`geom_path`) instead of segmented curves (`geom_curve_dual`). Default is FALSE.
#' @param ... Additional arguments passed to `geom_curve_dual()`.
#'
#' @return A `ggplot2` layer with curved segments.
#'
#' @rdname geom_curve_dual_function
#'
#' @examples
#' library(ggplot2)
#'
#' base <- ggplot() + xlim(-2.05,2.05)
#' base +
#'   geom_curve_dual_function(
#'   fun = function(x) 0.5 * exp(-abs(x)),
#'   xlim = c(-2, 2),
#'   color1 = "#EEEEEE",
#'   color2 = "#222222",
#'   offset = 0.004,
#'   linewidth = 1.2,
#'   smooth = TRUE
#'   ) +
#'   theme_dark()
#'
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
#'     linewidth = 1.5,
#'     smooth = TRUE
#'   ) +
#'   geom_curve_dual_function(
#'     fun = dt,
#'     args = list(df = 1),
#'     xlim = c(-5, 5),
#'     color1  = pair2$light,
#'     color2 = pair2$dark,
#'     offset = 0.003,
#'     linewidth = 1.5,
#'     smooth = TRUE
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
                                     offset = 0.003,
                                     linewidth = 1.2,
                                     args = list(),
                                     smooth = FALSE,
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

  if (smooth) {
    dx <- diff(x_vals)
    dy <- diff(y_vals)
    len <- sqrt(dx^2 + dy^2)
    unit_dx <- dx / len
    unit_dy <- dy / len
    perp_x <- -unit_dy
    perp_y <-  unit_dx
    mid_x <- 0.5 * (head(x_vals, -1) + tail(x_vals, -1))
    perp_x <- stats::approx(mid_x, perp_x, xout = x_vals, rule = 2)$y
    perp_y <- stats::approx(mid_x, perp_y, xout = x_vals, rule = 2)$y

    data_up <- data.frame(x = x_vals + offset * perp_x, y = y_vals + offset * perp_y)
    data_dn <- data.frame(x = x_vals - offset * perp_x, y = y_vals - offset * perp_y)

    list(
      ggplot2::geom_path(data = data_dn, aes(x = x, y = y), color = color2, linewidth = linewidth, ...),
      ggplot2::geom_path(data = data_up, aes(x = x, y = y), color = color1, linewidth = linewidth, ...)
    )
  } else {
    data <- data.frame(
      x    = head(x_vals, -1),
      y    = head(y_vals, -1),
      xend = tail(x_vals, -1),
      yend = tail(y_vals, -1)
    )
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
}
