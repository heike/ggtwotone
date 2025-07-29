#' Adjust Contrast Between Two Stroke Colors
#'
#' Given a base color and a background, generate a pair of colors (light and dark)
#' with sufficient perceptual contrast using WCAG or APCA methods.
#'
#' @param color A base color in hexadecimal format (e.g., "#6699CC").
#' @param contrast Minimum desired contrast ratio (default is 4.5).
#' @param method Contrast method: "WCAG", "APCA", or "auto" to try both.
#' @param background Background color (default: "#FFFFFF").
#' @param quiet Logical. If TRUE, suppresses warnings.
#'
#' @return A list with elements `light`, `dark`, `contrast`, and `method`.
#'
#' @examples
#' adjust_contrast_pair("#777777", contrast = 4.5, method = "auto", background = "#000000")
#'
#' adjust_contrast_pair("#66CCFF", contrast = 4.5, method = "APCA", background = "#FAFAFA")
#' @export
adjust_contrast_pair <- function(color, contrast = 4.5, method = "auto", background = "#FFFFFF", quiet = FALSE) {
  if (!requireNamespace("colorspace", quietly = TRUE)) {
    stop("Package 'colorspace' is required for contrast adjustment.")
  }

  if (!is.character(color) || length(color) != 1 || is.na(color)) {
    warning("Invalid color input.")
    return(list(light = "#FFFFFF", dark = "#000000", contrast = NA))
  }

  base_hcl <- if (quiet) {
    tryCatch({
      suppressWarnings(as(colorspace::hex2RGB(color), "polarLUV")@coords)
    }, error = function(e) NULL)
  } else {
    tryCatch({
      as(colorspace::hex2RGB(color), "polarLUV")@coords
    }, error = function(e) {
      warning(sprintf("Could not convert base color %s to HCL.", color))
      NULL
    })
  }

  if (is.null(base_hcl) || !is.numeric(base_hcl) || length(base_hcl) < 2) {
    return(list(
      light = "#FFFFFF",
      dark = "#000000",
      contrast = NA,
      method = method
    ))
  }

  h <- base_hcl[1]; c <- base_hcl[2]
  try_l <- seq(20, 95, by = 5)
  candidates <- colorspace::hex(colorspace::polarLUV(L = try_l, C = c, H = h))

  get_contrast <- function(fg, bg, method) {
    result <- tryCatch({
      cr <- colorspace::contrast_ratio(fg, bg, algorithm = method)
      if (method == "APCA") {
        if (is.matrix(cr)) abs(cr[, "normal"]) else abs(cr["normal"])
      } else {
        cr
      }
    }, error = function(e) {
      warning(sprintf("computation failed for %s vs %s [%s]: %s", fg, bg, method, e$message))
      NA
    })
    return(result)
  }

  contrast_methods <- if (method == "auto") c("WCAG", "APCA") else method
  best_result <- NULL

  for (m in contrast_methods) {
    contrasts <- sapply(candidates, get_contrast, bg = background, method = m)
    best_light <- candidates[which.max(contrasts)]
    best_dark <- candidates[which.min(contrasts)]
    max_contrast <- max(contrasts, na.rm = TRUE)

    if (!is.na(max_contrast) && max_contrast >= contrast) {
      best_result <- list(light = best_light, dark = best_dark, contrast = max_contrast, method = m)
      break
    }
  }

  if (is.null(best_result)) {
    warning(sprintf("No sufficient contrast found for base color %s. Using fallback colors.", color))
    return(list(light = "#FFFFFF", dark = "#000000", contrast = NA, method = method))
  }

  return(best_result)
}
