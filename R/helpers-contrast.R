#' @keywords internal
safe_as_hex <- function(col) {
  tryCatch({
    rgb <- grDevices::col2rgb(col) / 255
    rgb_to_hex <- rgb(rgb[1,], rgb[2,], rgb[3,])
    return(rgb_to_hex)
  }, error = function(e) {
    rep(NA_character_, length(col))
  })
}

get_contrast <- function(fg, bg, method = c("WCAG", "APCA")) {
  method <- match.arg(method)
  tryCatch({
    cr <- colorspace::contrast_ratio(fg, bg, algorithm = method)
    if (method == "APCA") {
      if (is.matrix(cr)) abs(cr[, "normal"]) else abs(cr["normal"])
    } else {
      cr
    }
  }, error = function(e) NA)
}

compute_best_text_color <- function(bg_color, method = "APCA", light = "#eeeeee", dark = "#111111") {
  bg_color <- safe_as_hex(bg_color)
  sapply(bg_color, function(bg) {
    c_light <- get_contrast(light, bg, method)
    c_dark  <- get_contrast(dark, bg, method)
    if (is.na(c_light) && is.na(c_dark)) return(dark)
    if (c_light > c_dark) light else dark
  })
}
