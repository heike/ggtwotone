#' @importFrom methods as
#' @importFrom stats lm predict
#' @importFrom utils head tail
#' @importFrom ggplot2 aes layer
#' @importFrom grid unit gpar textGrob curveGrob segmentsGrob grobTree gList
NULL

utils::globalVariables(c(
  "x", "y", "xend", "yend", "color1", "color2"
))
