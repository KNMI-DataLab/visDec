#' Detect edges
#' @param im Image
#' @param sigma Sigma
#' @export
DetectEdges <- function(im,sigma=1) {
  # adapted from http://dahtah.github.io/imager/foreground_background.html
  isoblur(im, sigma) %>% imgradient("xy") %>% llply(function(v) v^2) %>%
    add %>% imsplit("c") %>% add %>% sqrt
}

#' Return mean number of edges
#' @inheritParams DetectEdges
#' @export
DetectMeanEdges <- function(im, sigma=1) {
  DetectEdges(im, sigma) %>% mean
}
