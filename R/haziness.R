#' Obtains the dark horizontal average transmission
#' @param image The image object
#' @param winSize Should probably be renamed
#' @return vertical transmission profile
#' @export
# #' @importFrom imager pad extract_patches width height channels as.cimg
GetHorizAvgTrans <- function(image, winSize, omega, lambda) {

  if (missing(omega)) {
    omega <- 0.95
  }
  if (missing(winSize)) {
    winSize <- 15
  }
  if (missing(lambda)) {
    lambda <- 0.0001
  }
  darkChannel  <- GetDarkChannel(image, winSize)
  atmosphere   <- GetAtmosphere(image, darkChannel)
  transmission <- GetTransmissionEstimate(image, atmosphere, omega, winSize)
  rowMeans(t(transmission)) #transpose is done given the representation of imager that invert height and width in the matrix representation
}
