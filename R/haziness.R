#' Obtains the dark horizontal average transmission
#' @param image The image object
#' @param winSize Should probably be renamed
#' @param omega omega parameter
#' @param lambda lambda parameter
#' @return vertical transmission profile
#' @export
# #' @importFrom imager pad extract_patches width height channels as.cimg
GetHorizAvgTrans <- function(image, winSize = 15, omega = 0.95,
                             lambda = 0.001) {
  darkChannel  <- GetDarkChannel(image, winSize)
  atmosphere   <- GetAtmosphere(image, darkChannel)
  transmission <- GetTransmissionEstimate(image, atmosphere, omega, winSize)
  # the following transpose is done given the representation of
  # imager that invert height and width in the matrix representation
  rowMeans(t(transmission))
}

#' Obtains change point of transmission
#' @param image The image object
#' @export
TransmissionChangepoint <- function(image) {
  image %>% GetHorizAvgTrans() %>%
    cpt.mean(penalty = "None") %>%
    cpts()
}
