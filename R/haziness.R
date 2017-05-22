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
  transmission <- GetTransmission(image, omega = omega, winSize = winSize)
  # the following transpose is done given the representation of
  # imager that invert height and width in the matrix representation
  HorizontalAverageTransmission(transmission)
}

#' Obtains horizontal average from transmission
#' @param x the transmission
#' @export
HorizontalAverageTransmission <- function(x) {
  rowMeans(t(as.matrix(x)))
}

#' Obtains change point of transmission
#' @param transmission The horizontally averaged transmission
#' @export
TransmissionChangepoint <- function(transmission) {
  transmission %>%
    cpt.mean(penalty = "None") %>%
    cpts()
}

#' Smoothness of transmission
#' @param transmission The horizontally averaged transmission
#' @export
TransmissionSmoothness <- function(transmission) {
  sd(diff(transmission, lag=50)) /abs(mean(diff(transmission, lag = 50)))
}

#' Fractal dimension of numeric vector or cimg
#' @param x Numeric vector, matrix or of class cimg
#' @note Images (e.g objects of class cimg) are first transformed to grayscale
#'  and then considered as a matrix
#' @export
GetFractalDim <- function(x) {
  if (inherits(x, "cimg")) {
    x <- as.matrix(grayscale(x))
  }
  if (inherits(x, "matrix")) {
    return(fd.estimate(x)$fd[1,1,1])
  } else if (inherits(x, "numeric")) {
    return(fd.estimate(x)$fd[1,1])
  } else {
    stop(paste("GetFractalDim not defined for object of class", class(x)[1]))
  }
}
