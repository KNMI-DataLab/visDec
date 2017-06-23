GetPaddedImage <- function(image, padSize) {
  channels    <- channels(image, drop = FALSE)
  listArray   <- lapply(channels, as.array)
  listArray   <- lapply(listArray, drop)
  padded      <- lapply(listArray, padarray, padSize, padval = Inf,
                        direction = "both")
  padded      <- lapply(padded,
                        function(v) padarray(t(v), padSize, padval = Inf,
                                             direction = "both"))
  padded      <- lapply(padded, t)
  arrayFormat <- as.array(padded)
  arrayFormat <- abind(arrayFormat, along = 3)
  arrayFormat <- abind(arrayFormat, along = 4)
  dimnames(arrayFormat) <- NULL
  paddedImage <- aperm(arrayFormat, c(1, 2, 4, 3))
  as.cimg(paddedImage)
  # padImage <- pad(as.array(image), 2*padSize, axes = "xy", pos = -1, val = 100)
  # padarray(image, padSize, padval = Inf, direction = "both")
  # paddedImage <- pad(image, padSize, axes = "xy", pos = 0, val = Inf)
}

#' Obtains the dark channel
#' @inheritParams GetRadiance
#' @export
#' @importFrom imager pad patchstat width height channels as.cimg
#' @importFrom matlab padarray
#' @importFrom abind abind
#' @export
GetDarkChannel <- function(image, winSize=15) {
  m <- width(image)
  n <- height(image)
  padSize     <- floor(winSize / 2)
  # What does the padding add to the functionality?
  padIm            <- GetPaddedImage(image, padSize)
  grid             <- expand.grid(width = 1:m, height = 1:n)
  winsize          <- rep(winSize - 1, nrow(grid))
  offset           <- padSize - 1
  darkChannel      <- patchstat(padIm, "min", grid[, 1] + offset,
                                          grid[, 2] + offset, winsize, winsize)
  dim(darkChannel) <- c(m, n)
  as.cimg(darkChannel)
}

#' Obtains atmosphere light
#' @inheritParams GetRadiance
#' @param darkChannel Image dark channel (defaults to default dark channel)
#' @importFrom imager pad width height
#' @export
#'
GetAtmosphere <- function(image, darkChannel = NULL, winSize = 15) {
  n <- width(image)
  m <- height(image)
  if (is.null(darkChannel)) darkChannel <- GetDarkChannel(image, winSize)
  nPixel <- m * n
  nSearchPixels <- floor(nPixel * 0.01)
  darkVec       <- matrix(as.matrix(darkChannel), nPixel, 1)
  imageVec      <- matrix(image, nPixel, 3)
  sortedDark    <- sort(darkVec, decreasing = TRUE, index.return = TRUE)
  accumulator   <- matrix(0, 1, 3)
  for (k in 1:nSearchPixels){
    accumulator <- accumulator + imageVec[sortedDark$ix[k], 1:3]
  }
  atmosphere <- accumulator / nSearchPixels
  atmosphere
}

#' Obtains transmission
#' @inheritParams GetRadiance
#' @importFrom imager width height spectrum
#' @importFrom abind abind
#' @export
#'
GetTransmission <- function(image, atmosphere = NULL, omega = 0.95,
                           winSize = 15) {
  n <- width(image)
  m <- height(image)
  if (is.null(atmosphere)) atmosphere <- GetAtmosphere(image, winSize = winSize)
  channelsNum <- spectrum(image)
  toFill <- array(atmosphere, dim = c(1, 1, 3))
  final <- list()
  splittedImage <- channels(image, drop = TRUE)
  for (k in 1:channelsNum){
    #m and n are inverted given the internal array-like representation that
    # imager has #adding a fourth dimention to have a 4 dimentional matrix as
    # normal color images are (3 colors plus 1 frame)
    temp <- array(toFill[k], dim = c(n, m))
    division <- splittedImage[[k]] / temp
    final <- c(final, list(division))
  }
  arrayFormat <- abind(final, along = 3)
  arrayFormat <- abind(arrayFormat, along = 4)
  dimnames(arrayFormat) <- NULL
  toBeImage <- aperm(arrayFormat, c(1, 2, 4, 3))
  toBeImage <- as.cimg(toBeImage)
  transEst <- 1 - omega * GetDarkChannel(toBeImage, winSize)
  as.cimg(transEst)
}


#' Obtains radiance
#' @param image The image object
#' @param transmission The image transmission
#' @param atmosphere The image atmosphere
#' @param omega Constant for aerial perspective
#### @param lambda Regularization parameter for soft matting
#' @param winSize Should probably be renamed to patch
#' @importFrom imager width height spectrum
#' @export
#'
GetRadiance <- function(image, transmission = NULL, atmosphere = NULL,
                      omega = 0.95, winSize = 15) {
  if (is.null(atmosphere)) atmosphere <- GetAtmosphere(image, winSize = winSize)
  if (is.null(transmission)) {
    transmission <- GetTransmission(image, atmosphere, omega = omega,
                                            winSize = winSize)
  }
  n <- width(image)
  m <- height(image)
  channels <- spectrum(image)
  toFill <- array(atmosphere, dim = c(1, 1, 3))
  repAtmosphere <- NULL
  maxTransmission <- NULL
  for (k in 1:channels){
    # adding a fourth dimention to have a 4 dimentional matrix as normal color
    # images are (3 colors plus 1 frame)
    temp <- array(toFill[k], dim = c(n, m, 1))
    repAtmosphere <- abind(repAtmosphere, temp, along = 4)
  }
  repAtmosphere <- unname(repAtmosphere)
  maxValues <- pmax(transmission, 0.1)
  for (k in 1:channels){
    # adding a fourth dimention to have a 4 dimentional matrix as normal color
    # images are (3 colors plus 1 frame)
    temp <- array(maxValues, dim = c(n, m, 1))
    maxTransmission <- abind(maxTransmission, temp, along = 4)
  }
  maxTransmission <- unname(maxTransmission)
  radiance <- (as.array(image) - repAtmosphere) / maxTransmission +
    repAtmosphere
  radiance <- unname(radiance)
  as.cimg(radiance)
}


# #porting code of the get_laplacian.m
# #' Obtains Matting Laplacian of the image
# #' @inheritParams Dehaze
# #' @importFrom Matrix diag rowSums
# #' @importFrom pracma repmat
# #' @importFrom imager width height spectrum erode_square
# #' @importFrom Matrix sparseMatrix
# #' @references \url{http://www.wisdom.weizmann.ac.il/~levina/papers/Matting-Levin-Lischinski-Weiss-CVPR06.pdf}
# #'
# GetMattingLaplacian <- function(image) {
#   m <- width(image)
#   n <- height(image) # inverted to avoid confusion with matlab implementation
#   channels <- spectrum(image)
#   imgSize  <- m*n
#   winRad   <- 1
#   epsilon  <- 0.0000001
#   maxNumNeigh  <- (winRad * 2 + 1) ^ 2
#   # HERE I USE A SQUARE SHAPE TO ERODE AND NOT A DISK SHAPE AS IN THE ORIGINAL
#   # MATLAB CODE
#   #trimapAll    <- erode_square(trimapAll, winRad * 2 + 1)
#   indMat       <- matrix(1:imgSize, m, n)
#   #indices      <- which((1 - trimapAll) != 0)
#   indices      <- 1:(m*n)
#   numInd       <- length(indices)
#   maxNumVertex <- maxNumNeigh * numInd
#   rowInds      <- NULL #array(0, dim=c(maxNumVertex, 1))
#   colInds      <- NULL #array(0, dim=c(maxNumVertex, 1))
#   vals         <- NULL #array(0, dim=c(maxNumVertex, 1))
#
#   len <- 0
#   for(k in 1:numInd){
#   ind <- indices[k]
#   i <- ((ind-1) %% m) + 1
#   j <- floor((ind-1) / m) + 1
#   mMin <- max( 1, i - winRad )
#   mMax <- min( m, i + winRad )
#   nMin <- max( 1, j - winRad )
#   nMax <- min( n, j + winRad )
#   winInds <- indMat[ mMin : mMax, nMin : nMax]
#   winInds <- c(winInds)
#   #print(winInds)
#   numNeigh  <- length(winInds)
#   winImage <- image[mMin:mMax, nMin:nMax, ,]
#   arrayRep  <- as.array(winImage)
#   winImage <- array(arrayRep, c(numNeigh, channels))
#   winMean  <- colMeans(winImage)
#   winMean  <- t(winMean)##REQUIRED FOR R IMPLEMENTATION OF VECTOR ROW AND COLUMN COMPARED TO MATLAB
#   winVar   <- solve((t(winImage) %*% winImage / numNeigh) - (t(winMean) %*% winMean) + (epsilon / numNeigh * diag(channels)))
#   winImage <- winImage - pracma::repmat(winMean, numNeigh, 1)
#   winVals  <- (1 + winImage %*% winVar %*% t(winImage)) / numNeigh
#   subLen   <- numNeigh%*%numNeigh
#   winInds  <- pracma::repmat(winInds, numNeigh, 1)
#   #rowInds[1+len: len+subLen] <- c(winInds)
#   rowInds <- c(rowInds, c(winInds))
#   winInds <- t(winInds)
#   #colInds[1 + len:len + subLen] <- c(winInds)
#   colInds <- c(colInds, c(winInds))
#   #vals[1 + len:len + subLen] <- c(winVals)
#   vals <- c(vals, c(winVals))
#   len <- len + subLen;
#   #print(winVals)
#   }
#   #index inverted EQUIRED FOR R IMPLEMENTATION OF VECTOR ROW AND COLUMN COMPARED TO MATLAB
#   A <- sparseMatrix(i = colInds[1:len], j = rowInds[1:len], x = vals[1:len], dims = c(imgSize,imgSize))
#   v <- Matrix::rowSums(A)
#   #tempArray <- as.vector(padzeros(v, n*m, side = 'right'))
#   D <- diag(v, nrow = length(v))
#   L <- D - A
#   #return(L)
# }
#
# #' Refines transmission estimate via matting Laplacian
# #' @inheritParams Dehaze
# #' @param transmission Initial transmission estimate
# #' @param lambda Regularization parameter for the soft matting
# #' @importFrom pracma mldivide
# #' @importFrom imager width height
# RefineTransmissionEstimate <- function(image, transmission, lambda) {
#   n           <- width(image)
#   m           <- height(image)
#   L           <- GetMattingLaplacian(image)
#   A           <- L + lambda * diag(nrow = dim(L))
#   b           <- lambda * c(transmission)
#   x           <- mldivide(as.matrix(A),b)
#   dim(x)      <- c(m, n)
#   x
# }





