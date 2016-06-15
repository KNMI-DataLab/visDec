


#porting code of the machineLearn.m (just classification tree is implemented)

#' @importFrom R.matlab readMat
#' @importFrom tree tree
#' @importFrom caret confusionMatrix
Classify<-function(){
  matObjLoad<-readMat("./inst/extdata/results_november_all_patch15.mat")
  dataStruct<-data.frame(matObjLoad$dataStruct)
  transmission<-dataStruct[1,]
  vis<-unname(unlist(dataStruct[4,]))
  transmissionNoNa<-transmission[!is.na(vis)]
  transmissionNoNa<-matrix(unlist(transmissionNoNa),ncol = 552)
  visNoNa<-vis[!is.na(vis)]
  x<-transmissionNoNa
  y<-(visNoNa<=250)+2*(visNoNa>250&visNoNa<=1000)+3*(visNoNa>1000)

  #decide the fraction of partition to be train set
  smp_size <- floor(0.5 * length(y))

  set.seed(123)
  train_ind <- sample(length(y), size = smp_size)
  trainX <- x[train_ind, ]
  trainY <- y[train_ind]

  dfTrain<-data.frame(trainX,trainY)

  testX <-x[-train_ind, ]
  testY <-y[-train_ind]

  df<-data.frame(testX)
  classificationTree<-tree(as.factor(trainY)~.,data = dfTrain)
  predicted<-predict(object = classificationTree, newdata = df, type = "class")
  factorTestY<-as.factor(testY)
  confMat<-confusionMatrix(factorTestY,predicted)
  return(confMat)
}

#' @importFrom imager load.image pad extract_patches
#'
ImportImage<-function(imageFile)
{
  #imageFile = "/usr/people/pagani/development/fogVisibility/EHTW_201604071533.jpg"
  imageFile = "/usr/people/pagani/Pictures/index.png"
  image<-load.image(imageFile)
  return(image)
}


#porting code of the get_dark_channel.m

#' Obtains the dark channel
#' @param image The image object
#' @param winSize Should probably be renamed
#' @export
#' @importFrom imager pad extract_patches width height channels as.cimg
#' @importFrom matlab padarray
#' @importFrom abind abind
GetDarkChannel <- function(image, winSize) {
  m <- width(image)
  n <- height(image)
  # I know it is in the matlab code but we should check what it actually adds
  padSize   <- floor(winSize/2)
  channels  <- channels(image, drop = FALSE)
  listArray <- lapply(channels, as.array)
  listArray <- lapply(listArray, drop)
  padded    <- lapply(listArray, padarray, padSize, padval = Inf, direction = "both")
  padded    <- lapply(padded, function(v) padarray(t(v),padSize, padval = Inf, direction = "both"))
  padded    <- lapply(padded, t)
  arrayFormat <- as.array(padded)
  arrayFormat <- abind(arrayFormat, along = 3)
  arrayFormat <- abind(arrayFormat, along = 4)
  dimnames(arrayFormat) <- NULL
  paddedImage <- aperm(arrayFormat, c(1, 2, 4, 3))
  paddedImage <- as.cimg(paddedImage)
  #padImage <- pad(as.array(image), 2*padSize, axes = "xy", pos = -1, val = 100)
  #padarray(image, padSize, padval = Inf, direction = "both")
  #padImage <- pad(padImage, padSize, axes = "xy", pos = 1, val = 100)
  grid    <- expand.grid(width = 1:m, height = 1:n)
  winsize <- rep(winSize - 1, nrow(grid))
  offset  <- padSize - 1 #in cimg the patches are extracted from the center of the patch itself. Matlab extracts from top-left corner
  patches <- extract_patches(paddedImage, grid[, 1] + offset, grid[, 2] + offset, winsize, winsize)
  darkChannel <- vapply(patches, min, 1)
  dim(darkChannel) <- c(m, n)
  darkChannel
}



#porting code of the get_atmosphere.m

#' Obtains atmosphere
#' @param image The image object
#' @param darkChannel Image dark channel
#' @importFrom imager pad extract_patches width height

#'
GetAtmosphere <- function(image, darkChannel)
{
  n <- width(image)
  m <- height(image)
  nPixel <- m * n
  nSearchPixels <- floor(nPixel * 0.01)
  darkVec       <- matrix(darkChannel, nPixel, 1)
  imageVec      <- matrix(image, nPixel, 3)
  sortedDark    <- sort(darkVec, decreasing = TRUE, index.return=TRUE)
  accumulator   <- matrix(0,1,3)
  for (k in 1:nSearchPixels){
    accumulator <- accumulator + imageVec[sortedDark$ix[k], 1:3]
  }
  atmosphere <- accumulator / nSearchPixels
  atmosphere
}


#porting code of the get_transmission_estimate.m

#' Obtains transmission
#' @param image The image object
#' @param atmosphere The image atmosphere
#' @param omega
#' @param winSize Window size
#' @importFrom imager width height spectrum
#' @importFrom abind abind
#'
GetTransmissionEstimate <- function(image, atmosphere, omega, winSize)
{
  n <- width(image)
  m <- height(image)
  channelsNum <- spectrum(image)
  toFill <- array(atmosphere, dim = c(1, 1, 3))
  repAtmosphere <- NULL
  final<-list()
  splittedImage<-channels(image, drop = TRUE)
  for(k in 1:channelsNum){
    temp <- array(toFill[k], dim = c(n, m))#m and n are inverted given the internal array-like representation that imager has #adding a fourth dimention to have a 4 dimentional matrix as normal color images are (3 colors plus 1 frame)
    division <- splittedImage[[k]] / temp
    final <- c(final, list(division))
  }
  arrayFormat <- abind(final, along = 3)
  arrayFormat <- abind(arrayFormat, along = 4)
  dimnames(arrayFormat) <- NULL
  toBeImage <- aperm(arrayFormat, c(1, 2, 4, 3))
  toBeImage <- as.cimg(toBeImage)
  transEst <- 1 - omega * GetDarkChannel(toBeImage, winSize)
  transEst
}


#porting code of the get_radiance.m
#' Obtains transmission
#' @param image The image object
#' @param transmission The image transmission
#' @param atmosphere The image atmosphere
#' @importFrom imager width height spectrum
#'
GetRadiance<-function(image, transmission, atmosphere)
{
  n <- width(image)
  m <- height(image)
  channels <- spectrum(image)
  toFill <- array(atmosphere, dim = c(1, 1, 3))
  repAtmosphere <- NULL
  maxTransmission <- NULL
  for(k in 1:channels){
    temp <- array(toFill[k], dim = c(n, m, 1)) #adding a fourth dimention to have a 4 dimentional matrix as normal color images are (3 colors plus 1 frame)
    repAtmosphere <- abind(repAtmosphere,temp,along = 4)
  }
  repAtmosphere <- unname(repAtmosphere)
  maxValues <- pmax(transmission, 0.1)
  for(k in 1:channels){
    temp <- array(maxValues, dim = c(n, m, 1)) #adding a fourth dimention to have a 4 dimentional matrix as normal color images are (3 colors plus 1 frame)
    maxTransmission<-abind(maxTransmission, temp, along = 4)
  }
  maxTransmission<-unname(maxTransmission)
  radiance <- ((as.array(image) - repAtmosphere) / maxTransmission) + repAtmosphere
  radiance <- unname(radiance)
  radiance
}


#porting code of the get_laplacian.m
#' Obtains Lplacian of the image
#' @importFrom Matrix diag rowSums
#' @importFrom pracma repmat
#' @importFrom imager width height spectrum erode_square
#'
GetLaplacian <- function(image) {
  m <- width(image)
  n <- height(image) # inverted to avoid confusion with matlab implementation
  channels <- spectrum(image)
  imgSize  <- m*n
  winRad   <- 1
  epsilon  <- 0.0000001
  maxNumNeigh  <- (winRad * 2 + 1) ^ 2
  #HERE I USE A SQUARE SHAPE TO ERODE AND NOT A DISK SHAPE AS IN THE ORIGINAL MATLAB CODE
  #trimapAll    <- erode_square(trimapAll, winRad * 2 + 1)
  indMat       <- matrix(1:imgSize, m, n)
  #indices      <- which((1 - trimapAll) != 0)
  indices      <- 1:(m*n)
  numInd       <- length(indices)
  maxNumVertex <- maxNumNeigh * numInd
  rowInds      <- NULL #array(0, dim=c(maxNumVertex, 1))
  colInds      <- NULL #array(0, dim=c(maxNumVertex, 1))
  vals         <- NULL #array(0, dim=c(maxNumVertex, 1))

  len <- 0
  for(k in 1:numInd){
  ind <- indices[k]
  i <- ((ind-1) %% m) + 1
  j <- floor((ind-1) / m) + 1
  mMin <- max( 1, i - winRad )
  mMax <- min( m, i + winRad )
  nMin <- max( 1, j - winRad )
  nMax <- min( n, j + winRad )
  winInds <- indMat[ mMin : mMax, nMin : nMax]
  winInds <- c(winInds)
  #print(winInds)
  numNeigh  <- length(winInds)
  winImage <- image[mMin:mMax, nMin:nMax, ,]
  arrayRep  <- as.array(winImage)
  winImage <- array(arrayRep, c(numNeigh, channels))
  winMean  <- colMeans(winImage)
  winMean  <- t(winMean)##REQUIRED FOR R IMPLEMENTATION OF VECTOR ROW AND COLUMN COMPARED TO MATLAB
  winVar   <- solve((t(winImage) %*% winImage / numNeigh) - (t(winMean) %*% winMean) + (epsilon / numNeigh * diag(channels)))
  winImage <- winImage - pracma::repmat(winMean, numNeigh, 1)
  winVals  <- (1 + winImage %*% winVar %*% t(winImage)) / numNeigh
  subLen   <- numNeigh%*%numNeigh
  winInds  <- pracma::repmat(winInds, numNeigh, 1)
  #rowInds[1+len: len+subLen] <- c(winInds)
  rowInds <- c(rowInds, c(winInds))
  winInds <- t(winInds)
  #colInds[1 + len:len + subLen] <- c(winInds)
  colInds <- c(colInds, c(winInds))
  #vals[1 + len:len + subLen] <- c(winVals)
  vals <- c(vals, c(winVals))
  len <- len + subLen;
  #print(winVals)
  }
  #index inverted EQUIRED FOR R IMPLEMENTATION OF VECTOR ROW AND COLUMN COMPARED TO MATLAB
  A <- sparseMatrix(i = colInds[1:len], j = rowInds[1:len], x = vals[1:len], dims = c(imgSize,imgSize))
  v <- Matrix::rowSums(A)
  #tempArray <- as.vector(padzeros(v, n*m, side = 'right'))
  D <- diag(v, nrow = length(v))
  L <- D - A
  #return(L)
}


#porting code of the dehaze.m
#' Obtain the dehazed image
#' @importFrom pracma mldivide
#' @importFrom imager width height
#'
Dehaze <- function(image, omega, winSize, lambda)
{
if (missing(omega)){
  omega <- 0.95
}
if (missing(winSize)){
  winSize <- 15
}
if (missing(lambda)){
  lambda <- 0.0001
}
n           <- width(image)
m           <- height(image)
darkChannel <- GetDarkChannel(image, winSize)
atmosphere  <- GetAtmosphere(image, darkChannel)
transEst    <- GetTransmissionEstimate(image, atmosphere, omega, winSize)
L           <- GetLaplacian(image)
A           <- L + lambda * diag(nrow = dim(L))
b           <- lambda * c(transEst)
x           <- mldivide(as.matrix(A),b)
dim(x)      <- c(m, n)
transmission <- x
radiance     <- GetRadiance(image, transmission, atmosphere)
radiance
}




