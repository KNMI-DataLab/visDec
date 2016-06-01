


#porting code of the machineLearn.m (just classification tree is implemented)

#' @importFrom R.matlab readMat
#' @importFrom tree tree
#' @importFrom caret confusionMatrix
classify<-function(){
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
importImage<-function(imageFile)
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
#' @importFrom imager pad extract_patches width height
GetDarkChannel <- function(image, winSize) {
  m <- width(image)
  n <- height(image)

  # I know it is in the matlab code but we should check what it actually adds
  padSize  <- floor(winSize/2)
  padImage <- pad(image, padSize, "xy")

  grid    <- expand.grid(width = 1:m, height = 1:n)
  winsize <- rep(winSize - 1, nrow(grid))

  patches <- extract_patches(padImage, grid[, 1], grid[, 2], winsize, winsize)

  darkChannel      <- vapply(patches, min, 1/2)
  dim(darkChannel) <- c(m, n)
  darkChannel
}



#porting code of the get_atmosphere.m

#' @importFrom abind abind
#'
getAtmosphere<-function(image,darkChannel)
{
  dimImage<-dim(image)
  m<-dimImage[1]
  n<-dimImage[2]
  nPixel<-m*n
  nSearchPixels<-floor(nPixel*0.01)
  darkVec<-matrix(darkChannel,nPixel,1)
  imageVec<-matrix(image,nPixel,3)

  sortedDark<-sort(darkVec,decreasing = TRUE,index.return=TRUE)

  accumulator<-matrix(0,1,3)

  for (k in 1:nPixel)
  {
    accumulator<-accumulator+imageVec[sortedDark$ix[k],1:3]
  }

  atmosphere<-accumulator/nSearchPixels

  return(atmosphere)
}


#porting code of the get_transmission_estimate.m

#' @importFrom abind abind
#'
getTransmissionEstimate<-function(image,atmosphere,omega,winSize)
{
  dimImage<-dim(image)
  m<-dimImage[1]
  n<-dimImage[2]
  channels<-dimImage[4]
  toFill<-array(atmosphere,dim = c(1,1,3))
  repAtmosphere<-NULL
  for(k in 1:channels)
  {
    temp<- array(toFill[k],dim = c(m,n,1)) #adding a fourth dimention to have a 4 dimentional matrix as normal color images are (3 colors plus 1 frame)
    print(temp)
    repAtmosphere<-abind(repAtmosphere,temp,along = 4)
  }
  repAtmosphere<-unname(repAtmosphere)
  transEst <- 1 - omega * getDarkChannel((as.array(image))/repAtmosphere, winSize);
  return(transEst)
}


#porting code of the get_radiance.m


getRadiance<-function(image,transmission,atmosphere)
{
  dimImage<-dim(image)
  m<-dimImage[1]
  n<-dimImage[2]
  channels<-dimImage[4]

  toFill<-array(atmosphere,dim = c(1,1,3))
  repAtmosphere<-NULL
  maxTransmission<-NULL
  for(k in 1:channels)
  {
    temp<- array(toFill[k],dim = c(m,n,1)) #adding a fourth dimention to have a 4 dimentional matrix as normal color images are (3 colors plus 1 frame)
    #print(temp)
    repAtmosphere<-abind(repAtmosphere,temp,along = 4)
  }
  repAtmosphere<-unname(repAtmosphere)

  maxValues<-pmax(transmission, 0.1)
  for(k in 1:channels)
  {
    temp<- array(maxValues,dim = c(m,n,1)) #adding a fourth dimention to have a 4 dimentional matrix as normal color images are (3 colors plus 1 frame)
  #   print(temp)
    maxTransmission<-abind(maxTransmission,temp,along = 4)
  }

  maxTransmission<-unname(maxTransmission)

  radiance <- ((as.array(image) - repAtmosphere)/maxTransmission) + repAtmosphere

  radiance<-unname(radiance)
  return(radiance)


}


getLaplacian <- function(image, trimapAll) {

  dimImage <- dim(image)
  m        <- dimImage[1]
  n        <- dimImage[2]
  channels <- dimImage[4]
  imgSize  <- m*n
  winRad   <- 1
  epsilon  <- 0.0000001
  maxNumNeigh  <- (winRad * 2 + 1) ^ 2


  #for test purposes trimpAll
  trimapAll <- array(0,dim=c(n,m,1,channels))


  #HERE I USE A SQUARE SHAPE TO ERODE AND NOT A DISK SHAPE AS IN THE ORIGINAL MATLAB CODE
  trimapAll    <- erode_square(trimapAll, winRad * 2 + 1)
  indMat       <- matrix(1:imgSize, m, n)
  indices      <- which((1 - trimapAll) != 0)
  numInd       <- length(indices)
  maxNumVertex <- maxNumNeigh * numInd
  rowInds      <- array(0, dim=c(maxNumVertex, 1))
  colInds      <- array(0, dim=c(maxNumVertex, 1))
  vals         <- array(0, dim=c(maxNumVertex, 1))

  len <- 0

  for(k in 1:length(indices)){

  ind <- indices[k]

  i <- ((ind-1) %% m) + 1
  j <- floor((ind-1) / m) + 1



  mMin <- max( 1, i - winRad )
  mMax <- min( m, i + winRad )
  nMin <- max( 1, j - winRad )
  nMax <- min( n, j + winRad )

  winInds <- indMat[ mMin : mMax, nMin : nMax]
  winInds <- c(winInds)

  numNeigh <- nrow(winInds)



  win_image <- extract_patches(image, mMin, nMin, mMax - mMin, nMax - nMin)
  arrayRep <- as.array(win_image)
  win_image <- array(arrayRep, c(numNeigh, channels))

  win_mean <- colMean(win_image)

  win_var <- solve((t(win_image) * win_image / num_neigh) - (t(win_mean) * win_mean) + (epsilon / num_neigh * diag(channels) ) )


  ####################TILL HERE#############################################



  win_image <- win_image - rep(win_mean, c(num_neigh, 1))

  win_vals <- ( 1 + win_image * win_var * t(win_image) ) / num_neigh;

  sub_len <- num_neigh*num_neigh;

  win_inds <- rep(win_inds, c(1, num_neigh))

  row_inds[1+len: len+sub_len] <- c(win_inds)
  #
  win_inds <- t(win_inds)
  #
  col_inds[1+len: len+sub_len] <- c(win_inds)
  #
  vals[1+len: len+sub_len] <- c(win_vals)
  #
  len <- len + sub_len;
  }
  A = sparse(row_inds(1:len),col_inds(1:len),vals(1:len),img_size,img_size);
  #
 # D = spdiags(sum(A,2),0,n*m,n*m);
  #
  L = D - A;

}













