


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


getDarkChannel<-function(image,winSize)
{
  library(doParallel)
  library(tcltk)
  dimImage<-dim(image)
  m<-dimImage[1]
  n<-dimImage[2]
  pad_size<-floor(winSize/2)
  print(dimImage)
  test<-as.array(image)
  plot(image)

  padImage<-pad(image,axes="y",winSize)
  padImage<-pad(image,axes="x",winSize)
  darkChannel<-matrix(0,m,n)
  counter<-0

  cl <- makeCluster(2)
  registerDoParallel(cl)


  #in case of performance issue might think of parellalizing it
  #foreach(i=1:m,.packages='imager') %dopar% {
    for(i in 1:m){
      for(j in 1:n)
    #test<-foreach(j=1:n, .packages='imager') %dopar%
    {
      #counter<-counter+1
      #print(counter)

      patch<-extract_patches(padImage,i,j,winSize-1,winSize-1)
      #print(unlist(patch))
      darkChannel[i,j]<-min(unlist(patch))
    }
    #darkChannel<-cbind(darkChannel,unlist(test))
    #print(test)
  }

return(darkChannel)
}

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
  transEst <- 1 - omega * getDarkChannel((as.array(image))/repAtmosphere, winSize);
  return(transEst)
}


getRadiance<-function(image,transmission,atmosphere)
{
  dimImage<-dim(image)
  m<-dimImage[1]
  n<-dimImage[2]

  toFill<-array(atmosphere,dim = c(1,1,3))
  repAtmosphere<-NULL
  for(k in 1:channels)
  {
    temp<- array(toFill[k],dim = c(m,n,1)) #adding a fourth dimention to have a 4 dimentional matrix as normal color images are (3 colors plus 1 frame)
    print(temp)
    repAtmosphere<-abind(repAtmosphere,temp,along = 4)
  }

  #<-pmax(transmission, 0.1)
  # for(k in 1:channels)
  # {
  #   temp<- array(toFill[k],dim = c(m,n,1)) #adding a fourth dimention to have a 4 dimentional matrix as normal color images are (3 colors plus 1 frame)
  #   print(temp)
  #   repAtmosphere<-abind(repAtmosphere,temp,along = 4)
  # }



}



# rep_atmosphere = repmat(reshape(atmosphere, [1, 1, 3]), m, n);
#
# max_transmission = repmat(max(transmission, 0.1), [1, 1, 3]);
#
# radiance = ((image - rep_atmosphere) ./ max_transmission) + rep_atmosphere;














