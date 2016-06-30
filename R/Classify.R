#' #porting code of the machineLearn.m (just classification tree is implemented)
#'
#' #' @importFrom R.matlab readMat
#' #' @importFrom tree tree
#' #' @importFrom caret confusionMatrix
#' #' @importFrom stats predict
#' Classify<-function(){
#'   matObjLoad<-readMat("./inst/extdata/results_november_all_patch15.mat")
#'   dataStruct<-data.frame(matObjLoad$dataStruct)
#'   transmission<-dataStruct[1,]
#'   vis<-unname(unlist(dataStruct[4,]))
#'   transmissionNoNa<-transmission[!is.na(vis)]
#'   transmissionNoNa<-matrix(unlist(transmissionNoNa),ncol = 552)
#'   visNoNa<-vis[!is.na(vis)]
#'   x<-transmissionNoNa
#'   y<-(visNoNa<=250)+2*(visNoNa>250&visNoNa<=1000)+3*(visNoNa>1000)
#'
#'   #decide the fraction of partition to be train set
#'   smp_size <- floor(0.5 * length(y))
#'
#'   set.seed(123)
#'   train_ind <- sample(length(y), size = smp_size)
#'   trainX <- x[train_ind, ]
#'   trainY <- y[train_ind]
#'
#'   dfTrain<-data.frame(trainX,trainY)
#'
#'   testX <-x[-train_ind, ]
#'   testY <-y[-train_ind]
#'
#'   df<-data.frame(testX)
#'   classificationTree<-tree(as.factor(trainY)~.,data = dfTrain)
#'   predicted<-predict(object = classificationTree, newdata = df, type = "class")
#'   factorTestY<-as.factor(testY)
#'   confMat<-confusionMatrix(factorTestY,predicted)
#'   return(confMat)
#' }
#'
#' #' @importFrom imager load.image pad extract_patches
#' #'
#' ImportImage<-function(imageFile)
#' {
#'   #imageFile = "/usr/people/pagani/development/fogVisibility/EHTW_201604071533.jpg"
#'   imageFile = "/usr/people/pagani/Pictures/index.png"
#'   image<-load.image(imageFile)
#'   return(image)
#' }
