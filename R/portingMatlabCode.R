


#porting code of the machineLearn.m (just classification tree is implemented)

testRcode<-function(){
  library(R.matlab)
  library(caret)
  library(tree)
  matObjLoad<-readMat("/usr/people/pagani/development/fogVisibility/MatlabCodes/matlabcodes/JAKUB/results_november_all_patch15.mat")
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

#porting code of the get_dark_channel.m TO BE FINISHED
getDarkChannel<-function(winSize)
{
  library(EBImage)
  library(ptw)

  imageFile = "/usr/people/pagani/development/fogVisibility/EHTW_201604071533.jpg"
  image<-readImage(imageFile)
  dimImage<-dim(image)

  pad_size<-floor(winSize/2)

  print(dimImage)

  dataImage<-image@.Data
  #imgPadded<-padzeros(dataImage,pad_size,"both")
  imgPadded<-padzeros(t(dataImage[,,1]),pad_size,"both")
  imgPadded<-padzeros(t(imgPadded),pad_size,"both")

#the other dimentions of the matrix have to be worked


  return(imgPadded)


# pad_size = floor(win_size/2);
#
# padded_image = padarray(image, [pad_size pad_size], Inf);
#
# dark_channel = zeros(m, n);
#
# for j = 1 : m
# for i = 1 : n
# patch = padded_image(j : j + (win_size-1), i : i + (win_size-1), :);
#
# dark_channel(j,i) = min(patch(:));
# end
# end
}




