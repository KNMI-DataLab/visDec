#' Return image features
#' @param filePath string
#' @param ... additional arguments for cropping using imsub
#' @export
ImageFeatures <- function(filePath, ...) {
  im <- load.image(filePath)
  if (nargs() > 1) {
    im <- imsub(im, ...)
  }
  imT <- RGBtoHSV(im)
  transmission <- NULL
  transmission <- GetHorizAvgTrans(im)
  if(any(is.na(transmission))==TRUE)
  {
  #cat(paste(filePath, any(is.na(transmission)),"\n"), file="mylog22.txt", append=TRUE)
    #transmission <- transmission[!is.na(transmission)]
  transmission <- c(0,0)
  }
  #print(TransmissionChangepoint(transmission))
  data.table(filePath       = filePath,
             meanEdge       = DetectMeanEdges(im, 3),
             changePoint    = TransmissionChangepoint(transmission), #tryCatch(TransmissionChangepoint(transmission), error= print(filePath)),
             smoothness     = TransmissionSmoothness(transmission),
             fractalDim     = GetFractalDim(im),
             meanHue        = mean(channel(imT, 1)),
             meanSaturation = mean(channel(imT, 2)),
             meanBrightness = mean(channel(imT, 3))
             )

}
