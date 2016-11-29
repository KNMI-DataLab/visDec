#' Return image features
#' @param filePath string
#' @param ... additional arguments for cropping using imsub
#' @export
ImageFeatures <- function(filePath, ...) {
  im <- load.image(filePath)
  if (nargs() > 1) {
    message("cropping image ")
    im <- imsub(im, ...)
  }
  imT <- RGBtoHSV(im)
  transmission <- GetHorizAvgTrans(im)
  data.table(filePath       = filePath,
             meanEdge       = DetectMeanEdges(im, 3),
             changePoint    = TransmissionChangepoint(transmission),
             smoothness     = TransmissionSmoothness(transmission),
             fractaldim     = GetFractalDim(transmission),
             fractalDim     = GetFractalDim(im),
             meanHue        = mean(channel(imT, 1)),
             meanSaturation = mean(channel(imT, 2)),
             meanBrightness = mean(channel(imT, 3))
             )
}
