#' Return image features
#' @export
ImageFeatures <- function(filename, ...) {
  im <- load.image(filename)
  if (nargs() > 1) {
    message("cropping image ")
    im <- imsub(im, ...)
  }
  imT <- RGBtoHSV(im)
  transmission <- GetHorizAvgTrans(im)
  list(meanEdge       = DetectMeanEdges(im, 3),
       changePoint    = TransmissionChangepoint(transmission),
       smoothness     = TransmissionSmoothness(transmission),
       meanHue        = mean(channel(imT, 1)),
       meanSaturation = mean(channel(imT, 2)),
       meanBrightness = mean(channel(imT, 3)) )
}

# Envisioned usage
# Calculate features by filenames

# return features and filename

# join old dt with feature dt by filename
