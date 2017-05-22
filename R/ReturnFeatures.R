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
  tm <- GetTransmission(im)
  tm_hoz_avg <- HorizontalAverageTransmission(tm)
  data.table(filePath          = filePath,
             mean_edge         = DetectMeanEdges(im, 3),
             change_point      = TransmissionChangepoint(tm_hoz_avg),
             smoothness        = TransmissionSmoothness(tm_hoz_avg),
             fractal_dim       = GetFractalDim(im),
             mean_hue          = mean(channel(imT, 1)),
             mean_saturation   = mean(channel(imT, 2)),
             mean_brightness   = mean(channel(imT, 3)),
             mean_transmission = mean(tm)
             )
}
