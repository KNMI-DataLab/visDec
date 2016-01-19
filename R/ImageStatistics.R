#' @useDynLib visDec
#' @importFrom Rcpp sourceCpp
#' @import imager
CalculateImageStatistics <- function(filename) {
  img <- load.image(filename)
  return(list(mean = mean(img),
              variance = var(img)))
}

#' Calculate variance of gradient
#' @export
CalculateVarGradient <- function(filename, axes = "x") {
  stopifnot(axes=="x")
  img     <- load.image(filename)
  imgName <- FileNameParser(filename, "na*me_yyyymmdd_hhmm.jpg")
  imgGrad <- get_gradient(img, axes)[[1]]
  imgMean <- mean(imgGrad)
  imgVar  <- var(get_gradient(img, axes)[[1]])
  return(data.table(name     = imgName$name,
                    datetime = imgName$datetime,
                    mean     = imgMean,
                    var      = imgVar))
}

#' Extract basic image attributes
#' @param fullFilename Full path to the file
#' @param pattern (to extract name, date, and time)
#' @param method Defaults to own CImg implementation with imager as alternative
#' @return vector of name, datetime, mean, and variance
#' @export
#' @import data.table
ExtractBasicImageStatistics <- function(fullFilename, pattern="na*me_yyyymmdd_hhmm.jpg", method = "own") {
  tmpName  <- FileNameParser(fullFilename, pattern)
  if (method=="own") tmpStats <- ImageSummary(fullFilename)
  else tmpStats <- CalculateImageStatistics(fullFilename)
  return(data.table(name     = tmpName$name,
                    datetime = tmpName$datetime,
                    mean     = tmpStats$mean,
                    var      = tmpStats$variance))
}

FileNameParser <- function(fullFilename, pattern) {
  if (!file.exists(fullFilename)) stop("File does not exist.")
  if (pattern != "na*me_yyyymmdd_hhmm.jpg") stop("pattern not implemented")
  tmp <- strsplit(fullFilename, "/")[[1]]
  tmp <- tmp[length(tmp)]
  tmp <- strsplit(tmp, "_")[[1]]
  if (length(tmp)!=3) stop("filename does not match specified pattern")
  name <- tmp[1]
  date <- tmp[2]
  time <- substr(tmp[3], 1, 4)
  year <- substr(date,1,4)
  month <- substr(date, 5, 6)
  day   <- substr(date, 7, 8)
  hour  <- substr(time, 1, 2)
  min   <- substr(time, 3, 4)
  datetime <- as.POSIXct(paste(paste(year,month,day,sep="-"), paste(hour,min,sep=":")))
  return(list(name = name, datetime = datetime))
}

