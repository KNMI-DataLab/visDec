#' @useDynLib visDec
#' @importFrom Rcpp sourceCpp
CalculateImageStatistics <- function(filename) {
  img <- jpeg::readJPEG(filename)
  return(c(mean = mean(img),
              sd = sd(img)))
}

#' Extract basic image attributes
#' @param fullFilename Full path to the file
#' @param pattern (to extract name, date, and time)
#' @return vector of name, datetime, mean, and variance
#' @export
ExtractBasicImageStatistics <- function(fullFilename, pattern="na*me_yyyymmdd_hhmm.jpg") {
  tmpName  <- FileNameParser(fullFilename, pattern)
  tmpStats <- ImageSummary(fullFilename)
  return(data.frame(name     = tmpName$name,
                    datetime = tmpName$datetime,
                    mean     = tmpStats$mean,
                    var      = tmpStats$variance))
}

FileNameParser <- function(fullFilename, pattern) {
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
