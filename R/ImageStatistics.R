#' Extract basic image attributes
#' @param fullFilename Full path to the file
#' @param pattern (to extract name, date, and time)
#' @return vector of name, datetime, mean, and variance
#' @export
#' @useDynLib visDec
#' @import data.table
#' @import imager
#' @importFrom Rcpp sourceCpp
ExtractBasicImageStatistics <- function(fullFilename, pattern="na*me_yyyymmdd_hhmm.jpg") {
  tmpName  <- FileNameParser(fullFilename, pattern)
  tmpStats <- ImageSummary(fullFilename)
  return(data.table(name     = tmpName$name,
                    dateTime = tmpName$dateTime,
                    mean     = tmpStats$mean,
                    var      = tmpStats$variance))
}

#' Extracts name and date time from filename
#' @param fullFilename String
#' @param pattern String encoding the filename pattern
#' @export
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
  dateTime <- as.POSIXct(paste(paste(year,month,day,sep="-"), paste(hour,min,sep=":")))
  return(list(name = name, dateTime = dateTime))
}

