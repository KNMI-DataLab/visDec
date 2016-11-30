#' Extracts name and date time from filename
#' @param fullFilename String
#' @param pattern String encoding the filename pattern
#' @export
FileNameParser <- function(fullFilename, pattern) {
  if (!file.exists(fullFilename)) stop("File does not exist.")
  if (pattern != "na*me_yyyymmdd_hhmm.jpg") stop("pattern not implemented")
  tmp <- strsplit(fullFilename, "/")[[1]]
  tmp <- tmp[length(tmp)]
  name <- tmp
  tmp <- strsplit(tmp, "_")[[1]]
  if (length(tmp) == 3){
    filePrefix <- tmp[1]
    date <- tmp[2]
    time <- substr(tmp[3], 1, 4)
    year <- substr(date,1,4)
    month <- substr(date, 5, 6)
    day   <- substr(date, 7, 8)
    hour  <- substr(time, 1, 2)
    min   <- substr(time, 3, 4)
  }
  else stop("filename does not match specified pattern")
  dateTime <- as.POSIXct(paste(paste(year,month,day,sep="-"),
                               paste(hour,min,sep=":")),
                         tz = "UTC")
  return(data.table(filePrefix = filePrefix, filePath = fullFilename,
                    dateTime = dateTime))
}

#' Finds if an observation falls into the day time or not
#' @param fileInfo Data Table with image information
UniqueDaysPerStation <- function(fileInfo) {
  dateOnly <- dateTime <- filePrefix <- NULL
  fileInfo[, dateOnly := as.Date(dateTime, tz = 'UTC')]
  setkeyv(fileInfo, c("filePrefix","dateOnly"))
  stationDate <- fileInfo[, list(filePrefix, dateOnly)]
  subset(unique(stationDate))
}
