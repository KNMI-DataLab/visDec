#' Extracts name and date time from filename
#' @param fullFilename String
#' @param pattern String encoding the filename pattern
#' @export
FileNameParser <- function(fullFilename, pattern="na*me_yyyymmdd_hhmm.jpg") {
  if (!file.exists(fullFilename)) stop("File does not exist.")
  if (pattern != "na*me_yyyymmdd_hhmm.jpg") stop("pattern not implemented")
  tmp <- strsplit(fullFilename, "/")[[1]]
  tmp <- tmp[length(tmp)]
  name <- tmp
  tmp <- strsplit(tmp, "_")[[1]]
  if (length(tmp)!=3) stop("filename does not match specified pattern")
  filePrefix <- tmp[1]
  date <- tmp[2]
  time <- substr(tmp[3], 1, 4)
  year <- substr(date,1,4)
  month <- substr(date, 5, 6)
  day   <- substr(date, 7, 8)
  hour  <- substr(time, 1, 2)
  min   <- substr(time, 3, 4)
  dateTime <- as.POSIXct(paste(paste(year,month,day,sep="-"),
                               paste(hour,min,sep=":")),
                         tz = "CET")
  return(data.table(filePrefix = filePrefix, filePath = fullFilename, dateTime = dateTime))
}


#' Read properties file from file
#' @param fullFilename String
#' @export
ReadConfig <- function(configFileName) {
  configDF <- read.csv(configFileName)
  configDF
}

#' Finds if an opbservation falls into the day time or not
#' @param trainTestDT Data Table with image infromation
#' @export
RetriveDaysAndStation <- function(fileInfoDT) {
  fileInfoDT$dateOnly <- date(fileInfoDT$dateTime)
  setkeyv(testTrainDT, c("filePrefix","dateOnly"))
  uniqueDateStation<-subset(unique(testTrainDT), select=c("filePrefix", "dateOnly"))
  uniqueDateStation
}
