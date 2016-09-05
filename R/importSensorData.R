#' Read MOR data files
#' @param filenames List of filenames
#' @return data.table
#' @export
ReadMORSensorData <- function(filenames) {
  dateTime <- day <- IT_DATETIME <- TOA.MOR_10 <- NULL
  sensorData <- rbindlist(lapply(filenames, read.csv, stringsAsFactors = FALSE))
  sensorData <- data.table(sensorData)
  sensorData[TOA.MOR_10  == -1, TOA.MOR_10  := NA]
  #sensorData[, hhmmss := CorrectOurs(hhmmss)]
  sensorData[, IT_DATETIME := as.POSIXct(sensorData[, IT_DATETIME], format = "%Y%m%d_%H%M%S", tz = "UTC")]
  setnames(sensorData, "IT_DATETIME", "dateTime")
  #sensorData[, hhmmss := NULL]
  sensorData[, year   := year(dateTime)]
  sensorData[, month  := month(dateTime)]
  sensorData[, day    := mday(dateTime)]
  sensorData[, hour   := hour(dateTime)]
  setcolorder(sensorData, c(2, 1, 5, 6, 7, 8, 3, 4))
  return(sensorData)
}

#' Synchronize the sensor reading to picture acquisition time
#' @param sensorDataDT data table with sensor data
#' @param imageInfoDT data table with image summary information
#' @return data.table
#' @export
SynchronizeSensorPicture <- function(sensorDataDT, imageInfoDT){
  dateTime <- dateTimeFW4 <- dateTimeRW4 <- dateTimeOrig <- TOA.MOR_10 <- NULL
  imageInfoDT[, dateTimeFW4 := dateTime + 4 * 60]
  imageInfoDT[, dateTimeRW4 := dateTime - 4 * 60]
  setkey(imageInfoDT, dateTimeFW4)
  setkey(sensorDataDT, dateTime)
  imageInfoDT[, dateTimeOrig := dateTime]
  imageAndSensorFW4 <- sensorDataDT[imageInfoDT, roll="nearest"]
  setkey(imageInfoDT, dateTimeRW4)
  imageAndSensorRW4 <- sensorDataDT[imageInfoDT, roll="nearest"]
  setnames(imageAndSensorRW4, "TOA.MOR_10", "TOA.MOR_10_RW")
  setnames(imageAndSensorFW4, "TOA.MOR_10", "TOA.MOR_10_FW")
  setkey(imageAndSensorRW4, dateTimeOrig)
  setkey(imageAndSensorFW4, dateTimeOrig)
  combined <- imageAndSensorRW4[imageAndSensorFW4]
  combined[, TOA.MOR_10 := pmin(combined$TOA.MOR_10_FW, combined$TOA.MOR_10_RW)]
  combined[, c("TOA.MOR_10_RW", "TOA.MOR_10_FW", "dateTime") := NULL]
  combined[, grep("^i.*", colnames(combined)) := NULL]
  combined[, grep("*W4$", colnames(combined)) := NULL]
  setnames(combined, "dateTimeOrig", "dateTime")
  combined
}
