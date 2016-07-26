#' Read MOR data files
#' @param filenames List of filenames
#' @return data.table
#' @export
ReadMORSensorData <- function(filenames) {
  sensorData <- rbindlist(lapply(filenames, read.csv, stringsAsFactors = FALSE))
  sensorData <- data.table(sensorData)
  sensorData[TOA.MOR_10  == -1, TOA.MOR_10  := NA]
  #sensorData[, hhmmss := CorrectOurs(hhmmss)]
  sensorData[, IT_DATETIME := as.POSIXct(sensorData[, IT_DATETIME], format = "%Y%m%d_%H%M%S", tz = "CET")]
  setnames(sensorData, "IT_DATETIME", "dateTime")
  #sensorData[, hhmmss := NULL]
  sensorData[, year   := year(dateTime)]
  sensorData[, month  := month(dateTime)]
  sensorData[, day    := mday(dateTime)]
  sensorData[, hour   := hour(dateTime)]
  setcolorder(sensorData, c(2, 1, 5, 6, 7, 8, 3, 4))
  return(sensorData)
}

