#' Merge the information of configuration file and days and station of interest
#' @param dateStation Data Table with date of observation and station
#' @param configDF Data Table with station information (e.g., lon, lat, id)
#' @export
MergeDaysWithStationConfig <- function(dateStation, configDF) {
  mergedData<-merge(configDF, dateStation, by.x = "imagePrefix", by.y = "filePrefix")
  mergedData
}

#' Filter the lighthours given the information of the station (longitude, latitude) and day
#' @param mergedData data of station of interest and day of interest
#' @param originalFileInfoDT data table with image files information
#' @export
FilterDayLightHours <- function(mergedData, originalFileInfoDT) {
  dataWithSunTimes <- GetSunTimes(mergedData)
  combined <- merge(originalFileInfoDT, dataWithSunTimes, by.x = "dateOnly", by.y = "dateOnly")
  combined$isDay <- combined[,dateTime] > combined[,sunriseDateTime] & combined[,dateTime] < combined[,sunsetDateTime]
  combined
}

#' Find the sunrise and sunset times given a date and lon lat location
#' @param data Data Table with image infromation
#' @import maptools
#' @export
GetSunTimes <- function(data) {
  #the values in the list at positions are: [5]: lon [6]:lat [8]:date
  sunriseTime <- apply(data,1, function(x) sunriset(crds = matrix(c(as.numeric(x[5]), as.numeric(x[6])),nrow = 1), dateTime = as.POSIXct(x[8]), direction = "sunrise", POSIXct.out = TRUE))
  sunsetTime  <- apply(data,1, function(x) sunriset(crds = matrix(c(as.numeric(x[5]), as.numeric(x[6])),nrow = 1), dateTime = as.POSIXct(x[8]), direction = "sunset", POSIXct.out = TRUE))
  tempSunrise <- do.call(rbind.data.frame,sunriseTime)
  tempSunset  <- do.call(rbind.data.frame,sunsetTime)
  tempSunrise$date <- date(tempSunrise$time)
  tempSunrise$sunriseTime <- strftime(tempSunrise$time, format = "%T %Z")
  setnames(tempSunrise, old=c("time"), new = c("sunriseDateTime"))
  tempSunset$date  <- date(tempSunset$time)
  tempSunset$sunsetTime <- strftime(tempSunset$time, format = "%T %Z")
  setnames(tempSunset,old=c("time"), new=c("sunsetDateTime"))
  sunriseSunsetTimes <- merge(tempSunrise, tempSunset, by.x = "date", by.y = "date")
  data <- merge(data, sunriseSunsetTimes, by.x ="dateOnly", by.y = "date")
  data
}
