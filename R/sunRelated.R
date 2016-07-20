#' Merge the information of configuration file and days and station of interest
#' @param dateStation Data Table with date of observation and station
#' @param configDF Data Table with station information (e.g., lon, lat, id)
MergeDaysWithStationConfig <- function(dateStation, configDF) {
  merge(configDF, dateStation, by.x = "imagePrefix", by.y = "filePrefix")
}

#' Filter the lighthours given the information of the station (longitude, latitude) and day
#' @param originalFileInfoDT data table with image files information
#' @export
FilterDayLightHours <- function(originalFileInfoDT) {
  uniqueDaysStation <- UniqueDaysAndStation(originalFileInfoDT)
  mergedData <- MergeDaysWithStationConfig(uniqueDaysStation, properties)
  dataWithSunTimes <- GetSunTimes(mergedData)
  combined <- merge(originalFileInfoDT, dataWithSunTimes, by.x = "dateOnly", by.y = "dateOnly")
  #combined$isDay <- combined[,dateTime] > combined[,sunriseDateTime] & combined[,dateTime] < combined[,sunsetDateTime]
  combined[, isDay := dateTime > sunriseDateTime & dateTime < sunsetDateTime]
}

#' Find the sunrise and sunset times given a date and lon lat location
#' @param data Data Table with image infromation
#' @import maptools
GetSunTimes <- function(data) {
  #the values in the list at positions are: [5]: lon [6]:lat [8]:date
  fn <- function(x, direction) {
    sunriset(crds = matrix(c(as.numeric(x[5]), as.numeric(x[6])),nrow = 1),
             dateTime = as.POSIXct(x[8]), direction = direction,
             POSIXct.out = TRUE)
  }
  sunriseTime <- apply(data, 1, fn, "sunrise")
  sunsetTime <- apply(data, 1, fn, "sunset")
  tempSunrise <- do.call(rbind.data.frame,sunriseTime)
  tempSunset  <- do.call(rbind.data.frame,sunsetTime)
  tempSunrise$date <- as.Date(tempSunrise$time, tz="CET")
  tempSunrise$sunriseTime <- strftime(tempSunrise$time, format = "%T %Z", tz="CET")
  setnames(tempSunrise, old=c("time"), new = c("sunriseDateTime"))
  tempSunset$date  <- as.Date(tempSunset$time, tz="CET")
  tempSunset$sunsetTime <- strftime(tempSunset$time, format = "%T %Z", tz="CET")
  setnames(tempSunset,old=c("time"), new=c("sunsetDateTime"))
  sunriseSunsetTimes <- merge(tempSunrise, tempSunset, by.x = "date", by.y = "date")
  merge(data, sunriseSunsetTimes, by.x ="dateOnly", by.y = "date")
}
