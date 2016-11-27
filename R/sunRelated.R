#' Filter the lighthours given the information of the station (longitude,
#'latitude) and day
#' @param fileInfo data table with image files information
#' @param properties data table with specific information
#' @param offsetBeforeSunrise offset to include time before sunrise in minutes
#' @param offsetAfterSunset offset to include time after sunset in minutes
#' @export
FilterDayLightHours <- function(fileInfo, properties, offsetBeforeSunrise, offsetAfterSunset) {
  isDay <- dateTime <- sunriseDateTime <- sunsetDateTime <- NULL
  imagePrefix <- filePrefix <- dateOnly <- NULL
  uniqueDaysStation <- UniqueDaysAndStation(fileInfo)
  properties <- as.data.table(properties)
  setkey(properties, imagePrefix)
  setkey(uniqueDaysStation, filePrefix)
  mergedData        <- merge(properties, uniqueDaysStation, by.x = "imagePrefix", by.y = "filePrefix")
  dataWithSunTimes  <- GetSunTimes(mergedData)
  setkey(dataWithSunTimes, dateOnly)
  setkey(fileInfo, dateOnly)
  combined          <- merge(fileInfo, dataWithSunTimes, by = "dateOnly", allow.cartesian=TRUE)
  combined[, isDay := dateTime > sunriseDateTime - offsetBeforeSunrise * 60 & dateTime < sunsetDateTime + offsetAfterSunset * 60]
  combined <- combined[isDay == TRUE]
  combined[, isDay := NULL]
}

#' Find the sunrise and sunset times given a date and lon lat location
#' @param data Data Table with image infromation
#' @importFrom maptools sunriset
GetSunTimes <- function(data) {
  time <- NULL
  #the values in the list at positions are: [5]: lon [6]:lat [8]:date
  fn <- function(x, direction) {
    sunriset(crds = matrix(c(as.numeric(x[5]), as.numeric(x[6])),nrow = 1),
             dateTime = as.POSIXct(x[8], tz="UTC"), direction = direction,
             POSIXct.out = TRUE)
  }
  sunriseTime <- apply(data, 1, fn, "sunrise")
  sunsetTime  <- apply(data, 1, fn, "sunset")
  tempSunrise <- as.data.table(do.call(rbind.data.frame, sunriseTime))
  tempSunset  <- as.data.table(do.call(rbind.data.frame, sunsetTime ))
  tempSunrise[, date        := as.Date(time, tz = "UTC")]
  tempSunrise[, sunriseTime := strftime(time, format = "%T %Z", tz = "UTC")]
  tempSunset[ , date        := as.Date(time, tz = "UTC")]
  tempSunset[ , sunsetTime  := strftime(time, format = "%T %Z", tz = "UTC")]
  setnames(tempSunrise, old = c("time"), new = c("sunriseDateTime"))
  setnames(tempSunset,  old = c("time"), new = c("sunsetDateTime"))
  setkey(tempSunrise, date)
  setkey(tempSunset, date)
  sunriseSunsetTimes <- merge(tempSunrise, tempSunset, by = "date")
  merge(data, sunriseSunsetTimes, by.x ="dateOnly", by.y = "date")
}


#' Filter the lighthours given the information of the station (longitude,
#'latitude) and day
#' @param fileInfo data table with image files information
#' @param properties data table with specific information
#' @param offsetBeforeSunrise offset to include time before sunrise in minutes
#' @param offsetAfterSunrise offset to include time after sunrise in minutes
#' @param offsetBeforeSunset offset to include time before sunset in minutes
#' @param offsetAfterSunset offset to include time after sunset in minutes
WindowFilterDayLightHours <- function(fileInfo, properties, offsetBeforeSunrise, offsetAfterSunrise, offsetBeforeSunset, offsetAfterSunset) {
  isDay <- dateTime <- sunriseDateTime <- sunsetDateTime <- toAnalyze <- NULL
  uniqueDaysStation <- UniqueDaysAndStation(fileInfo)
  mergedData        <- merge(properties, uniqueDaysStation,
                             by.x = "imagePrefix", by.y = "filePrefix")
  dataWithSunTimes  <- GetSunTimes(mergedData)
  combined          <- merge(fileInfo, dataWithSunTimes, by = "dateOnly")
  combined[, toAnalyze := (dateTime > sunriseDateTime - offsetBeforeSunrise * 60 & dateTime < sunriseDateTime + offsetAfterSunrise * 60) | (dateTime > sunsetDateTime - offsetBeforeSunset * 60 & dateTime < sunsetDateTime + offsetAfterSunset * 60)]
  combined <- combined[toAnalyze == TRUE]
  combined[,toAnalyze := NULL]
}


#' Filter the images based on the time window of interest
#' @param fileInfo data table with image files information
#' @param initialTime initial time in HH:MM
#' @param finalTime final time in HH:MM
#' @export
TimeWindowFilter <- function(fileInfo, initialTime, finalTime){
  dateTime    <- NULL
  initialHHMM <- unlist(strsplit(initialTime, ":"))
  finalHHMM   <- unlist(strsplit(finalTime, ":"))
  filtered    <- fileInfo[(hour(dateTime) > as.numeric(initialHHMM[[1]]) |
                             (hour(dateTime) == as.numeric(initialHHMM[[1]]) &
                                minute(dateTime) >= as.numeric(initialHHMM[[2]]))) &
                            (hour(dateTime) < as.numeric(finalHHMM[[1]]) |
                               (hour(dateTime) == as.numeric(finalHHMM[[1]]) &
                                  minute(dateTime) <= as.numeric(finalHHMM[[2]])))]
  filtered
}




