context("sunshine")

## TODO: Rename context
## TODO: Add more tests
#library(foreach)
#library(iterators)
library(visDec)


files <- c("./Input/Meetterrein_20151009_0000.jpg", # midnight
           "./Input/Meetterrein_20151009_0610.jpg", # pre sunrise
           "./Input/Meetterrein_20151009_1200.jpg") # midday


test_that("check day", {

  fileInfo <- data.table::rbindlist(lapply(files, FileNameParser,
                                           pattern="na*me_yyyymmdd_hhmm.jpg"))

  #uniqueDaysStation <- UniqueDaysAndStation(imageSummary)
  #mergedStationDays <- MergeDaysWithStationConfig(uniqueDaysStation, properties)
  #print(FilterDayLightHours(mergedStationDays, imageSummary))
  tmp <- FilterDayLightHours(fileInfo, properties, 0, 0)
  expect_equal(tmp$dateTime[1], as.POSIXct("2015-10-09 06:10:00", tz = "UTC"))
  expect_equal(tmp$dateTime[2], as.POSIXct("2015-10-09 12:00:00", tz = "UTC"))
  expect_equal_to_reference(tmp, file = "./Reference/DaylightHours.rds")

})

test_that("check time window", {
  fileInfo <- data.table::rbindlist(lapply(files, FileNameParser,
                                           pattern="na*me_yyyymmdd_hhmm.jpg"))
  tmp2 <- WindowFilterDayLightHours(fileInfo, properties, 120, 60, 60, 60)
  #for the day and location the sunrise is at 05:53:02 UTC
  #for the day the and location sunset is at 16:59:18 UTC
  expect_equal(tmp2$dateTime[1], as.POSIXct("2015-10-09 06:10:00", tz = "UTC"))
  expect_equal_to_reference(tmp2, file = "./Reference/SunriseSunsetHoursFilter.rds")

  selected <- TimeWindowFilter(fileInfo, "00:00", "07:00")
  expect_equal(selected$dateTime[1], as.POSIXct("2015-10-09 00:00:00", tz = "UTC"))
  expect_equal(selected$dateTime[2], as.POSIXct("2015-10-09 06:10:00", tz = "UTC"))
  expect_equal_to_reference(selected, file = "./Reference/TimeWindowFilter.rds")
}

)
