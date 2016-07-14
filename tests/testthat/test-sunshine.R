context("sunshine")

## TODO: Rename context
## TODO: Add more tests
library(foreach)
library(iterators)
library(visDec)


midnightFile <- "./Input/Meetterrein_20151009_0000.jpg"
preSunriseTimeFile <- "./Input/Meetterrein_20151009_0610.jpg"
middayFile <- "./Input/Meetterrein_20151009_1200.jpg"


imageSummary <- foreach(file = iter(c(midnightFile, preSunriseTimeFile, middayFile)), .combine = rbind) %do% {
  fileInformation <- FileNameParser(file, "na*me_yyyymmdd_hhmm.jpg")
}
print(imageSummary)



test_that("check day", {
  uniqueDaysStation <- UniqueDaysAndStation(imageSummary)
  mergedStationDays <- MergeDaysWithStationConfig(uniqueDaysStation, properties)
  #print(FilterDayLightHours(mergedStationDays, imageSummary))
  expect_equal((FilterDayLightHours(mergedStationDays, imageSummary)$isDay), c(FALSE, FALSE, TRUE))
})
