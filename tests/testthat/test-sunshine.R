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
  tmp <- FilterDayLightHours(fileInfo, properties)
  expect_equal(tmp$isDay, c(FALSE, FALSE, TRUE))
  expect_equal_to_reference(tmp, file = "./Reference/DaylightHours.rds")
})
