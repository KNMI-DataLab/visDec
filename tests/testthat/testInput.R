library(visDec)

context("Filename parsing")

midnightFile <- "./Input/Meetterrein_20151009_0000.jpg"
standardFile <- "./Input/Meetterrein_20151009_0610.jpg"
sensorTestFile <- "./Input/DeBiltMORSensor2015.csv"

test_that("FileNameParser", {
  expect_match(FileNameParser(midnightFile, "na*me_yyyymmdd_hhmm.jpg")$filePrefix, "Meetterrein")
  expect_match(paste(FileNameParser(standardFile, "na*me_yyyymmdd_hhmm.jpg")$dateTime), "2015-10-09 06:10:00")

  expect_equal_to_reference(ReadMORSensorData(sensorTestFile), "./Reference/sensorOutput.rds")

  fileInfoDT <- FileNameParser(standardFile, "na*me_yyyymmdd_hhmm.jpg")
  expect_equal_to_reference(UniqueDaysAndStation(fileInfoDT), "./Reference/daysAndStationFiltered.rds")


})
