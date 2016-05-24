library(visDec)

midnightFile <- "./Meetterrein_20151009_0000.jpg"
standardFile <- "./Meetterrein_20151009_0610.jpg"
sensorTestFile <- "./MOR_DeBilt_201510.txt"

test_that("FileNameParser", {
  expect_match(FileNameParser(midnightFile, "na*me_yyyymmdd_hhmm.jpg")$name, "Meetterrein")
  expect_match(FileNameParser(standardFile, "na*me_yyyymmdd_hhmm.jpg")$dateTime, "2015-10-09 06:10:00")

  expect_error(ExtractBasicImageStatistics("bliblablub"), "File does not exist.")
})

test_that("Regression tests",{
  expect_equal_to_reference(ExtractBasicImageStatistics(midnightFile), "midnightOutput.rds")
  expect_equal_to_reference(ExtractBasicImageStatistics(standardFile), "standardOutput.rds")

  expect_equal_to_reference(ReadMORSensorData(sensorTestFile), "sensorOutput.rds")
})

expect_true(testRcode())

