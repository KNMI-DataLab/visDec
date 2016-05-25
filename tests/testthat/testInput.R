library(visDec)

midnightFile <- "./Meetterrein_20151009_0000.jpg"
standardFile <- "./Meetterrein_20151009_0610.jpg"
sensorTestFile <- "./MOR_DeBilt_201510.txt"

test_that("FileNameParser", {
  expect_match(FileNameParser(midnightFile, "na*me_yyyymmdd_hhmm.jpg")$name, "Meetterrein")
  expect_match(paste(FileNameParser(standardFile, "na*me_yyyymmdd_hhmm.jpg")$dateTime), "2015-10-09 06:10:00")

  expect_equal_to_reference(ReadMORSensorData(sensorTestFile), "sensorOutput.rds")
})

test_that("git check requirements work", {
  expect_message(UndocumentedFunction())
})
#expect_true(testRcode())

