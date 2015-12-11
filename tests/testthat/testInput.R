library(visDec)

midnightFile <- "../../inst/extdata/Meetterrein/Meetterrein_20151009_0000.jpg"
standardFile <- "../../inst/extdata/Meetterrein/Meetterrein_20151009_0010.jpg"
expect_match(ExtractBasicImageStatistics(midnightFile)$name,     "Meetterrein")
expect_match(ExtractBasicImageStatistics(standardFile)$datetime, "2015-10-09 00:10:00")

expect_error(ExtractBasicImageStatistics("bliblablub"), "File does not exist.")

expect_equal_to_reference(ExtractBasicImageStatistics(midnightFile), "midnightOutput.rds")
expect_equal_to_reference(ExtractBasicImageStatistics(midnightFile), "standardOutput.rds")
