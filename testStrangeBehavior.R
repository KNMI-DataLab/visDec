# library(imager)
library(data.table)
library(ggplot2)
library(visDec)
library(foreach)
library(iterators)
library(doParallel)
registerDoParallel(cores=2)

path <- system.file("extdata/Meetterrein", package="visDec")
filenames <- list.files(path,
                        pattern=glob2rx("Meetterrein_201510*.jpg"),
                        full.names=TRUE)
imageSummary <- foreach(file = iter(filenames), .combine = rbind) %dopar% {
  FileNameParser(file, "na*me_yyyymmdd_hhmm.jpg")
}

imageSummary2 <- foreach(file = iter(filenames), .combine = rbind) %dopar% {
  FileNameParser(file, "na*me_yyyymmdd_hhmm.jpg")
}
# tmpSummar <- copy(imageSummary)
#
daylightImagesTmp <- FilterDayLightHours(imageSummary2, properties, 0, 0)
# index <- imageSummary[, filePath %in% daylightImagesTmp[, filePath]]
# daylightImages <- daylightImages[, .(filePrefix, filePath, dateTime, dateOnly)]
# daylightImages <- copy(imageSummary[index])
daylightImages <- imageSummary



daylightImages[, id := 1:.N]
setkey(daylightImages, id)

imageFeatures <- foreach(id = iter(daylightImages[, id]), .combine = rbind) %dopar% {
  daylightImages[id, ImageFeatures(filePath, y > 16)]
}

setkey(daylightImages, filePath)
setkey(imageFeatures, filePath)

imageSummary <- merge(daylightImages, imageFeatures)

