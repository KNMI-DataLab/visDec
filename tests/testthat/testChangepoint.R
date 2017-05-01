context("buggyPicture")

##Test for the non correct computation of the change point due to a picture completely black where no change point is
##present when cut below the date and time label. N.B. the picture has a valid computation of the change point if
##the date and time label is left in place.
library(visDec)


file<-"./Input/7-709_20161122_1530.jpg"
  #tryCatch(ImageFeatures(file, y>39), error = function(e) {print(file)
    #print(e)})
expect_error(ImageFeatures(file, y>39), "Missing value: NA is not allowed in the data as changepoint methods are only sensible for regularly spaced data.", fixed=T)
