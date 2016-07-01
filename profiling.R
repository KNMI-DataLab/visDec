# for profiling
library(profvis)

# profile dehazing
im <- imager::load.image("tests/testthat/forest.jpg")

profvis({
  library(visDec)
  GetDarkChannel(im, 15)
})

profvis({
  library(visDec)
  Dehaze(im)
})
