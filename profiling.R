# for profiling
library(profvis)

# profile dehazing
im <- imager::load.image("tests/testthat/forest.jpg")

profvis({
  library(visDec)
  Dehaze(im)
})
