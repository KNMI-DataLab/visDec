# for profiling
library(profvis)

# profile dehazing
im <- imager::load.image("inst/extdata/Meetterrein/Meetterrein_20151009_0840.jpg")

profvis({
  library(visDec)
  Dehaze(im)
})

profvis({
  library(visDec)
  GetDarkChannel(im, 15)
})
