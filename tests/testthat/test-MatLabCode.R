context("MatLabCode")


test_that("Same input same output", {
  im <- imager::load.example("parrots")
  im <- imager::subim(im, x > 400, y > 400)
  darkChannel <- GetDarkChannel(im, 7)
  expect_equal_to_reference(darkChannel, "darkChannelReference.rds")
})
