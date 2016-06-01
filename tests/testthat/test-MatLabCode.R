context("MatLabCode")


test_that("Same input same output", {
  im <- imager::load.example("parrots")
  im <- imager::subim(im, x > 400, y > 400)
  winSize <- 7
  darkChannel <- GetDarkChannel(im, winSize)
  expect_equal_to_reference(darkChannel, "darkChannelReference.rds")
  atmosphere <- GetAtmosphere(im, darkChannel)
  expect_equal_to_reference(atmosphere, "atmosphereReference.rds")
  omega <- 0.95
  transmissionEst <- GetTransmissionEstimate(im, atmosphere, omega, winSize)
  expect_equal_to_reference(transmissionEst, "transmissionEstReference.rds")
  radiance <- GetRadiance(im, transmissionEst, atmosphere)
  expect_equal_to_reference(radiance, "radianceReference.rds")
  m <- imager::width(im)
  n <- imager::height(im)
  trimapAll <- array(0, c(m, n, 1, 3))
  laplacian <- GetLaplacian(im, trimapAll)
  expect_equal_to_reference(laplacian, "laplacianReference.rds")
  lambda <- 0.0001
  dehaze <- Dehaze(im, omega, winSize,lambda)
  expect_equal_to_reference(dehaze, "dahazeReference.rds")
})
