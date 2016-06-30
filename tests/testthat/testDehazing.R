context("MatLabCode")


test_that("Same input same output", {
  im <- imager::load.image("./10perSize.jpg")

  #matObjLoad <- R.matlab::readMat("../../inst/extdata/MatlabFiles/test/10percPicture/10PercResultsV3.mat")


  winSize <- 15

  darkChannel <- GetDarkChannel(im, winSize)
  expect_equal_to_reference(darkChannel, "darkChannelReference.rds")

  atmosphere <- GetAtmosphere(im, darkChannel)
  expect_equal_to_reference(atmosphere, "atmosphereReference.rds")


  omega <- 0.95
  transmissionEst <- GetTransmissionEstimate(im, atmosphere, omega, winSize)
  expect_equal_to_reference(transmissionEst, "transmissionEstReference.rds")


  radiance <- GetRadiance(im, transmissionEst, atmosphere)
  expect_equal_to_reference(radiance, "radianceReference.rds")


  #n <- imager::width(im)
  #m <- imager::height(im)
  ##trimapAll <- array(0, c(n, m, 1, 3))#n and m inverted due to cimg representation
  #laplacian <- GetMattingLaplacian(im)
  #expect_equal_to_reference(laplacian, "laplacianReference.rds")
  #matrixMatLoadLaplacian <- matObjLoad$laplacian
  #expect_equal(laplacian, matrixMatLoadLaplacian)

  lambda <- 0.0001
  dehaze <- Dehaze(im, omega, winSize,lambda)
  expect_equal_to_reference(dehaze, "dehazeReference.rds")
  dehaze2 <- Dehaze(im)
  expect_equal(dehaze, dehaze2)
  # Still with refined transmission
  #expect_equal_to_reference(dehaze, "dahazeReference.rds")
  #matrixMatLoadDeHaze <- matObjLoad$result
  #drop the temporal (z) dimension
  #expect_equal(drop(dehaze), matrixMatLoadDeHaze)


})
