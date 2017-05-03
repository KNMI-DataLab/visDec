context("Feature calculation")


test_that("Same input same output", {
  im <- imager::load.image("./Input/10perSize.jpg")

  featureSet <- ImageFeatures("./Input/10perSize.jpg")
  expect_equal_to_reference(featureSet, "./Reference/fullFeatureSet.rds")
  featureSet2 <- ImageFeatures("./Input/10perSize.jpg", x > 5)
  expect_equal_to_reference(featureSet2, "./Reference/fullFeatureSet2.rds")

  #matObjLoad <- R.matlab::readMat("../../inst/extdata/MatlabFiles/test/10percPicture/10PercResultsV3.mat")

  expect_equal_to_reference(DetectMeanEdges(im, 3), "./Reference/meanEdges.rds")

  winSize <- 15

  darkChannel <- GetDarkChannel(im, winSize)
  expect_equal_to_reference(as.matrix(darkChannel), "./Reference/darkChannelReference.rds")

  atmosphere <- GetAtmosphere(im, darkChannel)
  expect_equal_to_reference(atmosphere, "./Reference/atmosphereReference.rds")


  omega <- 0.95
  transmissionEst <- GetTransmission(im, atmosphere, omega, winSize)
  expect_equal_to_reference(as.matrix(transmissionEst), "./Reference/transmissionEstReference.rds")


  radiance <- GetRadiance(im, transmissionEst, atmosphere)
  expect_equal_to_reference(as.array(radiance), "./Reference/radianceReference.rds")

  radiance2 <- GetRadiance(im, omega = omega, winSize = winSize)
  expect_equal_to_reference(as.array(radiance2), "./Reference/radianceReference.rds")


  #n <- imager::width(im)
  #m <- imager::height(im)
  ##trimapAll <- array(0, c(n, m, 1, 3))#n and m inverted due to cimg representation
  #laplacian <- GetMattingLaplacian(im)
  #expect_equal_to_reference(laplacian, "laplacianReference.rds")
  #matrixMatLoadLaplacian <- matObjLoad$laplacian
  #expect_equal(laplacian, matrixMatLoadLaplacian)

  lambda <- 0.0001
  dehaze <- GetRadiance(im, omega = omega, winSize = winSize)
  expect_equal_to_reference(as.array(dehaze), "./Reference/dehazeReference.rds")
  dehaze2 <- GetRadiance(im, transmissionEst, atmosphere)
  expect_equal(dehaze, dehaze2)
  # Still with refined transmission
  #expect_equal_to_reference(dehaze, "dahazeReference.rds")
  #matrixMatLoadDeHaze <- matObjLoad$result
  #drop the temporal (z) dimension
  #expect_equal(drop(dehaze), matrixMatLoadDeHaze)


  avgHorizTrans <- GetHorizAvgTrans(im)
  expect_equal_to_reference(avgHorizTrans, "./Reference/avrHorizTransReference.rds")
  expect_equal_to_reference(GetFractalDim(avgHorizTrans), "./Reference/fractalDimTransmission.rds")
  expect_equal_to_reference(GetFractalDim(im), "./Reference/fractalDimImage.rds")
  expect_error(GetFractalDim("im"), "not defined for object of class", fixed = TRUE)

  expect_equal_to_reference(TransmissionChangepoint(avgHorizTrans), "./Reference/transmissionChangepoint.rds")
  expect_equal_to_reference(TransmissionSmoothness(avgHorizTrans), "./Reference/transmissionSmoothness.rds")

})
