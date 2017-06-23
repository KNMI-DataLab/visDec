context("cpp")


library(visDec)

test_that("Cpp code works", {
  expect_equal(4, timesTwo(2))
})
