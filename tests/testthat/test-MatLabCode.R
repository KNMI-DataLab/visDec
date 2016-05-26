context("MatLabCode")

## TODO: Rename context
## TODO: Add more tests

test_that("multiplication works", {
  expect_equal(2 * 2, 6)
})

test_that("Same input same output", {
  expect_equal_to_reference(get_radiance(im), "radianceReference.rds")
  expect_equal...
})
