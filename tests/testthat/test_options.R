context("test_options.R -- default options")
library(testthat)
library(sl3)

test_that("sl3 options work as expected", {
  sl3:::.onLoad()
  sl3:::.onAttach()

  sl3Options()
  sl3Options("sl3.verbose")
  expect_error(sl3Options("blahblah"))
  expect_error(sl3Options("blahblah", 5))

  sl3Options("sl3.verbose", TRUE)
  sl3Options("sl3.verbose")
  sl3Options("sl3.verbose", FALSE)
})
