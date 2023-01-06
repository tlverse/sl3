context("test-density_utils -- density_utils")

test_that("make_bins() works with equal range binning", {
  x <- c(0, 1, 9)
  bins <- make_bins(x, type = "equal_range", n_bins = 2)
  expect_equal(bins, c(0, 4.5, 9))
  
  x <- c(-10, 1, 2, 3, 10)
  bins <- make_bins(x, type = "equal_range", n_bins = 5)
  expect_equal(bins, c(-10, -6, -2, 2, 6, 10))
  
  x <- c(-10, 1, 2, 3, 9)
  bins <- make_bins(x, type = "equal_range", n_bins = 5)
  expect_equal(bins, c(-10, -6.2, -2.4, 1.4, 5.2, 9.0))
})

test_that("make_bins() works with equal mass binning", {
  x <- c(0, 1, 9)
  bins <- make_bins(x, type = "equal_mass", n_bins = 2)
  expect_equal(bins, c(0, 1, 9))
  
  x <- c(-10, 0, 0, 10)
  bins <- make_bins(x, type = "equal_mass", n_bins = 2)
  expect_equal(bins, c(-10, 0, 10))
  
  x <- c(-10, -2, -2, 1, 1, 1, 10)
  bins <- make_bins(x, type = "equal_mass", n_bins = 3)
  expect_equal(bins, c(-10, -2, 1, 10))
})

test_that("discretize_variable() works with equal range binning", {
  x <- 1:10
  
  expected_res <- list(
    x_discrete = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2),
    x_in_bin = c(0, 1, 2, 3, 4, 0.5, 1.5, 2.5, 3.5, 4.5),
    breaks = c(1, 5.5, 10)
  )

  res <- discretize_variable(x, type = "equal_range", n_bins = 2)
  expect_equal(res, expected_res)
})

test_that("discretize_variable() works with equal mass binning", {
  x <- 1:10
  
  expected_res <- list(
    x_discrete = c(1, 1, 1, 2, 2, 2, 3, 3, 3, 3),
    x_in_bin = c(0, 1, 2, 0, 1, 2, 0, 1, 2, 3),
    breaks = c(1, 4, 7, 10)
  )
  
  res <- discretize_variable(x, type = "equal_mass", n_bins = 3)
  expect_equal(res, expected_res)
})
