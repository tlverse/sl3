context("test-options.R -- default options")

test_that("sl3 options work as expected", {
  sl3:::.onLoad()
  sl3:::.onAttach()

  sl3Options()
  sl3Options("sl3.verbose")
  suppressMessages({
    expect_error(sl3Options("blahblah"))
  })
  suppressMessages({
    expect_error(sl3Options("blahblah", 5))
  })

  sl3Options("sl3.verbose", TRUE)
  sl3Options("sl3.verbose")
  sl3Options("sl3.verbose", FALSE)
})
