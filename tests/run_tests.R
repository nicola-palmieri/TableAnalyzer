local({
  # Plot assertions use synthetic data; discard incidental graphics devices.
  previous <- options(device = function(...) grDevices::pdf(file = NULL))
  on.exit(options(previous), add = TRUE)
  testthat::test_dir("tests")
})
