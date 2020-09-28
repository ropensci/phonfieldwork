file1 <- system.file("extdata", "test.Intensity", package = "phonfieldwork")
file2 <- readLines(file1)

test_that("intensity_to_df", {
  expect_equal(nrow(intensity_to_df(file1)), 74)
  expect_equal(nrow(intensity_to_df(file2)), 74)
})
