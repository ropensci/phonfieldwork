test_that("pitch_to_df", {
  file1 <- system.file("extdata", "test.Pitch", package = "phonfieldwork")
  file2 <- readLines(file1)

  expect_equal(nrow(pitch_to_df(file1)), 62)
  expect_equal(nrow(pitch_to_df(file2)), 62)
  expect_equal(nrow(pitch_to_df(file1, candidates = "all")), 426)
})
