library(phonfieldwork)
context("Tests for 'pitch_to_df()' function")

file1 <- system.file("extdata", "test.Pitch", package = "phonfieldwork")
file2 <- readLines(file1)

test_that("pitch_to_df", {
  expect_equal(nrow(pitch_to_df(file1)), 62)
  expect_equal(nrow(pitch_to_df(file2)), 62)
  expect_equal(nrow(pitch_to_df(file1, candidates = "all")), 426)
})

