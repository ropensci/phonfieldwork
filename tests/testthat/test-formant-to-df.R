library(phonfieldwork)
context("Tests for 'formant_to_df()' function")

file1 <- system.file("extdata", "e.Formant", package = "phonfieldwork")
file2 <- readLines(file1)

test_that("formant_to_df", {
  expect_equal(nrow(formant_to_df(file1)), 164)
  expect_equal(nrow(formant_to_df(file2)), 164)
})

