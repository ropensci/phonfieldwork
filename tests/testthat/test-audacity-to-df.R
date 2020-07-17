library(phonfieldwork)
context("Tests for 'tier_to_df()' function")

file_1 <- system.file("extdata", "test_audacity.txt", package = "phonfieldwork")

test_that("tier_to_df", {
  expect_equal(length(audacity_to_df(file_1)), 4)
})
