library(phonfieldwork)
context("Tests for 'tier_to_df()' function")

file_1 <- system.file("extdata", "test.TextGrid", package = "phonfieldwork")
test_that("tier_to_df",
          expect_equal(tier_to_df(file_1)[,1], 1:5))

