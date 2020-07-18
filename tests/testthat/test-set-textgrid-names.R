library(phonfieldwork)
context("Tests for 'set_textgrid_names()' function")

file1 <- system.file("extdata", "test.TextGrid", package = "phonfieldwork")
file2 <- readLines(file1)

test_that("tier_to_df", {
  expect_equal(set_textgrid_names(file1,
                                  tiers = 3,
                                  names = "new_name", write = FALSE)[63],
               '        name = "new_name"')
  expect_equal(set_textgrid_names(file2,
                                  tiers = 3,
                                  names = "new_name", write = FALSE)[63],
               '        name = "new_name"')
})

