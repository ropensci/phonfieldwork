library(phonfieldwork)
context("Tests for 'set_textgrid_names()' function")

file1 <- system.file("extdata", "test.TextGrid", package = "phonfieldwork")
file2 <- readLines(file1)
r1 <- set_textgrid_names(file1,
                         tiers = 3,
                         names = "new_name", write = FALSE)
r2 <- set_textgrid_names(file2,
                         tiers = 3,
                         names = "new_name", write = FALSE)
test_that("tier_to_df", {
  expect_equal(r1[63], '        name = "new_name"')
  expect_equal(r2[63], '        name = "new_name"')
})

