library(phonfieldwork)
context("Tests for 'textgrid_to_df()' functions")
file_name <- system.file("extdata", "test.TextGrid", package = "phonfieldwork")
t1 <- textgrid_to_df(file_name)
t2 <- textgrid_to_df(readLines(file_name))

tmp <- tempdir()
file.copy(system.file("extdata", "test.TextGrid", package = "phonfieldwork"),
          tmp)
t3 <- textgrid_to_df(textgrids_from_folder = tmp)

test_that("textgrid_to_df", {
  expect_true(nrow(t1) == 14)
  expect_true(nrow(t2) == 14)
  expect_true(nrow(t3) == 16) # WTF?!
})
