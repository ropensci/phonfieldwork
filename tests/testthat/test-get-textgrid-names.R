library(phonfieldwork)
context("Tests for 'get_textgrid_names()' function")

file1 <- system.file("extdata", "test.TextGrid", package = "phonfieldwork")
file2 <- readLines(file1)

test_that("get_textgrid_names", {
  expect_equal(
    get_textgrid_names(file1),
    c("intervals", "empty_intervals", "points")
  )
  expect_equal(
    get_textgrid_names(file2),
    c("intervals", "empty_intervals", "points")
  )
})
