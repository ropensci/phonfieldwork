test_that("get_textgrid_names", {
  file1 <- system.file("extdata", "test.TextGrid", package = "phonfieldwork")
  file2 <- readLines(file1)

  expect_equal(
    get_textgrid_names(file1),
    c("intervals", "empty_intervals", "points")
  )
  expect_equal(
    get_textgrid_names(file2),
    c("intervals", "empty_intervals", "points")
  )
})
