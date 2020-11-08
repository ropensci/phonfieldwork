test_that("textgrid_to_df", {
  file_name1 <- system.file("extdata", "test.TextGrid", package = "phonfieldwork")
  file_name2 <- system.file("extdata", "test_short.TextGrid", package = "phonfieldwork")
  t1 <- textgrid_to_df(file_name1)
  t2 <- textgrid_to_df(file_name2)
  t3 <- textgrid_to_df(readLines(file_name1))

  expect_true(nrow(t1) == 14)
  expect_true(nrow(t2) == 14)
  expect_true(nrow(t3) == 14)
})
