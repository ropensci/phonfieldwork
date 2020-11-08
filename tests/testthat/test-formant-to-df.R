test_that("formant_to_df", {
  file1 <- system.file("extdata", "e.Formant", package = "phonfieldwork")
  file2 <- readLines(file1)

  expect_equal(nrow(formant_to_df(file1)), 164)
  expect_equal(nrow(formant_to_df(file2)), 164)
})
