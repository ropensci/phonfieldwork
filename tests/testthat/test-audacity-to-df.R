test_that("audacity_to_df", {
  file_1 <- system.file("extdata", "test_audacity.txt", package = "phonfieldwork")
  expect_equal(length(audacity_to_df(file_1)), 4)
})
