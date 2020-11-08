test_that("exb_to_df", {
  file_name <- system.file("extdata", "test.exb", package = "phonfieldwork")
  df1 <- exb_to_df(file_name)

  expect_true(nrow(df1) == 8)
})
