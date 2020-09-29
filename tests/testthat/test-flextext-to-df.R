test_that("flextext_to_df_general", {
  skip_on_cran()
  file_name <- "additional_file/zilo_test.flextext"
  df <- flextext_to_df(file_name)
  expect_true(nrow(df) == 1170)
})
