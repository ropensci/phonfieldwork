file_name <- system.file("extdata", "test.exb", package = "phonfieldwork")
df1 <- exb_to_df(file_name)

test_that("exb_to_df", {
  expect_true(nrow(df1) == 8)
})
