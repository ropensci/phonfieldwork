file_name <- system.file("extdata", "test.exb", package = "phonfieldwork")
df1 <- exb_to_df(file_name)
df2 <- exb_to_df(
  exbs_from_folder =
    system.file("extdata", package = "phonfieldwork")
)

test_that("exb_to_df", {
  expect_true(nrow(df1) == 8)
  expect_true(nrow(df2) == 8)
})
