file_name <- system.file("extdata", "test.eaf", package = "phonfieldwork")
df1 <- eaf_to_df(file_name)

test_that("eaf_to_df", {
  expect_true(nrow(df1) == 12)
})
