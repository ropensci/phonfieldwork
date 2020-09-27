library(phonfieldwork)
context("Tests for 'eaf_to_df()' functions")
file_name <- system.file("extdata", "test.eaf", package = "phonfieldwork")
df1 <- eaf_to_df(file_name)
df2 <- eaf_to_df(
  eafs_from_folder =
    system.file("extdata", package = "phonfieldwork")
)

test_that("eaf_to_df", {
  expect_true(nrow(df1) == 12)
  expect_true(nrow(df2) == 12)
})
