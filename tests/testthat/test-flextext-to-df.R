library(phonfieldwork)
context("Tests for 'flextext_to_df()' functions")
file_name <- "https://raw.githubusercontent.com/agricolamz/phonfieldwork/master/vignettes/files/zilo_test.flextext"
df <- flextext_to_df(file_name)
test_that("flextext_to_df", {
  expect_true(nrow(df) == 1170)
})
