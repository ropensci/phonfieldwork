library(phonfieldwork)
context("Tests for 'flextext_to_df()' functions")
test_that("flextext_to_df_general", {
  skip_on_cran()
  file_name <- "https://raw.githubusercontent.com/agricolamz/phonfieldwork/master/vignettes/files/zilo_test.flextext"
  df <- flextext_to_df(file_name)
  test_that("flextext_to_df", {
    skip_on_cran()
    expect_true(nrow(df) == 1170)
  })
})
