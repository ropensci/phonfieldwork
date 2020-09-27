library(phonfieldwork)
context("Tests for 'create_glossed_document()' functions")
test_that("create_glossed_document", {
  skip_on_cran()
  file_name <- "https://raw.githubusercontent.com/agricolamz/phonfieldwork/master/vignettes/files/zilo_test.flextext"
  tmp <- tempdir()
  create_glossed_document(file_name, output_dir = tmp)
  create_glossed_document(file_name, output_dir = tmp, output_format = "html")
  create_glossed_document(flextext_to_df(file_name),
    output_dir = tmp,
    output_file = "glossed_document2"
  )

  test_that("create_glossed_document", {
    skip_on_cran()
    expect_true(length(list.files(tmp, pattern = "html$|docx$")) == 3)
  })
  rm(tmp)
})
