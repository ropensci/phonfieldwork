test_that("create_glossed_document", {
  skip_on_cran()
  file_name <- "additional_file/zilo_test.flextext"
  tmp <- tempdir()
  create_glossed_document(file_name, output_dir = tmp, output_format = "docx")
  create_glossed_document(file_name, output_dir = tmp, output_format = "html")
  create_glossed_document(flextext_to_df(file_name),
    output_dir = tmp,
    output_file = "glossed_document2",
    output_format = "html"
  )

  test_that("create_glossed_document", {
    expect_true(length(list.files(tmp, pattern = "html$|docx$")) == 3)
  })
  rm(tmp)
})
