test_that("read_from_folder", {
  textgrids <- read_from_folder(system.file("extdata", package = "phonfieldwork"),
                                "textgrid")

  expect_true(nrow(textgrids) == 42)
})
