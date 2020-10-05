textgrids <- read_from_folder(system.file("extdata", package = "phonfieldwork"),
                              textgrid_to_df)

test_that("read_from_folder", {
  expect_true(nrow(textgrids) == 42)
})
