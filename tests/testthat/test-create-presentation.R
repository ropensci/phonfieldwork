test_that("create_presentation", {
  skip_on_cran()
  tmp <- tempdir()
  create_presentation(stimuli = 1:3, output_dir = tmp)
  create_presentation(stimuli = 1:3, output_dir = tmp, render = FALSE)
  expect_true("stimuli_presentation.html" %in% list.files(tmp))
  rm(tmp)
})
