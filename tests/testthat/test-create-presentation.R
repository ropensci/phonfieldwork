library(phonfieldwork)
context("Tests for 'create_presentation()' functions")

tmp <- tempdir()
create_presentation(stimuli = 1:3, output_dir = tmp)
create_presentation(stimuli = 1:3, output_dir = tmp, render = FALSE)

test_that("create_presentation", {
  expect_true("stimuli_presentation.html" %in% list.files(tmp))
})
