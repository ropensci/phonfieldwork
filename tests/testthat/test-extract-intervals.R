library(phonfieldwork)
context("Tests for 'extract_intervals()' functions")

tdir <- tempdir()
file.copy(system.file("extdata", "test.wav", package = "phonfieldwork"), tdir)

extract_intervals(file_name = paste0(tdir, "/test.wav"),
                  textgrid = system.file("extdata", "test.TextGrid",
                                         package = "phonfieldwork"),
                  path = tdir)

test_that("extract_intervals", {
  expect_true("2_e.wav" %in% list.files(tdir))
})
