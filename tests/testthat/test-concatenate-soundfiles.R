library(phonfieldwork)
context("Tests for 'concatenate_soundfiles()' functions")
s1 <- system.file("extdata", "test.wav", package = "phonfieldwork")
s2 <- system.file("extdata", "post.wav", package = "phonfieldwork")
tdir <- tempdir()
file.copy(c(s1, s2), tdir)
list.files(tdir)
concatenate_soundfiles(path = tdir, result_file_name = "concatenated")

test_that("concatenate_soundfiles", {
  expect_true("concatenated.wav" %in% list.files(tdir))
  expect_true("concatenated.TextGrid" %in% list.files(tdir))
})

rm(tdir)
