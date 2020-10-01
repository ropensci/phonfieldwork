t1 <- system.file("extdata", "test.TextGrid", package = "phonfieldwork")
t2 <- system.file("extdata", "post.TextGrid", package = "phonfieldwork")
tdir <- tempdir(check = TRUE)
file.copy(c(t1, t2), tdir)
list.files(tdir)
concatenate_textgrids(path = tdir, result_file_name = "concatenated")

test_that("concatenate_textgrids", {
  expect_true("concatenated.TextGrid" %in% list.files(tdir))
})
file.remove(paste0(tdir, "/", c(basename(t1), basename(t2), "concatenated.TextGrid")))
rm(tdir)
