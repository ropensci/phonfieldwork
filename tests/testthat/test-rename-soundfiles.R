library(phonfieldwork)
context("Tests for 'rename_soundfiles()' functions")
s1 <- system.file("extdata", "test.wav", package = "phonfieldwork")
s2 <- system.file("extdata", "post.wav", package = "phonfieldwork")
tdir <- tempdir()
dir.create(paste0(tdir, "/rename_audio"))
file.copy(c(s1, s2), paste0(tdir, "/rename_audio"))
rename_soundfiles(
  stimuli = c("s1", "s2"),
  path = paste0(tdir, "/rename_audio")
)
test_that("rename_soundfiles", {
  expect_equal(
    list.files(paste0(tdir, "/rename_audio"), "\\.wav$"),
    c("1_s1.wav", "2_s2.wav")
  )
})
unlink(paste0(tdir, "/rename_audio"))
rm(tdir)
