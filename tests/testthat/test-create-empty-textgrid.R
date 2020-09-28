s1 <- system.file("extdata", "test.wav", package = "phonfieldwork")
tdir <- tempdir()
dir.create(paste0(tdir, "/make_txtgrd"))
file.copy(s1, paste0(tdir, "/make_txtgrd"))

create_empty_textgrid(paste0(tdir, "/make_txtgrd/test.wav"))

test_that("create_empty_textgrid", {
  expect_equal(
    list.files(paste0(tdir, "/make_txtgrd"), "\\.TextGrid"),
    "test.TextGrid"
  )
})
unlink(paste0(tdir, "/make_txtgrd"))
rm(tdir)
