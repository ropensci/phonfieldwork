test_that("create_empty_textgrid", {
  s1 <- system.file("extdata", "test.wav", package = "phonfieldwork")
  tdir <- tempdir(check = TRUE)
  create_empty_textgrid(get_sound_duration(s1),
                        path = tdir,
                        result_file_name = "test"
  )

  expect_equal(
    list.files(tdir, "\\.TextGrid"),
    "test.TextGrid"
  )

  unlink(paste0(tdir, "/make_txtgrd"))
  rm(tdir)
})
