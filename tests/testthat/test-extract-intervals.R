test_that("extract_intervals", {
  tdir <- tempdir()
  file.copy(system.file("extdata", "test.wav", package = "phonfieldwork"), tdir)

  extract_intervals(
    file_name = paste0(tdir, "/test.wav"),
    textgrid = system.file("extdata", "test.TextGrid",
                           package = "phonfieldwork"
    ),
    path = tdir
  )

  expect_true("2_e.wav" %in% list.files(tdir))

  rm(tdir)
})


