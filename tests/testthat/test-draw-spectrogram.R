library(phonfieldwork)
context("Tests for 'draw_spectrogram()' functions")

test_that("draw_spectrogram_general", {
  skip_on_cran()
  tdir <- tempdir()
  dir.create(paste0(tdir, "/draw_spectrogram"))
  grDevices::png(filename = paste0(tdir, "/draw_spectrogram/test1.png"))
  draw_spectrogram(system.file("extdata", "test.wav", package = "phonfieldwork"))
  supress_message <- grDevices::dev.off()

  raven_an <- data.frame(time_start = 450,
                         time_end  = 520,
                         freq_low = 3,
                         freq_high = 4.5)
  grDevices::png(filename = paste0(tdir, "/draw_spectrogram/test2.png"))
  draw_sound(system.file("extdata", "test.wav", package = "phonfieldwork"),
             raven_annotation = raven_an)
  supress_message <- grDevices::dev.off()

  test_that("draw_spectrogram", {
    skip_on_cran()
    expect_equal(list.files(paste0(tdir, "/draw_spectrogram"), "\\.png$"),
                 c("test1.png", "test2.png"))
  })
  unlink(paste0(tdir, "/draw_spectrogram"))
  rm(tdir)
})
