library(phonfieldwork)
context("Tests for 'draw_spectrogram()' functions")
tdir <- tempdir()
dir.create(paste0(tdir, "/draw_spectrogram"))
grDevices::png(filename = paste0(tdir, "/draw_spectrogram/test1.png"))
draw_spectrogram(system.file("extdata", "test.wav", package = "phonfieldwork"))
supress_message <- grDevices::dev.off()

list.files()
test_that("draw_spectrogram", {
  expect_equal(list.files(paste0(tdir, "/draw_spectrogram"), "\\.png$"),
               c("test1.png"))
})
unlink(paste0(tdir, "/draw_spectrogram"))
rm(tdir)
