library(phonfieldwork)
context("Tests for 'draw_sound()' functions")
tdir <- tempdir()
dir.create(paste0(tdir, "/draw_sound"))
grDevices::png(filename = paste0(tdir, "/draw_sound/test1.png"))
draw_sound(system.file("extdata", "test.wav", package = "phonfieldwork"))
supress_message <- grDevices::dev.off()

grDevices::png(filename = paste0(tdir, "/draw_sound/test2.png"))
draw_sound(system.file("extdata", "test.wav", package = "phonfieldwork"),
           system.file("extdata", "test.TextGrid",
                       package = "phonfieldwork"))
supress_message <- grDevices::dev.off()

grDevices::png(filename = paste0(tdir, "/draw_sound/test3.png"))
draw_sound(system.file("extdata", "test.wav", package = "phonfieldwork"),
           system.file("extdata", "test.TextGrid", package = "phonfieldwork"),
           pitch = system.file("extdata", "test.Pitch", package = "phonfieldwork"))
supress_message <- grDevices::dev.off()

list.files()
test_that("draw_sound", {
  expect_equal(list.files(paste0(tdir, "/draw_sound"), "\\.png$"),
               c("test1.png", "test2.png", "test3.png"))
})
unlink(paste0(tdir, "/draw_sound"))
rm(tdir)
