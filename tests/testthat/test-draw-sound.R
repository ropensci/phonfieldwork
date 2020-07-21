library(phonfieldwork)
context("Tests for 'draw_sound()' functions")

test_that("draw_sound_general", {
  skip_on_cran()
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
             pitch = system.file("extdata", "test.Pitch",
                                 package = "phonfieldwork"))
  supress_message <- grDevices::dev.off()

  grDevices::png(filename = paste0(tdir, "/draw_sound/test4.png"))
  draw_sound(system.file("extdata", "test.wav", package = "phonfieldwork"),
             pitch = system.file("extdata", "test.Pitch",
                                 package = "phonfieldwork"))
  supress_message <- grDevices::dev.off()

  grDevices::png(filename = paste0(tdir, "/draw_sound/test5.png"))
  draw_sound(system.file("extdata", "test.wav", package = "phonfieldwork"),
             system.file("extdata", "test.TextGrid", package = "phonfieldwork"),
             zoom = c(0.4, 0.65),
             pitch = system.file("extdata", "test.Pitch",
                                 package = "phonfieldwork"))
  supress_message <- grDevices::dev.off()

  draw_sound(system.file("extdata", "test.wav", package = "phonfieldwork"),
             system.file("extdata", "test.TextGrid", package = "phonfieldwork"),
             zoom = c(0.4, 0.65),
             pitch = system.file("extdata", "test.Pitch",
                                 package = "phonfieldwork"),
             output_file = paste0(tdir, "/draw_sound/test6"))

  list.files()
  test_that("draw_sound", {
    skip_on_cran()
    expect_equal(list.files(paste0(tdir, "/draw_sound"), "\\.png$"),
                 c("test1.png", "test2.png", "test3.png", "test4.png",
                   "test5.png", "test6.png"))
  })
  unlink(paste0(tdir, "/draw_sound"))
  rm(tdir)
})
