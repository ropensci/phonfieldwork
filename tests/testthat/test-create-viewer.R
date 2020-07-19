library(phonfieldwork)
context("Tests for 'create_viewer()' functions")
tmp <- tempdir()
dir.create(paste0(tmp, "/audio"))
download.file("https://github.com/agricolamz/phonfieldwork/raw/master/vignettes/s1/s1_sounds/1_s1_%C3%A6.wav",
              paste0(tmp, "/audio/", "1_s1.wav"))
download.file("https://github.com/agricolamz/phonfieldwork/raw/master/vignettes/s1/s1_sounds/2_s1_%C4%B1.wav",
              paste0(tmp, "/audio/", "2_s1.wav"))

dir.create(paste0(tmp, "/pic"))
download.file("https://github.com/agricolamz/phonfieldwork/raw/master/vignettes/s1/s1_pics/1_s1_%C3%A6.png",
              paste0(tmp, "/pic/", "1_s1.png"))
download.file("https://github.com/agricolamz/phonfieldwork/raw/master/vignettes/s1/s1_pics/2_s1_%C4%B1.png",
              paste0(tmp, "/pic/", "2_s1.png"))

dir.create(paste0(tmp, "/result"))
create_viewer(audio_dir = paste0(tmp, "/audio"),
              picture_dir = paste0(tmp, "/pic"),
              table = data.frame(id = 1:2),
              output_dir = paste0(tmp, "/result"))
test_that("create_viewer", {
  expect_true(length(list.files(paste0(tmp, "/result"), "\\.html$")) == 1)
})

unlink(paste0(tmp, "/result"))
unlink(paste0(tmp, "/audio"))
unlink(paste0(tmp, "/pic"))
rm(tmp)
