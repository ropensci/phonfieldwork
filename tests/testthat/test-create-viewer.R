library(phonfieldwork)
context("Tests for 'create_viewer()' functions")
tmp1 <- tempdir()
download.file("https://github.com/agricolamz/phonfieldwork/raw/master/vignettes/s1/s1_sounds/1_s1_%C3%A6.wav",
              paste0(tmp1, "/", "1_s1.wav"))
download.file("https://github.com/agricolamz/phonfieldwork/raw/master/vignettes/s1/s1_sounds/2_s1_%C4%B1.wav",
              paste0(tmp1, "/", "1_s2.wav"))
download.file("https://github.com/agricolamz/phonfieldwork/raw/master/vignettes/s1/s1_sounds/3_s1_%C9%92.wav",
              paste0(tmp1, "/", "1_s3.wav"))

tmp2 <- tempdir()
download.file("https://github.com/agricolamz/phonfieldwork/raw/master/vignettes/s1/s1_pics/1_s1_%C3%A6.png",
              paste0(tmp2, "/", "1_s1.png"))
download.file("https://github.com/agricolamz/phonfieldwork/raw/master/vignettes/s1/s1_pics/2_s1_%C4%B1.png",
              paste0(tmp2, "/", "2_s1.png"))
download.file("https://github.com/agricolamz/phonfieldwork/raw/master/vignettes/s1/s1_pics/3_s1_%C9%92.png",
              paste0(tmp2, "/", "3_s1.png"))

tmp3 <- tempdir()
create_viewer(audio_dir = tmp1,
              picture_dir = tmp2,
              table = data.frame(id = 1:3),
              output_dir = tmp3)

test_that("create_viewer", {
  expect_true(length(list.files(tmp3, "\\.html$")) == 1)
})
