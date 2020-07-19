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

create_viewer(audio_dir = paste0(tmp, "/audio"),
              picture_dir = paste0(tmp, "/pic"),
              table = data.frame(glottocode = c("stan1293", "russ1263")),
              output_file = "with_a_map",
              output_dir = paste0(tmp, "/result"),
              map = TRUE)

test_that("create_viewer", {
  expect_true(length(list.files(paste0(tmp, "/result"), "\\.html$")) == 2)
  expect_error(create_viewer(audio_dir = paste0(tmp, "/audio"),
                             picture_dir = paste0(tmp, "/pic"),
                             table = data.frame(id = 1:2),
                             output_file = "with_a_map",
                             output_dir = paste0(tmp, "/result"),
                             map = TRUE),
               'If you want to create a map in a viewer, you need to add a glottocode \\(or latitude and longitude\\) column to the datafarame in a table argument\\.')
  })

unlink(paste0(tmp, "/result"))
unlink(paste0(tmp, "/audio"))
unlink(paste0(tmp, "/pic"))
rm(tmp)
