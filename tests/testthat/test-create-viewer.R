test_that("create_viewer_general", {
  skip_on_cran()
  tmp <- tempdir()
  dir.create(paste0(tmp, "/result"))
  create_viewer(
    audio_dir = "additional_file/audio",
    picture_dir = "additional_file/pic",
    table = data.frame(id = 1:2),
    output_dir = paste0(tmp, "/result")
  )

  create_viewer(
    audio_dir = "additional_file/audio",
    picture_dir = "additional_file/pic",
    table = data.frame(glottocode = c("stan1293", "russ1263")),
    output_file = "with_a_map",
    output_dir = paste0(tmp, "/result"),
    map = TRUE
  )

  test_that("create_viewer", {
    expect_true(length(list.files(paste0(tmp, "/result"), "\\.html$")) == 2)
    expect_error(
      create_viewer(
        audio_dir = "additional_file/audio",
        picture_dir = "additional_file/pic",
        table = data.frame(id = 1:2),
        output_file = "with_a_map",
        output_dir = paste0(tmp, "/result"),
        map = TRUE
      ),
      paste0(
        "If you want to create a map in a viewer, you need to",
        " add a glottocode \\(or latitude and longitude\\)",
        " column to the datafarame in a table argument\\."
      )
    )
  })

  unlink(paste0(tmp, "/result/"))
  rm(tmp)
})
