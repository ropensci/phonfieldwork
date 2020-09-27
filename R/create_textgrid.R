create_textgrid <- function(file_name,
                            tiers,
                            point_tiers = NULL) {
  file_name <- normalizePath(file_name)
  spl <- unlist(strsplit(basename(file_name), "\\."))
  name <- substring(
    basename(file_name),
    1,
    nchar(basename(file_name)) - nchar(spl[length(spl)]) - 1
  )
  textgrid_path <- paste0(dirname(file_name), name, ".TextGrid")

  dur <- get_sound_duration(file_name)

  fileConn <- file(textgrid_path)
  writeLines(
    paste0(
      'File type = "ooTextFile"\nObject class = "TextGrid"\n\nxmin = 0\n',
      "xmax = ",
      dur$duration,
      "\ntiers? <exists>"
    ),
    fileConn
  )
  close(fileConn)
  file.show(textgrid_path)
}
