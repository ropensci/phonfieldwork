#' Read multiple files from the folder
#'
#' This function reads multiple files from the folder. The first argument is the path, the second argument is the type of files to read.
#'
#' @author George Moroz <agricolamz@gmail.com>
#'
#' @param path to a folder with multiple sound files.
#' @param type should be one of the following: "duration", "audacity", "eaf", "exb", "flextext", "formant", "intensity", "picth", "srt", "textgrid"
#'
#' @examples
#'
#' read_from_folder(system.file("extdata", package = "phonfieldwork"), "eaf")
#'
#' @export

read_from_folder <- function(path, type) {
  # check for correct type --------------------------------------------------
  possible_types <-
    c(
      "duration",
      "audacity",
      "eaf",
      "exb",
      "flextext",
      "formant",
      "intensity",
      "picth",
      "srt",
      "textgrid"
    )
  match.arg(type, possible_types, several.ok = FALSE)

  # get extension -----------------------------------------------------------
  ext <- switch(
    type,
    duration = "(.wave?$)|(.WAVE?$)",
    audacity = ".txt$",
    eaf = ".eaf$",
    exb = ".exb$",
    flextext = ".flextext$",
    formant = ".Formant$",
    intensity = ".Intensity$",
    picth = ".Pitch$",
    srt = ".srt$",
    textgrid = ".TextGrid$"
  )

  # get apropriate function -------------------------------------------------
  FUN <- switch(
    type,
    duration = "get_sound_duration(x)",
    audacity = "audacity_to_df(x)",
    eaf = "eaf_to_df(x)",
    exb = "exb_to_df(x)",
    flextext = "flextext_to_df(x)",
    formant = "formant_to_df(x)",
    intensity = "intensity_to_df(x)",
    picth = "pitch_to_df(x)",
    srt = "srt_to_df(x)",
    textgrid = "textgrid_to_df(x)"
  )

  path <- normalizePath(path)
  files <- list.files(path, pattern = ext, full.names = TRUE)
  do.call(rbind, lapply(
    files,
    FUN = function(x) {
      tryCatch(
        eval(parse(text = FUN)),
        error = function(e) {
          warning("Error while reading from ", x, ":\n", e$message, "\n")
          return(NA)
        }
      )
    }
  ))
}
