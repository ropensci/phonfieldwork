#' Get file(s) duration
#'
#' Calculate sound(s) duration.
#'
#' @author George Moroz <agricolamz@gmail.com>
#'
#' @param file_name a sound file
#'
#' @return Dataframe with two columns: file name and duration
#'
#' @examples
#' get_sound_duration(
#'   system.file("extdata", "test.wav", package = "phonfieldwork")
#' )
#' @export
#'
#' @importFrom tuneR readWave
#' @importFrom tuneR readMP3
#' @importFrom tools file_ext

get_sound_duration <- function(file_name) {
  if (isa(file_name, "Wave")) {
    s <- file_name
    source <- "custom_file"
    duration <- length(s@left) / s@samp.rate
  } else {
    ext <- tolower(tools::file_ext(file_name))

    if (ext == "wave" | ext == "wav") {
      s <- tuneR::readWave(file_name, header = TRUE)
      duration <- s$samples / s$sample.rate
    } else if (ext == "mp3") {
      s <- tuneR::readMP3(file_name)
      duration <- length(s@left) / s@samp.rate
    } else {
      stop("The get_sound_duration() functions works only with .wav(e)
             or .mp3 formats")
    }
    source <- basename(file_name)
  }

  return(data.frame(
    file = source,
    duration = duration
  ))
}
