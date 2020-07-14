#' Get file(s) duration
#'
#' Calculate sound(s) duration.
#'
#' @author George Moroz <agricolamz@gmail.com>
#'
#' @param file_name a sound file
#' @param sounds_from_folder path to a folder with multiple sound files.
#' If this argument is not \code{NULL}, then the function goes through all
#' files and calculates duration for all of them.
#'
#' @examples
#' get_sound_duration(
#'   sounds_from_folder = system.file("extdata", package = "phonfieldwork"))
#'
#' @export
#'
#' @importFrom tuneR readWave
#' @importFrom tuneR readMP3

get_sound_duration <- function(file_name,
                               sounds_from_folder = NULL){
  if(is.null(sounds_from_folder)){
    if(class(file_name) == "Wave"){
      s <- file_name
    } else{
      ext <- unlist(strsplit(file_name, "\\."))
      ext <- ext[length(ext)]

      if(ext == "wave"|ext == "wav"){
        s <- tuneR::readWave(file_name, header=TRUE)
        duration <- s$samples/s$sample.rate
      } else if(ext == "mp3"){
        s <- tuneR::readMP3(file_name)
        duration <- length(s@left)/s@samp.rate
      } else{
        stop("The get_sound_durations() functions works only with .wav(e)
             or .mp3 formats")
      }
    }
    return(data.frame(file =
                        rev(unlist(strsplit(normalizePath(file_name), "/")))[1],
                      duration = duration))
  } else{
    path <- normalizePath(sounds_from_folder)
    sounds_from_folder <- list.files(path,
                                     pattern = "(\\.wave?$)|(\\.mp3$)|
                                     (\\.WAVE?$)|(\\.MP3$)")
    sounds_from_folder <- paste0(path, "/",
                                 sounds_from_folder)
    l <- lapply(sounds_from_folder, phonfieldwork::get_sound_duration)
    do.call(rbind, l)
  }
}
