#' Read multiple files from the folder
#'
#' This function reads multiple files from the folder. The first argument is the path, the second argument is the phonfieldwork function that define the type of files.
#'
#' @author George Moroz <agricolamz@gmail.com>
#'
#' @param path to a folder with multiple sound files.
#' @param FUN should be one of the following functions: \code{\link{get_sound_duration}}, \code{\link{audacity_to_df}}, \code{\link{eaf_to_df}}, \code{\link{exb_to_df}}, \code{\link{flextext_to_df}}, \code{\link{formant_to_df}}, \code{\link{intensity_to_df}}, \code{\link{pitch_to_df}}, \code{\link{srt_to_df}}, \code{\link{textgrid_to_df}}
#'
#' @examples
#'
#' read_from_folder(system.file("extdata", package = "phonfieldwork"), get_sound_duration)
#'
#' @export

read_from_folder <- function(path, FUN) {
  phonfieldwork_functions <- c(
    "get_sound_duration", "audacity_to_df", "eaf_to_df", "exb_to_df",
    "flextext_to_df", "formant_to_df", "intensity_to_df", "pitch_to_df",
    "srt_to_df", "textgrid_to_df"
  )
  exts <- c(
    ".wav$", ".txt$", ".eaf$", ".exb$", ".flextext$", ".Formant$",
    ".Intensity$", ".Pitch$", ".srt$", ".TextGrid$"
  )
  input_function <- as.character(substitute(FUN))
  match.arg(input_function, phonfieldwork_functions)
  ext <- exts[phonfieldwork_functions %in% input_function]
  path <- normalizePath(path)
  files <- list.files(path, pattern = ext, full.names = TRUE)
  do.call(rbind, lapply(files, FUN = FUN))
}
