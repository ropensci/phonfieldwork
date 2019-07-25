#' Concatenate sounds
#'
#' Creates a sound file and a Praat TextGrid whose interval labels are the original names of the sound
#'
#' @author George Moroz <agricolamz@gmail.com>
#'
#' @param filename name of the result files (and TextGrid, if this argument equal TRUE)
#' @param textgrid logic value, whether create Praat TextGrid whose interval labels are the original names of the sound
#' @param tier tier number or name that should be used as base for extraction and names
#' @param prefix character vector containing prefix(es) for file names
#' @param suffix character vector containing suffix(es) for file names
#' @param autonumber if TRUE automatically add number of extracted sound to the filename. Prevents from creating a duplicated files
#' @param path path to the directory with soundfiles.
#'
#' @export
#' @importFrom tuneR readWave
#' @importFrom tuneR bind
#' @importFrom tuneR writeWave
#'


extract_intervals <- function(filename,
                              textgrid,
                              tier = 1,
                              prefix = NULL,
                              suffix = NULL,
                              autonumber = TRUE,
                              path = getwd()){
  s <- tuneR::readWave(filename)
  tg <- readLines(textgrid)

# get start, end and annotation -------------------------------------------

  tg_df <- tier_to_df(textgrid, tier)
  starts <- tg_df[tg_df$annotation != "", "start"]
  ends <- tg_df[tg_df$annotation != "", "end"]
  if(isTRUE(autonumber)){
    auto <- 1:nrow(tg_df)
  } else {
    auto <- NULL
  }
  annotations <- paste0(prefix,
                        tg_df[tg_df$annotation != "", "annotation"],
                        suffix,
                        "-",
                        auto)

  lapply(seq_along(starts), function(i){
    s_fragment <- tuneR::extractWave(s, from = starts[i], to = ends[i], xunit = "time")
    tuneR::writeWave(s_fragment, paste0(path, "/", annotations[i], ".wav"))
  }
  )
}
