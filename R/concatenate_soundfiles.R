#' Concatenate sounds
#'
#' Creates a sound file and a Praat TextGrid whose interval labels are the original names of the sound
#'
#' @author George Moroz <agricolamz@gmail.com>
#'
#' @param path path to the directory with soundfiles.
#' @param filename name of the result files (and TextGrid, if this argument equal TRUE)
#' @param textgrid logic value, whether create Praat TextGrid whose interval labels are the original names of the sound
#'
#' @export
#' @importFrom tuneR readWave
#' @importFrom tuneR bind
#' @importFrom tuneR writeWave
#'

concatenate_soundfiles <- function(path = getwd(),
                                   filename,
                                   textgrid = TRUE){

# concatenate sounds ------------------------------------------------------

  files <- list.files()

  unlist(
    lapply(seq_along(files), function(i){
      res <- unlist(strsplit(files[i], "\\."))
      res[length(res)]
    })) ->
    extension

  not_wav <- which(!(tolower(extension) %in% "wav"))
  if(length(not_wav) > 0){
    stop(paste0(c("There are some non-wav files:", files[not_wav]), collapse = "\n"))
  }

  list <- lapply(files, tuneR::readWave)
  sound <- Reduce(tuneR::bind, list)
  tuneR::writeWave(sound, paste0(filename, ".wav"))
# create a TextGrid -------------------------------------------------------

  if(isTRUE(textgrid)){
    duration <- unlist(lapply(list, function(i){length(i@left)/i@samp.rate}))
    start_time <- c(0, cumsum(duration[-length(duration)]))
    end_time <- cumsum(duration)
    my_textgrid <- data.frame(TierNumber = 1,
                              TierName = "annotation",
                              TierType = "IntervalTier",
                              Index = seq_along(files),
                              StartTime = start_time,
                              EndTime = end_time,
                              Label = files,
                              stringsAsFactors = FALSE)
    writeLines(c('File type = "ooTextFile"',
                 'Object class = "TextGrid"',
                 '',
                 'xmin = 0 ',
                 paste0('xmax = ', end_time[length(end_time)]),
                 'tiers? <exists> ',
                 'size = 1 ',
                 'item []: ',
                 '    item [1]:',
                 '        class = "IntervalTier"',
                 '        name = "labels"',
                 '        xmin = 0',
                 paste0('        xmax = ', end_time[length(end_time)]),
                 paste0('        intervals: size = ', length(duration)),
                 paste0(paste0('        intervals [', my_textgrid$Index, ']:'),
                        "\n",
                        paste0('            xmin = ', my_textgrid$StartTime),
                        "\n",
                        paste0('            xmax = ', my_textgrid$EndTime),
                        "\n",
                        paste0('            text = "', my_textgrid$Label, '"'),
                        "\n",
                        collapse = "")),
               paste0(filename, ".TextGrid"))
  }
}

