#' Concatenate sounds
#'
#' Creates a sound file and a Praat TextGrid whose interval labels are the original names of the sound
#'
#' @author George Moroz <agricolamz@gmail.com>
#'
#' @param path path to the directory with soundfiles.
#' @param file_name name of the result files (and TextGrid, if this argument equal TRUE).
#' @param textgrid logic value, whether create Praat TextGrid whose interval labels are the original names of the sound.
#' @return
#'
#' @examples
#' # create two files in a temprary folder "test_folder"
#' s1 <-  system.file("extdata", "test.wav", package = "phonfieldwork")
#' s2 <-  system.file("extdata", "post.wav", package = "phonfieldwork")
#' tdir <- tempdir()
#' file.copy(s1, tdir)
#' file.copy(s2, tdir)
#'
#' # here are two .wav files in a folder
#' list.files(tdir)
#' # [1] "post.wav" "test.wav"
#'
#' # Concatenate all files from the folder into concatenated.wav and create corresponding TextGrid
#' concatenate_soundfiles(file_name = "concatenated", path = tdir)
#'
#' list.files(tdir)
#' # [1] "concatenated.TextGrid" "concatenated.wav" "post.wav" "test.wav" ...
#' @return no output
#' @export
#' @importFrom tuneR readWave
#' @importFrom tuneR bind
#' @importFrom tuneR writeWave
#'

concatenate_soundfiles <- function(file_name,
                                   path,
                                   textgrid = TRUE){

# concatenate sounds ------------------------------------------------------

  files <- list.files(path = path)

  unlist(
    lapply(seq_along(files), function(i){
      res <- unlist(strsplit(files[i], "\\."))
      res[length(res)]
    })) ->
    extension

  files <- files[which(tolower(extension) %in% "wav")]
  if(length(files) == 0){
    stop("There is no any .wav files")
  }

  list <- lapply(paste0(path, "/", files), tuneR::readWave)
  sound <- Reduce(tuneR::bind, list)
  tuneR::writeWave(sound, paste0(path, "/", file_name, ".wav"))
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
               paste0(path, "/", file_name, ".TextGrid"))
  }
}

