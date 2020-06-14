#' Concatenate sounds
#'
#' Creates a sound file and a Praat TextGrid whose interval labels are the original names of the sound
#'
#' @author George Moroz <agricolamz@gmail.com>
#'
#' @param path path to the directory with soundfiles.
#' @param file_name name of the result and annotation files.
#' @param annotation there are several variants: "textgrid" for Praat TextGrid, "eaf" for ELAN's .eaf file, or "exb" for EXMARaLDA's .exb file
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
#' @importFrom tuneR readMP3
#' @importFrom tuneR bind
#' @importFrom tuneR writeWave
#'

concatenate_soundfiles <- function(file_name,
                                   path,
                                   annotation = "textgrid"){

# concatenate sounds ------------------------------------------------------

  files <- list.files(path = path, pattern = ".wav$")

  if(length(files) == 0){
    stop("There is no any .wav files")
  }

  list <- lapply(paste0(path, "/", files), function(file_name){
    ext <- tolower(substring(file_name, regexpr("\\..*$", file_name) + 1))
    if(ext == "wave"|ext == "wav"){
      s <- tuneR::readWave(file_name)
    } else if(ext == "mp3"){
      s <- tuneR::readMP3(file_name)
    } else{
      stop("The concatenate_soundfiles() functions works only with .wav(e) or .mp3 formats")
    }})
  sound <- Reduce(tuneR::bind, list)
  tuneR::writeWave(sound, paste0(path, "/", file_name, ".wav"))
# create a TextGrid -------------------------------------------------------

  if(annotation == "textgrid"){
    duration <- unlist(lapply(list, function(i){length(i@left)/i@samp.rate}))
    start_time <- c(0, cumsum(duration[-length(duration)]))
    end_time <- cumsum(duration)
    my_textgrid <- data.frame(TierNumber = 1,
                              TierName = "annotation",
                              TierType = "IntervalTier",
                              Index = seq_along(files),
                              StartTime = start_time,
                              EndTime = end_time,
                              Label = sort(files),
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
  } else if(annotation == "eaf"){
    print("Will be done in the future")
  } else if(annotation == "exb"){
      # options = list(sp_id = c(),
      #              sp_abbreviation = c(),
      #              sp_sex = c(),
      #              sp_language_used = c(),
      #              sp_l1 = c(),
      #              sp_l2 = c(),
      #              sp_atr = list(list(attr_names = c(),
      #                                 attr_values = c())))
    print("Will be done in the future")
  } else if(annotation != "exb" &
            annotation != "eaf" &
            annotation != "TextGrid" &
            !is.null(annotation)){
    warning('Annotation was not created. The only variant for annotation argument are: "textgrid", "eaf", "exb" or NULL')
  }
}

