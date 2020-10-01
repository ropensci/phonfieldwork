#' Concatenate sounds
#'
#' Creates a merged sound file from old sound files in a folder. If the annotation argument is not equal to \code{NULL}, it creates an annotation file (Praat .TextGrid, ELAN .eaf or EXMARaLDA .exb) with original sound names annotation.
#'
#' @author George Moroz <agricolamz@gmail.com>
#'
#' @param path path to the directory with soundfiles.
#' @param result_file_name name of the result and annotation files.
#' @param annotation character. There are several variants: "textgrid" for Praat TextGrid, "eaf" for ELAN's .eaf file, or "exb" for EXMARaLDA's .exb file. It is also possible to use \code{NULL} in order to prevent the creation of the annotation file.
#' @return
#'
#' @examples
#' # create two files in a temprary folder "test_folder"
#' s1 <- system.file("extdata", "test.wav", package = "phonfieldwork")
#' s2 <- system.file("extdata", "post.wav", package = "phonfieldwork")
#' tdir <- tempdir()
#' file.copy(c(s1, s2), tdir)
#'
#' # here are two .wav files in a folder
#' list.files(tdir)
#' # [1] "post.wav" "test.wav" ...
#'
#' # Concatenate all files from the folder into concatenated.wav and create
#' # corresponding TextGrid
#' concatenate_soundfiles(path = tdir, result_file_name = "concatenated")
#'
#' list.files(tdir)
#' # [1] "concatenated.TextGrid" "concatenated.wav" "post.wav" "test.wav" ...
#'
#' @return no output
#' @export
#' @importFrom tuneR readWave
#' @importFrom tuneR readMP3
#' @importFrom tuneR bind
#' @importFrom tuneR writeWave
#' @importFrom tools file_ext
#'

concatenate_soundfiles <- function(path,
                                   result_file_name = "concatenated",
                                   annotation = "textgrid") {
  match.arg(annotation, c("textgrid", "eaf", "exb"))

  # concatenate sounds ------------------------------------------------------
  files <- list.files(
    path = normalizePath(path),
    pattern = "(\\.WAVE?$)|(\\.wave?$)|(\\.MP3?$)|(\\.mp3?$)"
  )

  if (length(files) == 0) {
    stop("There is no any .wav or .mp3 files")
  }

  list <- lapply(paste0(normalizePath(path), "/", files), function(file_name) {
    ext <- tolower(tools::file_ext(file_name))
    if (ext == "wave" | ext == "wav") {
      s <- tuneR::readWave(file_name)
    } else if (ext == "mp3") {
      s <- tuneR::readMP3(file_name)
    }
  })

  sound_attributes <- lapply(seq_along(list), function(x) {
    data.frame(
      file = files[x],
      stereo = list[[x]]@stereo,
      samp.rate = list[[x]]@samp.rate,
      bit = list[[x]]@bit
    )
  })

  sound_attributes <- do.call(rbind, sound_attributes)

  problems <- lapply(sound_attributes, function(i) {
    length(unique(i)) != 1
  })


  if (TRUE %in% problems[-1]) {
    pos_probs <- c("channel representation", "sampling rate", "bit rate")
    problem_text <- paste(pos_probs[unlist(problems)[-1]], collapse = ", and ")
    message(sound_attributes[, unlist(problems)])
    stop(paste0(
      "You have a problem with ",
      problem_text, ". Sampling rate, resolution (bit), and number of
                channels should be the same across all recordings."
    ))
  }
  sound <- do.call(tuneR::bind, list)
  tuneR::writeWave(sound, paste0(path, "/", result_file_name, ".wav"))

  # create an annotation ----------------------------------------------------
  if (!is.null(annotation)){
    if (annotation[1] == "textgrid") {
      duration <- unlist(lapply(list, function(i) {
        length(i@left) / i@samp.rate
      }))
      start_time <- c(0, cumsum(duration[-length(duration)]))
      end_time <- cumsum(duration)
      my_textgrid <- data.frame(
        TierNumber = 1,
        TierName = "annotation",
        TierType = "IntervalTier",
        Index = seq_along(files),
        StartTime = start_time,
        EndTime = end_time,
        Label = sort(files),
        stringsAsFactors = FALSE
      )
      writeLines(
        c(
          'File type = "ooTextFile"',
          'Object class = "TextGrid"',
          "",
          "xmin = 0 ",
          paste0("xmax = ", end_time[length(end_time)]),
          "tiers? <exists> ",
          "size = 1 ",
          "item []: ",
          "    item [1]:",
          '        class = "IntervalTier"',
          '        name = "labels"',
          "        xmin = 0",
          paste0("        xmax = ", end_time[length(end_time)]),
          paste0("        intervals: size = ", length(duration)),
          paste0(paste0("        intervals [", my_textgrid$Index, "]:"),
                 "\n",
                 paste0("            xmin = ", my_textgrid$StartTime),
                 "\n",
                 paste0("            xmax = ", my_textgrid$EndTime),
                 "\n",
                 paste0('            text = "', my_textgrid$Label, '"'),
                 "\n",
                 collapse = ""
          )
        ),
        paste0(path, "/", result_file_name, ".TextGrid")
      )
    } else if (annotation[1] == "eaf") {
      warning("Will be done in the future")
    } else if (annotation[1] == "exb") {
      warning("Will be done in the future")
    }
  }
}
