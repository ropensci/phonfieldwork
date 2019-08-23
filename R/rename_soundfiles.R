#' Rename soundfiles
#'
#' Rename soundfiles using the template from user.
#'
#' @author George Moroz <agricolamz@gmail.com>
#'
#' @param path path to the directory with soundfiles.
#' @param stimuli character vector of stimuli
#' @param translations character vector of translations (optonal)
#' @param prefix character vector of length one containing prefix for file names
#' @param suffix character vector of length one containing suffix for file names
#' @param order numeric vector that define the order of stimuli. By default the order of the stimuli is taken.
#' @param backup logical. If TRUE, function creates backup folder with all files. By default is TRUE.
#' @return no output
#' @export
#'

rename_soundfiles <- function(stimuli,
                              translations = NULL,
                              prefix = NULL,
                              suffix = NULL,
                              order = NULL,
                              path,
                              backup = TRUE) {
  path <- normalizePath(path)
  files <- list.files(path)

  unlist(
    lapply(seq_along(files), function(x){
      res <- unlist(strsplit(files, "\\."))
      res[length(res)]
      })) ->
    extension

  not_wav <- which(!(tolower(extension) %in% "wav"))
  if(length(not_wav) > 0){
    stop(paste0(c("There are some non-wav files:", files[not_wav]),
                collapse = "\n"))
  }

  if(isTRUE(backup)){
    dir.create(path = paste0(path, "/backup"))
    file.copy(paste0(path, "/", files), paste0(path, "/backup/", files))
  }
  if(length(files) > length(stimuli)){
    stop("Number of files is greater then number of stimuli")
  } else if(length(files) < length(stimuli)){
    stop("Number of files is smaller then number of stimuli")
  }
  if(is.null(order)){
    order <- seq_along(stimuli)
  } else if(length(stimuli) != length(order)){
    stop("Stimuli and order vectors have different length")
  }

  medial_part <- if(!is.null(translations)){
    paste0(stimuli[order], "_", translations[order])
    } else {
      stimuli[order]
      }

  file.rename(paste0(path, "/", files),
              paste0(path, "/", prefix, medial_part, suffix, ".", extension[1]))
}

