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
#' @param order numeric vector that define the order of stimuli. By default the
#' order of the stimuli is taken.
#' @param autonumber logical. If TRUE, function creates an automatic numbering of files.
#' @param backup logical. If TRUE, function creates backup folder with all
#' files. By default is TRUE.
#' @param logging logical. If TRUE creates a .csv file with the correspondences of old names and new names. This could be useful for restoring in case something goes wrong.
#' @return no output
#' @export
#'
#' @importFrom utils write.csv

rename_soundfiles <- function(stimuli,
                              translations = NULL,
                              prefix = NULL,
                              suffix = NULL,
                              order = NULL,
                              path,
                              autonumber = TRUE,
                              backup = TRUE,
                              logging = TRUE) {
  path <- normalizePath(path)
  files <- list.files(path)

  extension <- unlist(
    lapply(seq_along(files), function(x){
      res <- unlist(strsplit(files[x], "\\."))
      res[length(res)]
    }))


  not_wav <- which(!(tolower(extension) %in% "wav"))
  if(length(not_wav) > 0){
    stop(paste0(c("There are some non-wav files:", files[not_wav]),
                collapse = "\n"))
  }

  if(isTRUE(backup)){
    dir.create(path = paste0(path, "/backup"))
    file.copy(paste0(path, "/", files), paste0(path, "/backup/", files))
  }
  if(length(files) > length(unique(stimuli))){
    stop("Number of files is greater then number of stimuli")
  } else if(length(files) < length(unique(stimuli))){
    stop("Number of files is smaller then number of stimuli")
  }
  if(is.null(order)){
    order <- seq_along(stimuli)
  } else if(length(stimuli) != length(order)){
    stop("Stimuli and order vectors have different length")
  }

  medial_part <- if(!is.null(translations)){
    paste0(stimuli[order], "_", translations[order])
  } else{
    stimuli[order]
  }

  if(autonumber){
    prefix <- paste0(add_leading_symbols(medial_part), "_", prefix)
  }

  if(logging){
    logging_path <- ifelse(backup,
                          paste0(path, "/backup/logging.csv"),
                          paste0(path, "/logging.csv"))
    logging_df <- data.frame(from = files,
                             to = paste0(prefix,
                                         medial_part,
                                         suffix,
                                         ".", extension[1]))
    utils::write.csv(x = logging_df, file = logging_path, row.names = FALSE)
    message(paste0("You can find change correspondences in the following",
                   " file:\n",
                   logging_path))
  }

  result <- file.rename(
    paste0(path, "/", files),
    paste0(path, "/", prefix, medial_part, suffix, ".", extension[1]))
}
