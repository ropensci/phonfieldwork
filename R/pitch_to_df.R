#' Praat Pitch tier to dataframe
#'
#' Convert a Praat Pitch tier to a dataframe.
#'
#' @author George Moroz <agricolamz@gmail.com>
#'
#' @param file_name string with a filename or path to the Pitch tier
#' @param candidates Praat Pitch tier contains multiple candidates for each
#' time slice, use the value \code{"all"} if you want to get them all
#'
#' @return a dataframe with columns:  \code{time_start}, \code{time_end},
#' \code{frequency} and, if \code{candidates} = \code{"all"},
#' \code{candidate_id} and \code{strength}
#'
#' @examples
#' pitch_to_df(system.file("extdata", "test.Pitch", package = "phonfieldwork"))
#' @export
#'
#' @importFrom readr guess_encoding
#'

pitch_to_df <- function(file_name,
                        candidates = "") {
  # read file ---------------------------------------------------------------
  if (grepl("Pitch", file_name[2])) {
    pitch <- file_name
  } else {
    # thanks to Artem Klevtsov for this code
    con <- file(file_name,
                encoding = readr::guess_encoding(file_name)$encoding)
    pitch <- readLines(con)
    close(con)
  }

  # get metadata ------------------------------------------------------------
  time_start <- as.numeric(gsub("[^0-9\\.]", "", pitch[grep("xmin =", pitch)]))
  time_end <- as.numeric(gsub("[^0-9\\.]", "", pitch[grep("xmax =", pitch)]))

  # get all frames ----------------------------------------------------------
  s <- split(seq_along(pitch), cumsum(grepl("frames? \\[\\d{1,}\\]", pitch)))
  s <- s[-1]
  result <- lapply(seq_along(s), function(i) {
    data.frame(
      candidate_id = 1:sum(grepl("frequency =", pitch[s[[i]]])),
      frequency = grep("frequency =", pitch[s[[i]]], value = TRUE),
      strength = grep("strength =", pitch[s[[i]]], value = TRUE)
    )
  })

  result <- do.call(rbind, result)
  result$frequency <- as.numeric(gsub("[^0-9\\.]", "", result$frequency))
  result$strength <- as.numeric(gsub("[^0-9\\.]", "", result$strength))

  # define frames with maximum strengh --------------------------------------
  if (candidates != "all") {
    result <- data.frame(frequency = result[
      result$candidate_id == 1,
      "frequency"
    ])
  }
  result$frequency <- ifelse(result$frequency == 0, NA, result$frequency)
  result$time_start <- seq(time_start, time_end, length.out = nrow(result))
  result$time_end <- result$time_start
  return(result)
}
