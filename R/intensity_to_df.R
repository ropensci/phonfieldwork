#' Praat Intensity tier to dataframe
#'
#' Convert a Praat Intensity tier to a dataframe.
#'
#' @author George Moroz <agricolamz@gmail.com>
#'
#' @param file_name string with a filename or path to the Intensity tier
#'
#' @return a dataframe with columns:  \code{time_start}, \code{time_end},
#' \code{Intensity}
#'
#' @examples
#' intensity_to_df(system.file("extdata", "test.Intensity", package = "phonfieldwork"))
#' @export
#'
#' @importFrom readr guess_encoding
#'

intensity_to_df <- function(file_name) {

  # read file ---------------------------------------------------------------
  if (grepl("Intensity", file_name[2])) {
    intensity <- file_name
  } else {
    # thanks to Artem Klevtsov for this code
    con <- file(file_name,
                encoding = readr::guess_encoding(file_name)$encoding)
    intensity <- readLines(con)
    close(con)
  }

  # get metadata ------------------------------------------------------------
  start <- as.numeric(strsplit(intensity[grep(
    "xmin = ",
    intensity
  )], "=")[[1]][2])
  end <- as.numeric(strsplit(intensity[grep(
    "xmax = ",
    intensity
  )], "=")[[1]][2])
  values <- intensity[grep("z \\[1\\] \\[\\d*\\] =", intensity)]
  values <- lapply(strsplit(values, "="), function(i) {
    i[[2]]
  })
  values <- as.numeric(unlist(values))

  # merge into df -----------------------------------------------------------
  time <- seq(start, end, length.out = length(values))
  result <- data.frame(
    time_start = time,
    time_end = time,
    intensity = values
  )
  return(result)
}
