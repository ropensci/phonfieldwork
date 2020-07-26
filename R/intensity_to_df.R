#' Praat Intensity tier to dataframe
#'
#' Convert a Praat Intensity tier to a dataframe.
#'
#' @author George Moroz <agricolamz@gmail.com>
#'
#' @param file_name string with a filename or path to the Intensity tier
#' @param encoding Intensity tier encoding. Import from \code{readLines()} function.
#'
#' @return a dataframe with columns:  \code{time_start}, \code{time_end},
#' \code{Intensity}
#'
#' @examples
#' intensity_to_df(system.file("extdata", "test.Intensity", package = "phonfieldwork"))
#'
#' @export
#'

intensity_to_df <- function(file_name,
                            encoding = "unknown"){

# read file ---------------------------------------------------------------
  if(grepl("Intensity", file_name[2])){
    intensity <- file_name
  } else{
    intensity <- readLines(file_name, encoding = encoding)
  }

# get metadata ------------------------------------------------------------
  start <- as.numeric(strsplit(intensity[grep("xmin = ",
                                              intensity)], "=")[[1]][2])
  end <- as.numeric(strsplit(intensity[grep("xmax = ",
                                            intensity)], "=")[[1]][2])
  values <- intensity[grep("z \\[1\\] \\[\\d*\\] =", intensity)]
  lapply(strsplit(values, "="), function(i){
    i[[2]]
  }) -> values
  values <- as.numeric(unlist(values))

# merge into df -----------------------------------------------------------
  time <- seq(start, end, length.out = length(values))
  result <- data.frame(time_start = time,
                       time_end = time,
                       intensity = values)
  return(result)
}
