#' Praat Formant object to dataframe
#'
#' Convert a Praat Formant object to a dataframe.
#'
#' @author George Moroz <agricolamz@gmail.com>
#'
#' @param file_name string with a filename or path to the Formant file
#' @param encoding Pitch tier encoding. Import from \code{readLines()} function.
#'
#' @return a dataframe with columns:  \code{time_start}, \code{time_end},
#' \code{frequency}, \code{bandwidth} and \code{formant}
#'
#' @examples
#' formant_to_df(system.file("extdata", "e.Formant", package = "phonfieldwork"))
#'
#' @export
#'

formant_to_df <- function(file_name,
                          encoding = "unknown"){
  # read file ---------------------------------------------------------------
  if(grepl("Formant", file_name[2])){
    formant <- file_name
  } else{
    formant <- readLines(file_name, encoding = encoding)
  }

  # get metadata ------------------------------------------------------------
  time_start <- as.numeric(gsub("[^0-9\\.]", "", formant[grep("xmin =", formant)]))
  time_end <- as.numeric(gsub("[^0-9\\.]", "", formant[grep("xmax =", formant)]))

  # get all frames ----------------------------------------------------------
  s <- split(seq_along(formant), cumsum(grepl("frames \\[\\d{1,}\\]", formant)))
  s <- s[-1]
  lapply(seq_along(s), function(i){
    data.frame(id = i,
               formant = 1:sum(grepl("frequency =", formant[s[[i]]])),
               frequency = grep("frequency =", formant[s[[i]]], value = TRUE),
               bandwidth = grep("bandwidth =", formant[s[[i]]], value = TRUE))
  }) -> result

  result <- do.call(rbind,result)
  result$frequency <- as.numeric(gsub("[^0-9\\.]", "", result$frequency))
  result$bandwidth <- as.numeric(gsub("[^0-9\\.]", "", result$bandwidth))

  df <- data.frame(id = seq_along(s),
                   time_start = seq(time_start, time_end,
                                    length.out = length(s)))
  result <- merge(result, df, by = "id")
  result$time_end <- result$time_start
  return(result[,-1])
}
