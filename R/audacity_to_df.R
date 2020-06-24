#' Audacity's labels to dataframe
#'
#' Audacity make it possible to annotate sound files with labels that can be exported as a .tsv file with .txt extension. This function convert result to dataframe.
#'
#' @author George Moroz <agricolamz@gmail.com>
#'
#' @param file_name file_name string with a filename or path to the .txt file produced by Audacity
#' @return a dataframe with columns:  \code{content}, \code{time_start}, \code{time_end}.
#'
#' @importFrom utils read.delim
#' @export

audacity_to_df <- function(file_name){
  df <- utils::read.delim(file_name)
  names(df) <- c("time_start", "time_end", "content")
  df$time_start <- as.numeric(gsub(",", ".", df$time_start))
  df$time_end <- as.numeric(gsub(",", ".", df$time_end))
  return(df)
}
