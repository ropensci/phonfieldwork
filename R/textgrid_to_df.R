#' TextGrid to dataframe
#'
#' Convert Praat TextGrid to a dataframe.
#'
#' @author George Moroz <agricolamz@gmail.com>
#'
#' @param file_name string with a filename or path to the TextGrid
#' @param encoding TextGrid encoding. Import from \code{readLines()} function.
#'
#' @return a dataframe with columns:  \code{id}, \code{time_start}, \code{time_end} (if it is an interval tier -- the same as the start value), \code{content}, \code{tier} and \code{source}
#'
#' @examples
#' textgrid_to_df(system.file("extdata", "test.TextGrid", package = "phonfieldwork"))
#'
#' @export

textgrid_to_df <- function(file_name, encoding = "unknown"){
  if(grepl("TextGrid", file_name[2])){
    tg <- file_name
  } else{
    tg <- readLines(file_name, encoding = encoding)
  }
  n_tiers <- as.double(regmatches(tg[7], regexpr("\\d{1,}", tg[7])))
  lapply(1:n_tiers, function(x){
    df <- phonfieldwork::tier_to_df(tg, x)
    df$tier <- x
    return(df)
  }) ->
    l
  result <- do.call(rbind, l)
  source <- unlist(strsplit(normalizePath(file_name), "/"))
  result$source <- source[length(source)]
  return(result[order(result$time_start),])
}
