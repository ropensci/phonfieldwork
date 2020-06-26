#' TextGrid to dataframe
#'
#' Convert Praat TextGrid to a dataframe.
#'
#' @author George Moroz <agricolamz@gmail.com>
#'
#' @param textgrid string with a filename or path to the TextGrid
#' @param encoding TextGrid encoding. Import from \code{readLines()} function.
#'
#' @return a dataframe with columns:  \code{id}, \code{time_start}, \code{time_end} (if it is an interval tier -- the same as the start value), \code{content}, and \code{tier}
#'
#' @examples
#' textgrid_to_df(example_textgrid)
#'
#' @export

textgrid_to_df <- function(textgrid, encoding = "unknown"){
  if(grepl("TextGrid", textgrid[2])){
    tg <- textgrid
  } else{
    tg <- readLines(textgrid, encoding = encoding)
  }
  n_tiers <- as.double(regmatches(tg[7], regexpr("\\d", tg[7])))
  lapply(1:n_tiers, function(x){
    df <- phonfieldwork::tier_to_df(tg, x)
    df$tier <- x
    return(df)
  }) ->
    l
  result <- do.call(rbind, l)
  return(result[order(result$time_start),])
}
