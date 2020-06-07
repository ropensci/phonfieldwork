#' TextGrid to dataframe
#'
#' Convert Praat TextGrid to a dataframe.
#'
#' @author George Moroz <agricolamz@gmail.com>
#'
#' @param textgrid string with a filename or path to the TextGrid
#' @return a dataframe with columns:  \code{id}, \code{start}, \code{end} (if it is an interval tier -- the same as the start value), \code{annotation}, and \code{tier}
#'
#' @examples
#' textgrid_to_df(example_textgrid)
#'
#' @export

textgrid_to_df <- function(textgrid, tier = 1){
  if(grepl("TextGrid", textgrid[2])){
    tg <- textgrid
  } else{
    tg <- readLines(textgrid)
  }
  n_tiers <- as.double(regmatches(tg[7], regexpr("\\d", tg[7])))
  lapply(1:n_tiers, function(x){
    df <- phonfieldwork::tier_to_df(tg, x)
    df$tier <- x
    if(!("end" %in% names(df))){
      df <- data.frame(id = df$id,
                       start = df$start,
                       end = df$start,
                       annotation = df$annotation,
                       tier = df$tier)
    }
    return(df)
  }) ->
    l
  result <- Reduce(rbind, l)
  return(result[order(result$start),])
}
