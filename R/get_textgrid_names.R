#' Extract TextGrid names
#'
#' Extract TextGrid names.
#'
#' @author George Moroz <agricolamz@gmail.com>
#'
#' @param textgrid path to the TextGrid
#' @return return a vector of tier names from given TextGrid
#' @examples
#' get_textgrid_names(example_textgrid)
#'
#' @export
#'

get_textgrid_names <- function(textgrid, encoding = "unknown"){
# read TextGrid -----------------------------------------------------------
  if(grepl("TextGrid", textgrid[2])){
    tg <- textgrid
  } else{
    tg <- readLines(normalizePath(textgrid), encoding = encoding)
  }

# extract names of tiers --------------------------------------------------
  lapply(grep('name = ".*"', tg), function(i){
    unlist(strsplit(tg[i], '"'))[2]
  }) ->
    names
  return(unlist(names))
}
