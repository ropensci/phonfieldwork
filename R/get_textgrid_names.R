#' Extract TextGrid names
#'
#' Extract TextGrid names.
#'
#' @author George Moroz <agricolamz@gmail.com>
#'
#' @param textgrid path to the TextGrid
#' @param encoding TextGrid encoding. Import from \code{readLines()} function.
#'
#' @return return a vector of tier names from given TextGrid
#' @examples
#' get_textgrid_names(system.file("extdata", "test.TextGrid", package = "phonfieldwork"))
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
