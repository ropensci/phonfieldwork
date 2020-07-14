#' Rewrite TextGrid names
#'
#' Rewrite TextGrid names.
#'
#' @author George Moroz <agricolamz@gmail.com>
#'
#' @param textgrid path to the TextGrid
#' @param tiers integer vector with the number of tiers that should be named
#' @param names vector of strings with new names for TextGrid tiers
#' @param write logical. If TRUE (by dafault) it overwrites an existing tier
#' @param encoding TextGrid encoding. Import from \code{readLines()} function.
#'
#' @return a string that contain TextGrid. If argument write is \code{TRUE},
#' then no output.
#' @examples
#' set_textgrid_names(system.file("extdata", "test.TextGrid",
#'                                package = "phonfieldwork"),
#' tiers = 3, names = "new_name", write = FALSE)
#'
#' @export
#'

set_textgrid_names <- function(textgrid,
                               tiers,
                               names,
                               write = TRUE,
                               encoding = "unknown"){
  # read TextGrid -----------------------------------------------------------
  if(grepl("TextGrid", textgrid[2])){
    tg <- textgrid
  } else{
    tg <- readLines(normalizePath(textgrid), encoding = encoding)
  }

# rewrite names in TextGrid -----------------------------------------------
  lapply(grep('name = ".*"', tg)[tiers], function(i){
    tg[i] <<- paste0('        name = "',
                    names[i],
                    '"')
  }) -> result

# write the result TextGrid -----------------------------------------------
  if (isTRUE(write)) {
    writeLines(tg, normalizePath(textgrid))
  } else {
    return(tg)
  }
}
