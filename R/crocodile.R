#' Crocodile's roar
#'
#' Creates a crocodile's roar for a given number of times.
#'
#' @author Kudrjashov Sergej 
#'
#' @param numeric value
#'
#' @return a string with crocodile's roar
#'
#' @examples crocodile(4)
#'

crocodile <- function(iterations) {
  
  if (iterations == 1) {
    return('arrr')
  }
  
  roar <- ''
  for (i in c(1:iterations)) {
    roar <- paste(roar, 'arrr', sep = '-')
  }
  
roar <- substring(roar, 2, nchar(roar))

return(roar)

}

