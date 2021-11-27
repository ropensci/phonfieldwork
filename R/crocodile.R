#' create crocodile's roar
#'
#' @description Creates a crocodile's roar for a given number of times.
#'
#' @author Kudrjashov Sergej 
#'
#' @param  iterations integer numeric value
#'
#' @return a string with terrifying crocodile's roar
#'
#' @example 
#' #' basic usage of crocodile
#' crocodile(iterations = 3)
#' 
#' 
crocodile <- function(iterations) {
  
roar <- rep('arrr', iterations)

roar <- paste(roar, collapse = '-')

return(roar)

}
