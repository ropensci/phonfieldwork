#' create crocodile's roar
#'
#' @description Creates a crocodile's roar for a given number of times.
#'
#' @author Kudrjashov Sergej <xenomirant@gmail.com>
#'
#' @param  iterations integer numeric value
#'
#' @return a string with cute crocodile's roar
#'
#' @examples 
#' crocodile(3)
#' 
#' @export
#'
 
crocodile <- function(iterations) {
  
roar <- rep('arrr', iterations)

roar <- paste(roar, collapse = '-')

roar

}
