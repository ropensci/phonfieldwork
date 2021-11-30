#' frog
#'
#' Returns a string of 'kva'
#'
#' @author Valeria Buntiakova <valleriabun@gmail.com>
#'
#' @param n length of string
#'
#' @return a string of 'kva'
#'
#' @examples
#' frog(3)
#' 
#' @export
#'

frog <- function(length){
  paste(rep('kva', length), collapse='-')
}


