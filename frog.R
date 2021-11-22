# frog
#
# Returns a string of 'kva'
#
# @param length of string
#
# @return a string of 'kva'
#

frog <- function(n){
  kvas = rep('kva', n)
  return(paste(kvas, collapse='-'))
}