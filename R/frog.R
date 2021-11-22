# frog
#
# Returns a string of 'kva'
#
# @author Valeria Buntiakova <valleriabun@gmail.com>
#
# @param length of string
#
# @return a string of 'kva'
#
# @examples
# frog(3)
# 
# @export
#

frog <- function(n){
  kvas = rep('kva', n)
  return(paste(kvas, collapse='-'))
}
