#' Reads textgrid with all encoding
#'
#' @param file_name string with a filename or path to the TextGrid
#' @noRd
#' @importFrom uchardet detect_file_enc
#'

read_textgrid <- function(file_name){
  if(grepl("TextGrid", file_name[2])){
    tg <- file_name
  } else{
    # thanks to Artem Klevtsov for this code
    con <- file(file_name, encoding = uchardet::detect_file_enc(file_name))
    tg <- readLines(con)
    close(con)
  }
  return(tg)
}
