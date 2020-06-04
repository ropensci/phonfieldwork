#' FLEX's .flextext file to dataframe
#'
#' Convert .flextext file from FLEX to a dataframe.
#'
#' @author George Moroz <agricolamz@gmail.com>
#'
#' @param flextext string with a filename or path to the .flextext file
#' @return a dataframe with columns:
#'
#' @export
#' @importFrom xml2 read_xml
#' @importFrom xml2 xml_find_all
#' @importFrom xml2 xml_attr
#' @importFrom xml2 xml_text
#' @importFrom xml2 xml_children

flextext_to_df <- function(flextext){
  l <- xml2::read_xml(flextext)
}


