#' Extract TextGrid names
#'
#' Extract TextGrid names.
#'
#' @author George Moroz <agricolamz@gmail.com>
#'
#' @param textgrid path to the TextGrid
#'
#' @return return a vector of tier names from given TextGrid
#' @examples
#' get_textgrid_names(system.file("extdata", "test.TextGrid",
#'                                package = "phonfieldwork"))
#'
#' @export
#'

get_textgrid_names <- function(textgrid){
  unique(unlist(textgrid_to_df(textgrid)$tier_name))
}
