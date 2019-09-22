#' Create indices padded with zeros
#'
#' Create indices padded with zeros. This is important for creating appropriate for sorting names.
#'
#' @author George Moroz <agricolamz@gmail.com>
#'
#' @param file_names vector of any values.
#'
#' @return A string with numbers padded with leadinng zero.
#'
#' @examples
#' add_leading_symbols(1:200)
#'
#' @export

add_leading_symbols <- function(file_names) {
  n_digits <- nchar(length(file_names))
  sprintf(paste0("%0", n_digits, "d"), 1:length(file_names))
}
