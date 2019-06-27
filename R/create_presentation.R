#' Create a presentation
#'
#' Create a html or powerpoint presentation from list of words and translations.
#'
#' @author George Moroz <agricolamz@gmail.com>
#'
#' @param stimuli vector of stimuli (obligatory)
#' @param translations vector of translations (optional)
#' @param output string that difine the output type: "html" (by default) or "pptx"
#' @param filename name of the result presentation file
#'
#' @examples
#' ## Not run:
#' create_presentation("rzeka", "river")
#'
#' @export
#' @importFrom rmarkdown render
#'

create_presentation <- function(stimuli,
                                translations = "",
                                output = "html",
                                filename = "stimuli_presentation") {
  output <- ifelse(output == "pptx",
                   "powerpoint_presentation",
                   "ioslides_presentation")
  rmd <- paste0(c(paste0("---\noutput: ",
                       output,
                       "\n---\n\n"),
                       collapse = ""),
                 paste0("## ",
                       stimuli,
                       "\n\n",
                       translations,
                       "\n\n"),
               collapse = "")
  writeLines(rmd, paste0(filename, ".Rmd"))
  rmarkdown::render(paste0(filename, ".Rmd"))
  file.remove(paste0(filename, ".Rmd"))
}
