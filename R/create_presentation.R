#' Creates a presentation
#'
#' Creates an html or powerpoint presentation in a working directory from list
#' of words and translations.
#' \href{https://ropensci.github.io/phonfieldwork/additional/first_example.html}{Here}
#' is an example of such presentation.
#'
#' @author George Moroz <agricolamz@gmail.com>
#'
#' @param stimuli the vector of stimuli (obligatory). Can be a path to an image.
#' @param translations the vector of translations (optional)
#' @param external the vector with the indices of external images
#' @param font_size font size in px (50, by default)
#' @param output_format the string that difine the R Markdown output format:
#' "html" (by default) or "pptx"
#' @param output_file the name of the result presentation file
#' (by default stimuli_presentation)
#' @param output_dir the output directory for the rendered file
#' @param render the logical argument, if \code{TRUE} render the created R
#' Markdown presentation to the \code{output_dir} folder, otherwise returns the
#' path to the temporary file with a Rmd file.
#' @return If \code{render} is \code{FALSE}, the function returns a path to the
#' temporary file. If \code{render} is \code{TRUE}, there is no output in a
#' function.
#' @examples
#' create_presentation(
#'   stimuli = c("rzeka", "drzewo"),
#'   translations = c("river", "tree"),
#'   render = FALSE
#' )
#'
#' # with image
#' create_presentation(
#'   stimuli = c(
#'     "rzeka", "drzewo",
#'     system.file("extdata", "r-logo.png",
#'       package = "phonfieldwork"
#'     )
#'   ),
#'   translations = c("river", "tree", ""),
#'   external = 3,
#'   render = FALSE
#' )
#' @export
#' @importFrom rmarkdown render
#'

create_presentation <- function(stimuli,
                                translations = "",
                                external = NULL,
                                font_size = 50,
                                output_dir,
                                output_format = "html",
                                output_file = "stimuli_presentation",
                                render = TRUE) {
  output_format <- ifelse(output_format == "pptx",
    "  powerpoint_presentation",
    "  ioslides_presentation:\n    transition: faster"
  )

  l <- rep("internal", length(stimuli))
  l[external] <- "external"
  stimuli <- as.character(stimuli)
  stimuli[l == "external"] <- normalizePath(stimuli[l == "external"])

  rmd <- paste0(paste0(
    "---\ntitle: 'Use arrows for scrolling'\noutput:\n",
    output_format,
    "\n---\n\n"
  ),
  paste0('##\n<div class="container">\n',
    ifelse(l == "internal", "**", "![]("),
    stimuli,
    ifelse(l == "internal", "**", ")"),
    "\n\n",
    translations,
    "\n</div>\n\n",
    collapse = ""
  ),
  paste0(
    "\n<style>\n",
    ".container {
    position: absolute;
    top: 50%;
    left: 50%;\n",
    "    font-size: ",
    font_size,
    "px;
    -moz-transform: translateX(-50%) translateY(-50%);
    -webkit-transform: translateX(-50%) translateY(-50%);
    transform: translateX(-50%) translateY(-50%);
}
</style>
"
  ),
  collapse = ""
  )
  tmp <- tempfile(pattern = output_file, fileext = ".Rmd")
  writeLines(rmd, tmp)
  if (isTRUE(render)) {
    rmarkdown::render(
      input = tmp,
      output_file = output_file,
      output_dir = normalizePath(output_dir),
      quiet = TRUE
    )
    suppress_messages <- file.remove(tmp)
  } else {
    tmp
  }
}
