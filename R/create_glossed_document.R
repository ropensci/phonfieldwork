#' Create a glossed document
#'
#' Creates a file with glossed example (export from .flextext or other formats)
#'
#' @author George Moroz <agricolamz@gmail.com>
#'
#' @param flextext path to a .flextext file.
#' @param rows vetcor of rows from the flextext should be appear in the final document. Possible values are: "cf", "hn", "gls", "msa". "gls" is default.
#' @param output_file the name of the result .html file (by default stimuli_viewer)
#' @param output_dir the output directory for the rendered file
#' @param output_format The option can be "html" or "docx"
#'
#' @return If \code{render} is \code{FALSE}, the function returns a path to the temporary file with .csv file. If \code{render} is \code{TRUE}, there is no output in a function.
#'
#' @export
#' @importFrom rmarkdown render
#' @importFrom utils installed.packages
#' @importFrom utils write.csv

create_glossed_document <- function(flextext = NULL,
                                    rows = c("gls"),
                                    output_dir,
                                    output_file = "glossed_document",
                                    output_format = "docx"){
  if(!("dplyr" %in% utils::installed.packages()[,"Package"])){
    stop('For this function you need to install dplyr package with a command install.packages("dplyr").')
  }
  if(!("tidyr" %in% utils::installed.packages()[,"Package"])){
    stop('For this function you need to install tidyr package with a command install.packages("tidyr").')
  }
  if(!(output_format %in% c("html", "docx"))){
    stop('The output_format can be only "html" or "docx"')
  }

# flextext input ----------------------------------------------------------
  # flextext = "/home/agricolamz/Desktop/zilo_test.flextext"
  if(!is.null(flextext)){
    tmp1 <- tempfile(fileext = ".csv")
    utils::write.csv(flextext_to_df(flextext), tmp1, row.names = FALSE)
    output_format2 <- ifelse(output_format == "docx", "word", output_format)
    rmarkdown::render(paste0(.libPaths()[1],
                             "/phonfieldwork/rmarkdown/templates/glossed_document/skeleton/skeleton.Rmd"),
                      params = list(data = tmp1, rows = rows),
                      output_dir = output_dir,
                      output_format = paste0(output_format2[1], "_document"),
                      quiet = TRUE,
                      output_file = output_file)
    message(paste0("Output created: ", output_dir, output_file, ".", output_format[1]))
# non-flextext input ------------------------------------------------------
  } else if(!is.null(word_level) &
            !is.null(gloss_level) &
            !is.null(translation_level)){
    print("will be done soon")
  } else {
    stop('You need to specify either flextext or word_level, gloss_level, and translation_level arguments.')
  }
}
