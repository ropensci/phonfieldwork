% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_viewer.R
\name{create_viewer}
\alias{create_viewer}
\title{Create an annotation viewer}
\usage{
create_viewer(
  audio_dir,
  picture_dir = NULL,
  table,
  captions = NULL,
  sorting_columns = NULL,
  about = "Created with the `phonfieldworks` package (Moroz 2020).",
  map = FALSE,
  output_dir,
  output_file = "stimuli_viewer",
  render = TRUE
)
}
\arguments{
\item{audio_dir}{path to the directory with sounds}

\item{picture_dir}{path to the directory with pictures}

\item{table}{data frame with data ordered according to files in the audio folder}

\item{captions}{vector of strings that will be used for captions for a picture.}

\item{sorting_columns}{vector of strings for sorting the result column}

\item{about}{it is either .Rmd file or string with the text for about information: author, project, place of gahtered information and other metadata, version of the viewer and so on}

\item{map}{the logical argument, if \code{TRUE} and there is a \code{glottocode} column in \code{table}}

\item{output_dir}{the output directory for the rendered file}

\item{output_file}{the name of the result .html file (by default stimuli_viewer)}

\item{render}{the logical argument, if \code{TRUE} renders the created R Markdown viewer to the \code{output_dir} folder, otherwise returns the path to the temporary file with a .csv file.}
}
\value{
If \code{render} is \code{FALSE}, the function returns a path to the temporary file with .csv file. If \code{render} is \code{TRUE}, there is no output in a function.
}
\description{
Creates an html file with table and sound preview and player
}
\author{
George Moroz <agricolamz@gmail.com>
}
