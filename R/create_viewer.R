#' Create an annotation viewer
#'
#' Creates an html file with table and sound preview and player
#'
#' @author George Moroz <agricolamz@gmail.com>
#'
#' @param audio_dir path to the directory with sounds
#' @param picture_dir path to the directory with pictures
#' @param textgrid string with a filename or path to the TextGrid
#' @param tiers vecors of numbers or names of TextGrid tiers. They merged into a table and used in the created viewer.
#' @param merge_column string with a column name that will be used for merging stimuli and video and audio viewer.
#' @param caption_column string with a column name that will be used for captions for a picture (by defaul is the same as merge_column).
#' @param output_file the name of the result .html file (by default stimuli_viewer)
#' @param output_dir the output directory for the rendered file
#' @param render the logical argument, if \code{TRUE} renders the created R Markdown viewer to the \code{output_dir} folder, otherwise returns the path to the temporary file with a .csv file.
#' @param about it is either .Rmd file or string with the text for about
#'
#' @return If \code{render} is \code{FALSE}, the function returns a path to the temporary file with .csv file. If \code{render} is \code{TRUE}, there is no output in a function.
#'
#' @export
#' @importFrom rmarkdown render
#' @importFrom utils installed.packages
#' @importFrom utils write.csv


create_viewer <- function(audio_dir,
                          picture_dir,
                          textgrid,
                          tiers = 1,
                          merge_column,
                          caption_column = NULL,
                          about = "This page were created with the `phonfieldworks` package (Moroz 2019) for R (R Core Team 2020).",
                          output_dir,
                          output_file = "stimuli_viewer",
                          render = TRUE){
  if(is.null(caption_column)){caption_column =  merge_column}
  if(!("DT" %in% utils::installed.packages()[,"Package"])){
    stop('For this function you need to install DT package with a command install.packages("DT").')
  }
  audio <- list.files(normalizePath(audio_dir))
  pictures <- list.files(normalizePath(picture_dir))
  if(length(audio) > length(pictures)){
    stop("The number of audio files is greater then number of pictures.")
  }
  if(length(audio) < length(pictures)){
    stop("The number of audio files is less then number of pictures.")
  }

  result_df <- lapply(seq_along(tiers), function(i){
    df <- tier_to_df(textgrid = textgrid, tier = tiers[i])
    return(df$annotation[df$annotation != ""])
  })
  result_df <- as.data.frame(result_df)


# check whether there is an empty names -----------------------------------
  tier_names <- get_textgrid_names(textgrid)[tiers]
  tier_names <- lapply(seq_along(tier_names), function(i){
    ifelse(tier_names[i] == "", paste0("X", i), tier_names[i])
  })
  tier_names <- unlist(tier_names)

# make a coumn names ------------------------------------------------------
  colnames(result_df) <- tier_names

# create correct relative paths -------------------------------------------
  audio_dir <- strsplit(normalizePath(audio_dir),
                        normalizePath(output_dir))[[1]][2]
  audio_dir <- substr(audio_dir, 2, nchar(audio_dir))
  result_df$audio <- paste0(audio_dir, "/", audio)

  picture_dir <- strsplit(normalizePath(picture_dir),
                          normalizePath(output_dir))[[1]][2]
  picture_dir <- substr(picture_dir, 2, nchar(picture_dir))
  result_df$pictures <- paste0(picture_dir, "/", pictures)

# create a .csv file ------------------------------------------------------
  tmp1 <- tempfile(fileext = ".csv")
  utils::write.csv(result_df, tmp1, row.names = FALSE)

# create about file -------------------------------------------------------
  if(substr(about, nchar(about)-3, nchar(about)) == ".Rmd"){
    tmp2 <- about
  } else {
    tmp2 <- tempfile(fileext = ".Rmd")
    writeLines(about, tmp2)
  }

# render .Rmd -------------------------------------------------------------
  if(render == TRUE){
  rmarkdown::render(paste0(.libPaths()[1],
"/phonfieldwork/rmarkdown/templates/annotation_viewer/skeleton/skeleton.Rmd"),
                    params = list(data = tmp1,
                                  about = tmp2,
                                  merge_column = merge_column,
                                  caption_column = caption_column),
                    output_dir = output_dir,
                    quiet = TRUE,
                    output_file = output_file)
  message(paste0("Output created: ", output_dir, output_file, ".html"))
  } else {
    return(tmp1)
  }
}
