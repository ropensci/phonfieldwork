#' Create an annotation viewer
#'
#' Creates an html file with table and sound preview and player
#'
#' @author George Moroz <agricolamz@gmail.com>
#'
#' @param audio_dir path to the directory with sounds
#' @param picture_dir path to the directory with pictures
#' @param table data frame with data ordered according to files in the audio folder
#' @param captions vector of strings that will be used for captions for a picture.
#' @param sorting_columns vector of strings for sorting the result column
#' @param output_file the name of the result .html file (by default stimuli_viewer)
#' @param output_dir the output directory for the rendered file
#' @param render the logical argument, if \code{TRUE} renders the created R Markdown viewer to the \code{output_dir} folder, otherwise returns the path to the temporary file with a .csv file.
#' @param about it is either .Rmd file or string with the text for about information: author, project, place of gahtered information and other metadata, version of the viewer and so on
#' @param map the logical argument, if \code{TRUE} and there is a \code{glottocode} column in \code{table}
#'
#' @return If \code{render} is \code{FALSE}, the function returns a path to the temporary file with .csv file. If \code{render} is \code{TRUE}, there is no output in a function.
#'
#' @export
#' @importFrom rmarkdown render
#' @importFrom utils installed.packages
#' @importFrom utils write.csv

create_viewer <- function(audio_dir,
                          picture_dir,
                          table,
                          captions = NULL,
                          sorting_columns = NULL,
                          about = "Created with the `phonfieldworks` package (Moroz 2020).",
                          map = FALSE,
                          output_dir,
                          output_file = "stimuli_viewer",
                          render = TRUE){
  if(!("DT" %in% utils::installed.packages()[,"Package"])){
    stop('For this function you need to install DT package with a command install.packages("DT").')
  }

  if(isTRUE(map)){
    if(!("lingtypology" %in% utils::installed.packages()[,"Package"])){
      stop('If you want to create a map in a viewer, you need to install lingtypology package with a command install.packages("lingtypology").')
    }

    if(!("glottocode" %in% names(table))){
      if(!("longitude" %in% names(table)) &
         !("latitude" %in% names(table))){
        stop('If you want to create a map in a viewer, you need to add a glottocode (or latitude and longitude) column to the datafarame in a table argument.')
      } else {
        table$glottocode <- "fake"
      }
    } else {
      if(!("longitude" %in% names(table)) &
         !("latitude" %in% names(table))){
        table$longitude <- NA
        table$latitude <- NA
      }
    }
  }

  message('Since the result .html file possibly containes some vulnerable data, researcher(s) bear the whole responsibility for the publishing of the result. Run vignette("ethical_research_with_phonfieldwork") for more details.')

  audio <- list.files(normalizePath(audio_dir))
  pictures <- list.files(normalizePath(picture_dir))
  if(length(audio) > length(pictures)){
    stop("The number of audio files is greater then number of pictures.")
  }
  if(length(audio) < length(pictures)){
    stop("The number of audio files is less then number of pictures.")
  }

# create correct relative paths -------------------------------------------
  audio_dir <- strsplit(normalizePath(audio_dir),
                        normalizePath(output_dir))[[1]][2]
  audio_dir <- substr(audio_dir, 2, nchar(audio_dir))
  table$audio <- paste0(audio_dir, "/", audio)

  picture_dir <- strsplit(normalizePath(picture_dir),
                          normalizePath(output_dir))[[1]][2]
  picture_dir <- substr(picture_dir, 2, nchar(picture_dir))
  table$pictures <- paste0(picture_dir, "/", pictures)

  # sort rows according to sorting_columns
  if(!is.null(sorting_columns)){
    table <- table[do.call(order, table[sorting_columns]), ]
  }

# create a .csv file ------------------------------------------------------
  tmp1 <- tempfile(fileext = ".csv")
  utils::write.csv(table, tmp1, row.names = FALSE)

# create about file -------------------------------------------------------
  if(substr(about, nchar(about)-3, nchar(about)) == ".Rmd"){
    tmp2 <- about
  } else {
    tmp2 <- tempfile(fileext = ".Rmd")
    writeLines(about, tmp2)
  }


# create map file ---------------------------------------------------------
  tmp3 <- tempfile(fileext = ".Rmd")
  if(isTRUE(map)){
    writeLines(
    '## map

```{r map, echo=FALSE, message=FALSE, warning=FALSE}
if("fake" %in% df$glottocode){
  df$language_for_lingtypology <- "fake"
} else {
  df$language_for_lingtypology <- lingtypology::lang.gltc(df$glottocode)
}

library(lingtypology)
map.feature(languages = df$language_for_lingtypology,
            longitude = df$longitude,
            latitude = df$latitude,
            popup = df$viewer)
```

<div class = "my_block" id="my_block" onclick = "pic_disappear()">
  <span class="close">&times;</span>
  <img class = "my_img" id="my_img">
  <div class = "caption" id="caption">
  </div>
</div>',
    tmp3)
  } else{
    writeLines('', tmp3)
  }

# render .Rmd -------------------------------------------------------------
  if(render == TRUE){
  rmarkdown::render(paste0(.libPaths()[1],
"/phonfieldwork/rmarkdown/templates/annotation_viewer/skeleton/skeleton.Rmd"),
                    params = list(data = tmp1,
                                  about = tmp2,
                                  map = tmp3,
                                  captions = captions),
                    output_dir = output_dir,
                    quiet = TRUE,
                    output_file = output_file)
  message(paste0("Output created: ", output_dir, output_file, ".html"))
  suppress_message <- file.remove(tmp1)
  suppress_message <- file.remove(tmp2)
  if(isTRUE(map)){
    suppress_message <- file.remove(tmp3)
  }
  } else {
    return(tmp1)
    suppress_message <- file.remove(tmp2)
    if(isTRUE(map)){
      suppress_message <- file.remove(tmp3)
    }
  }
}
