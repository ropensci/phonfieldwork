#' TextGrid to dataframe
#'
#' Convert Praat TextGrid to a dataframe.
#'
#' @author George Moroz <agricolamz@gmail.com>
#'
#' @param file_name string with a filename or path to the TextGrid
#' @param encoding TextGrid encoding. Import from \code{readLines()} function.
#' @param textgrids_from_folder path to a folder with multiple .TextGrid files.
#' If this argument is not \code{NULL}, then the function goes through all files
#' and create a merged dataframe for all of them.
#'
#' @return a dataframe with columns:  \code{id}, \code{time_start},
#' \code{time_end} (if it is an interval tier -- the same as the start value),
#' \code{content}, \code{tier} and \code{source}
#'
#' @examples
#' textgrid_to_df(system.file("extdata", "test.TextGrid",
#'                            package = "phonfieldwork"))
#'
#' @export

textgrid_to_df <- function(file_name,
                           encoding = "unknown",
                           textgrids_from_folder = NULL){
  if(is.null(textgrids_from_folder)){
    if(grepl("TextGrid", file_name[2])){
      tg <- file_name
    } else{
      tg <- readLines(file_name, encoding = encoding)
    }
    n_tiers <- as.double(regmatches(tg[7], regexpr("\\d{1,}", tg[7])))
    lapply(1:n_tiers, function(x){
      df <- phonfieldwork::tier_to_df(tg, x)
      df$tier <- x
      return(df)
    }) ->
      l
    result <- do.call(rbind, l)
    if(grepl("TextGrid", file_name[2])){
      source <- "custom_file"
    } else{
      source <- unlist(strsplit(normalizePath(file_name), "/"))
    }
    result$source <- source[length(source)]
    return(result[order(result$time_start),])
  } else {
    files <- paste0(normalizePath(textgrids_from_folder),
                    "/",
                    list.files(normalizePath(textgrids_from_folder),
                               "\\.TextGrid$"))
    return(do.call(rbind, lapply(files, function(i){
      textgrid_to_df(file_name = i, encoding = encoding)
    })))
  }
}
