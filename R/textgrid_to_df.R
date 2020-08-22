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
#' \code{content}, \code{tier}, \code{tier_name} and \code{source}
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

    sum(grepl("item ?\\[\\d{1,}\\]:", tg)) < 1

    lapply(split(seq_along(tg),
                 cumsum(grepl("item ?\\[\\d{1,}\\]:", tg)))[-1],
           function(i){
             class <- unlist(strsplit(tg[i[2]], '"'))[2]
             step_by = ifelse(class == "IntervalTier", 4, 3)
             start_max = ifelse(class == "IntervalTier", 9, 8)
             data.frame(
               id = seq_along(seq(8, length(i), by = step_by)),
               time_start = gsub("[^0-9.]", "", tg[i[seq(8, length(i),
                                                         by = step_by)]]),
               time_end = gsub("[^0-9.]", "", tg[i[seq(start_max, length(i),
                                                       by = step_by)]]),
               content = tg[i[seq(start_max+1, length(i), by = step_by)]],
               tier = gsub("[^0-9]", "", tg[i[1]]),
               tier_name = unlist(strsplit(tg[i[3]], '"'))[2],
               stringsAsFactors = FALSE)
           }) ->
      l

    result <- do.call(rbind, l)


    result$content <- unlist(lapply(result$content, function(j){
      unlist(strsplit(j, '"'))[2]
    }))
    result$time_start <- as.numeric(result$time_start)
    result$time_end <- as.numeric(result$time_end)
    result$tier <- as.numeric(result$tier)
    if(grepl("TextGrid", file_name[2])){
      source <- "custom_file"
    } else{
      source <- unlist(strsplit(normalizePath(file_name), "/"))
    }
    result$source <- source[length(source)]

    rownames(result) <- NULL
    return(result[order(result$time_start),])
  } else {
    files <- paste0(normalizePath(textgrids_from_folder),
                    "/",
                    list.files(normalizePath(textgrids_from_folder),
                               "\\.TextGrid$"))
    return(do.call(rbind,
                   lapply(files, function(i){
                     textgrid_to_df(file_name = i, encoding = encoding)
    })))
  }
}
