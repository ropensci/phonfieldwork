#' Subtitles .srt file to dataframe
#'
#' Convert subtitles .srt file to a dataframe.
#'
#' @author George Moroz <agricolamz@gmail.com>
#'
#' @param file_name string with a filename or path to the .srt file
#' @param encoding .srt encoding. Import from \code{readLines()} function.
#'
#' @return a dataframe with columns:  \code{id}, \code{content}, \code{time_start}, \code{time_end}.
#'
#' @examples
#' srt_to_df(system.file("extdata", "test.srt", package = "phonfieldwork"))
#'
#' @export

srt_to_df <- function(file_name, encoding = "unknown"){

  # after https://stackoverflow.com/a/36532461/6056442

  srt <- readLines(file_name, encoding = encoding, warn = FALSE)

  # convert to dataframe
  s <- split(1:length(srt), cumsum(grepl("^\\s*$",srt)))
  lapply(split(1:length(srt), cumsum(grepl("^\\s*$",srt))), function(i){
    block <- srt[i]
    block <- block[!grepl("^\\s*$", block)]
    if(length(block) == 0){
      return(NULL)
    }
    if(length(block) < 3){
      warning(paste0("There are some non-standard blocks in ",
                     file_name,
                     " file"))
    }
    return(data.frame(id = block[1],
                      times = block[2],
                      content = paste0(block[3:length(block)], collapse ="\n")))
  }) ->
    result

  result <- do.call(rbind,result)
  result <- cbind(result, do.call(rbind, strsplit(result[,'times'],' --> ')))
  result <- result[,-2]

  # convert time to seconds

  lapply(3:4, function(i){
    do.call(rbind, lapply(strsplit(result[,i], ':|,'),
                          as.numeric)) %*% c(60*60,60,1,1/1000)
  }) ->
    l

  result <- cbind(result, as.data.frame(l))
  result <- result[, -c(3:4)]
  names(result)[3:4] <- c("time_start", "time_end")
  return(result)
}
