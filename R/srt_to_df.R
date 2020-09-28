#' Subtitles .srt file to dataframe
#'
#' Convert subtitles .srt file to a dataframe.
#'
#' @author George Moroz <agricolamz@gmail.com>
#'
#' @param file_name string with a filename or path to the .srt file
#'
#' @return a dataframe with columns:  \code{id}, \code{content},
#' \code{time_start}, \code{time_end}, \code{source}.
#'
#' @examples
#' srt_to_df(system.file("extdata", "test.srt", package = "phonfieldwork"))
#' @export
#'
#' @importFrom uchardet detect_file_enc
#'

srt_to_df <- function(file_name) {

  # thanks to Artem Klevtsov for this code
  con <- file(file_name, encoding = uchardet::detect_file_enc(file_name))
  srt <- readLines(con)
  close(con)

  # after https://stackoverflow.com/a/36532461/6056442

  # convert to dataframe
  result <- lapply(split(seq_along(srt), cumsum(grepl("^\\s*$", srt))), function(i) {
    block <- srt[i]
    block <- block[!grepl("^\\s*$", block)]
    if (length(block) == 0) {
      return(NULL)
    }
    if (length(block) < 3) {
      warning(paste0(
        "There are some non-standard blocks in ",
        file_name,
        " file"
      ))
    }
    return(data.frame(
      id = block[1],
      times = block[2],
      content = paste0(block[3:length(block)], collapse = "\n")
    ))
  })

  result <- do.call(rbind, result)
  result <- cbind(result, do.call(rbind, strsplit(result[, "times"], " --> ")))
  result <- result[, -2]
  names(result)[3:4] <- c("time_start", "time_end")

  # convert time to seconds

  l <- lapply(c("time_start", "time_end"), function(i) {
    do.call(rbind, lapply(
      strsplit(result[, i], ":|,"),
      as.double
    )) %*% c(60 * 60, 60, 1, 1 / 1000)
  })

  result <- cbind(result, as.data.frame(l))
  result <- result[, -which(names(result) %in% c("time_start", "time_end"))]
  names(result)[3:4] <- c("time_start", "time_end")
  result$source <- basename(file_name)
  return(result)
}
