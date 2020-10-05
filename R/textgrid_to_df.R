#' TextGrid to dataframe
#'
#' Convert Praat TextGrid to a dataframe.
#'
#' @author George Moroz <agricolamz@gmail.com>
#'
#' @param file_name string with a filename or path to the TextGrid
#'
#' @return a dataframe with columns:  \code{id}, \code{time_start},
#' \code{time_end} (if it is an interval tier -- the same as the start value),
#' \code{content}, \code{tier}, \code{tier_name} and \code{source}
#'
#' @examples
#' textgrid_to_df(system.file("extdata", "test.TextGrid",
#'   package = "phonfieldwork"
#' ))
#'
#' # this is and example of reading a short .TextGrid format
#' textgrid_to_df(system.file("extdata", "test_short.TextGrid",
#'   package = "phonfieldwork"
#' ))
#' @export
#'
#' @importFrom uchardet detect_file_enc
#'

textgrid_to_df <- function(file_name) {
  tg <- read_textgrid(file_name)

  if (sum(grepl("tiers\\? <exists>", tg)) > 0) {
    if (sum(grepl("item ?\\[\\d{1,}\\]:", tg)) < 1) {
      "It looks like there is no tiers in this .TextGrid"
    }
    split_text <- split(
      seq_along(tg),
      cumsum(grepl("item ?\\[\\d{1,}\\]:", tg))
    )[-1]
    correction <- 0
  } else if ("<exists>" %in% tg) {
    if (sum(grepl("IntervalTier|TextTier", tg)) < 1) {
      "It looks like there is no tiers in this .TextGrid"
    }
    split_text <- split(
      seq_along(tg),
      cumsum(grepl("IntervalTier|TextTier", tg))
    )[-1]
    correction <- 1
  }


  l <- lapply(
    split_text,
    function(i) {
      class <- unlist(strsplit(tg[i[2 - correction]], '"'))[2]
      step_by <- ifelse(class == "IntervalTier", 4, 3) - correction
      start_max <- ifelse(class == "IntervalTier", 9, 8) - correction * 2
      data.frame(
        id = 0,
        time_start = gsub("[^0-9.]", "", tg[i[seq(8 - correction * 2,
                                                  length(i),
                                                  by = step_by
        )]]),
        time_end = gsub("[^0-9.]", "", tg[i[seq(start_max, length(i),
                                                by = step_by
        )]]),
        content = tg[i[seq(start_max + 1, length(i), by = step_by)]],
        tier = 0,
        tier_name = unlist(strsplit(tg[i[3 - correction]], '"'))[2],
        stringsAsFactors = FALSE
      )
    }
  )

  result <- do.call(rbind, l)

  result$id <- as.numeric(gsub("\\d{1,}\\.", "", rownames(result)))
  result$tier <- as.numeric(gsub("\\.\\d{1,}", "", rownames(result)))

  result$content <- unlist(lapply(result$content, function(j) {
    unlist(strsplit(j, '"'))[2]
  }))
  result$time_start <- as.numeric(result$time_start)
  result$time_end <- as.numeric(result$time_end)
  result$tier <- as.numeric(result$tier)
  if (grepl("TextGrid", file_name[2])) {
    source <- "custom_file"
  } else {
    source <- basename(file_name)
  }
  result$source <- source

  rownames(result) <- NULL
  return(result[order(result$time_start), ])
}
