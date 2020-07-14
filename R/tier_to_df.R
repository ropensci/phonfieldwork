#' TextGrid's tier to dataframe
#'
#' Convert selected tier from a Praat TextGrid to a dataframe.
#'
#' @author George Moroz <agricolamz@gmail.com>
#'
#' @param file_name string with a filename or path to the TextGrid
#' @param tier value that could be either ordinal number of the tier either
#' name of the tier. By default is '1'.
#' @param encoding TextGrid encoding. Import from \code{readLines()} function.
#'
#' @return a dataframe with columns:  \code{id}, \code{time_start},
#' \code{time_end}, \code{content}
#'
#' @examples
#' tier_to_df(system.file("extdata", "test.TextGrid",
#'                        package = "phonfieldwork"))
#'
#' @export
#'

tier_to_df <- function(file_name, tier = 1, encoding = "unknown"){
  if(grepl("TextGrid", file_name[2])){
    tg <- file_name
  } else{
    tg <- readLines(file_name, encoding = encoding)
  }
  # get start and end info about tiers --------------------------------------
  starts <- grep("item ?\\[\\d{1,}\\]:", tg)
  ends <- c(starts[-1]-1, length(tg))

  # extract tier by number --------------------------------------------------
  if(is.numeric(tier)){
    if(tier > length(starts)){
      stop(paste0("It looks like there is no tier number '", tier, "'"))
    }
    tier_number <- tier
  } else {

    # extract tier by name ----------------------------------------------------
    names <- sub('"\\s*',
                 "",
                 sub('\\s*name = "',
                     "",
                     tg[grep('name = ".*"', tg)]))
    if(!(tier %in% names)){
      stop(paste0("It looks like there is no any tier with a name '",tier,"'"))
    }
    tier_number <- which(names %in% tier)
  }
  w_tier <- tg[starts[tier_number]:ends[tier_number]]
  # number of annotations
  length <- as.numeric(gsub("\\D","", w_tier[6]))


# for interval tiers ------------------------------------------------------
  if(grepl("IntervalTier", w_tier[2])){
    results <- data.frame(id = 1:length,
                          time_start = w_tier[grep("intervals \\[", w_tier)+1],
                          time_end = w_tier[grep("intervals \\[", w_tier)+2],
                          content = w_tier[grep("intervals \\[", w_tier)+3],
                          stringsAsFactors = FALSE)
    results$time_end <- as.numeric(gsub("[^0-9\\.]", "", results$time_end))

# for point tiers ---------------------------------------------------------
  } else {
    results <- data.frame(id = 1:length,
                          time_start = w_tier[grep("points \\[", w_tier)+1],
                          time_end = w_tier[grep("points \\[", w_tier)+1],
                          content = w_tier[grep("points \\[", w_tier)+2],
                          stringsAsFactors = FALSE)
    results$time_end <- as.numeric(gsub("[^0-9\\.]", "", results$time_end))
  }

  results$time_start <- as.numeric(gsub("[^0-9\\.]", "", results$time_start))
  results$content <- sub('.* = "', "", results$content)
  results$content <- sub('".?$', "", results$content)
  return(results)
# it won't work if somebody will have string 'intervals [' or 'points ['
# in the annotation. I will fix it in the future...
}
