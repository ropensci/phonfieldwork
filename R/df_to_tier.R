#' Dataframe to TextGrid's tier
#'
#' Convert a dataframe to a Praat TextGrid.
#'
#' @author George Moroz <agricolamz@gmail.com>
#'
#' @param df an R dataframe object that contains columns named "annotation", "start" and "end" (if you want an interval tier)
#' @param textgrid a character with a filename or path to the TextGrid
#' @param tier_name a vector that contain a name for a created tier
#' @param overwrite a logic argument, if \code{TRUE} overwrites the existing TextGrid file
#' @return If \code{overwrite} is \code{FALSE}, then the function returns a vector of strings with a TextGrid. If \code{overwrite} is \code{TRUE}, then no output.
#' @examples
#' my_df <- data.frame(id = 1:5,
#'           start = c(0.00000000,0.01246583,0.24781914,0.39552363,0.51157715),
#'           end = c(0.01246583,0.24781914,0.39552363,0.51157715,0.65267574),
#'           annotation = c("", "T", "E", "S", "T"))
#' df_to_tier(my_df, textgrid = example_textgrid, overwrite = FALSE)
#'
#' @export
#'

df_to_tier <- function(df, textgrid, tier_name = "", overwrite = TRUE){
  if(!("start" %in% names(df))|!("annotation" %in% names(df))){
    stop('df columns should have the folowing names: "annotation", "start" and "end" (if you want an interval tier)')
  }

  if(grepl("TextGrid", textgrid[2])){
    tg <- textgrid
  } else{
    tg <- readLines(textgrid)
  }

  n_tiers <- as.numeric(gsub("\\D", "", tg[7]))
  tg[7] <- paste0("size = ", n_tiers+1, " ")

  tier_class <- ifelse("end" %in% names(df),
                       '        class = "IntervalTier" ',
                       '        class = "TextTier" ')

  tier_type <- ifelse("end" %in% names(df),
                      paste0('        intervals'),
                      paste0('        points'))


  if ("end" %in% names(df)) {
    all_annotations <- lapply(1:nrow(df), function(i) {
      c(
        paste0(tier_type, " [", i, "]:"),
        paste0("            xmin = ", df$start[i]),
        paste0("            xmax = ", df$end[i]),
        paste0('            text = "', df$annotation[i], '" ')
      )
    })
  } else {
    all_annotations <- lapply(1:nrow(df), function(i) {
      c(
        paste0(tier_type, "[", i, "]:"),
        paste0("            number = ", df$start[i]),
        paste0('            mark = "', df$annotation[i], '" ')
      )
    })
  }

  add_tier <- c(
    paste0("    item [", n_tiers+1, "]:"),
    tier_class,
    paste0('        name = "', tier_name, '" '),
    tg[grep("item", tg)[2]+3],
    tg[grep("item", tg)[2]+4],
    paste0(tier_type, ": size = ", nrow(df), " "),
    unlist(all_annotations)
  )
  if(overwrite){
    writeLines(append(tg, add_tier), textgrid)
  } else {
    append(tg, add_tier)
    }
}
