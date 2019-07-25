#' Dataframe to TextGrid's tier
#'
#' Convert a dataframe to a Praat TextGrid.
#'
#' @author George Moroz <agricolamz@gmail.com>
#'
#' @param df an R dataframe object that contains columns named "annotation", "start" and "end" (if you want an interval tier)
#' @param textgrid character with a filename or path to the TextGrid
#' @param name vector that contain a name for a created tier
#'
#' @examples
#' # tier_to_df("my.TextGrid")
#'
#' @export
#'

df_to_tier <- function(df, textgrid, name = ""){
  if(!("start" %in% names(df))|!("annotation" %in% names(df))){
    stop('df columns should have the folowing names: "annotation", "start" and "end" (if you want an interval tier)')
  }

  tg <- readLines(textgrid)
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
        paste0(tier_type, "[", i, "]:"),
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
    paste0('        name = "', name, '" '),
    tg[grep("item", tg)[2]+3],
    tg[grep("item", tg)[2]+4],
    paste0(tier_type, ": ", nrow(df), " "),
    unlist(all_annotations)
  )
  writeLines(append(tg, add_tier), textgrid)
}
