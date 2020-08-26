#' Dataframe to TextGrid's tier
#'
#' Convert a dataframe to a Praat TextGrid.
#'
#' @author George Moroz <agricolamz@gmail.com>
#'
#' @param df an R dataframe object that contains columns named "annotation",
#' "time_start" and "time_end"
#' @param textgrid a character with a filename or path to the TextGrid
#' @param tier_name a vector that contain a name for a created tier
#' @param overwrite a logic argument, if \code{TRUE} overwrites the existing
#' TextGrid file
#' @return If \code{overwrite} is \code{FALSE}, then the function returns a
#' vector of strings with a TextGrid. If \code{overwrite} is \code{TRUE}, then
#' no output.
#' @examples
#' time_start <-  c(0.00000000,0.01246583,0.24781914,0.39552363,0.51157715)
#' time_end <-  c(0.01246583,0.24781914,0.39552363,0.51157715,0.65267574)
#' content = c("", "T", "E", "S", "T")
#' df_to_tier(my_df <- data.frame(id = 1:5, time_start, time_end, content),
#'            system.file("extdata", "test.TextGrid",
#'                        package = "phonfieldwork"),
#'            overwrite = FALSE)
#'
#' @export
#'
#' @importFrom uchardet detect_file_enc
#'

df_to_tier <- function(df, textgrid, tier_name = "", overwrite = TRUE){
  if(!("time_start" %in% names(df))|
      !("time_end" %in% names(df))|
      !("content" %in% names(df))){
    stop(paste0('df columns should have the folowing names: "content"',
                '"time_start" and "time_end"'))
  }

  if(grepl("TextGrid", textgrid[2])){
    tg <- textgrid
  } else{
    # thanks to Artem Klevtsov for this code
    con <- file(textgrid, encoding = uchardet::detect_file_enc(textgrid))
    tg <- readLines(con)
    close(con)
  }

  n_tiers <- as.numeric(gsub("\\D", "", tg[7]))
  tg[7] <- paste0("size = ", n_tiers+1, " ")

  if(!(FALSE %in% (df$time_start == df$time_end))){
    df <- df[, -which(names(df) %in% "time_end")]
  }

  tier_class <- ifelse("time_end" %in% names(df),
                       '        class = "IntervalTier" ',
                       '        class = "TextTier" ')

  tier_type <- ifelse("time_end" %in% names(df),
                      paste0('        intervals'),
                      paste0('        points'))


  if ("time_end" %in% names(df)) {
    all_annotations <- lapply(seq_along(df$time_start), function(i) {
      c(
        paste0(tier_type, " [", i, "]:"),
        paste0("            xmin = ", df$time_start[i]),
        paste0("            xmax = ", df$time_end[i]),
        paste0('            text = "', df$content[i], '" ')
      )
    })
  } else {
    all_annotations <- lapply(seq_along(df$time_start), function(i) {
      c(
        paste0(tier_type, "[", i, "]:"),
        paste0("            number = ", df$time_start[i]),
        paste0('            mark = "', df$content[i], '" ')
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
