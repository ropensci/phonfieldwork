#' Remove tier from texgrid
#'
#' @param textgrid character with a filename or path to the TextGrid
#' @param tier value that could be either ordinal number of the tier either name
#' of the tier
#' @param overwrite logical. If TRUE (by dafault) it overwrites an existing
#' tier.
#'
#' @return a string that contain TextGrid. If argument write is \code{TRUE},
#' then no output.
#'
#' @export
#'

remove_textgrid_tier <- function(textgrid,
                                 tier,
                                 overwrite = TRUE){
  df <- textgrid_to_df(textgrid)
  if(is.numeric(tier)){
    result <- df[!(df$tier %in% tier),]
  } else{
    result <- df[!(df$tier_name %in% tier),]
  }

  tmp <- tempdir()
  phonfieldwork::create_empty_textgrid(
    duration = max(df$time_end),
    path = tmp,
    result_file_name = basename(textgrid))

  new_tg <- readLines(paste0(tmp, "/", basename(textgrid)))
  new_tg[7] <- "size = 0 "
  writeLines(text = new_tg[1:8], paste0(tmp, "/", basename(textgrid)))

  lapply(seq_along(unique(result$tier)), function(i){
    phonfieldwork::df_to_tier(
      df = result[result$tier == unique(result$tier)[i],],
      textgrid = paste0(tmp, "/", basename(textgrid)),
      tier_name = unique(result$tier_name)[i],
      overwrite = TRUE)
  })
  if(isFALSE(overwrite)){
    readLines(paste0(tmp, "/", basename(textgrid)))
    paste0(tmp, "/", basename(textgrid))
  } else{
    file.copy(from = paste0(tmp, "/", basename(textgrid)),
              to = normalizePath(textgrid),
              overwrite = TRUE)
    unlink(tmp)
  }
}
