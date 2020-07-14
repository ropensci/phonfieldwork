#' Create boundaries in a texgrid tier
#'
#'
#' @author George Moroz <agricolamz@gmail.com>
#'
#' @param textgrid character with a filename or path to the TextGrid
#' @param tier value that could be either ordinal number of the tier either name
#' of the tier
#' @param new_tier_name a name of a new created tier
#' @param n_of_annotations number of new annotations per annotation to create
#' @param each non-negative integer. Each new blank annotation is repeated every
#'  first, second or ... times
#' @param omit_blank logical. If TRUE (by dafault) it doesn't create
#' subannotation for empy annotations.
#' @param overwrite logical. If TRUE (by dafault) it overwrites an existing
#' tier.
#' @param encoding TextGrid encoding. Import from \code{readLines()} function.
#'
#' @return a string that contain TextGrid. If argument write is \code{TRUE},
#' then no output.
#'
#' @examples
#' create_subannotation(system.file("extdata", "test.TextGrid",
#'                                  package = "phonfieldwork"),
#'                      tier = 1, overwrite = FALSE)
#'
#' @export
#'

create_subannotation <- function(textgrid,
                                 tier = 1,
                                 new_tier_name = "",
                                 n_of_annotations = 4,
                                 each = 1,
                                 omit_blank = TRUE,
                                 overwrite = TRUE,
                                 encoding = "unknown"){

# read TextGrid -----------------------------------------------------------
  if(grepl("TextGrid", textgrid[2])){
    tg <- textgrid
  } else{
    tg <- readLines(normalizePath(textgrid), encoding = encoding)
  }

  df <- phonfieldwork::tier_to_df(tg, tier = tier)

  if(omit_blank){
    df <- df[df$content != "",]
  }

  lapply(seq_along(df$content), function(i){
    t <- seq(df$time_start[i],
             df$time_end[i],
             length.out = each*(n_of_annotations+1))
    data.frame(time_start = t[-length(t)],
               time_end = t[-1])
  }) ->
    l

  final <- do.call(rbind, l)
  final <- cbind(id = seq_along(final$time_start), final, content = "")
  phonfieldwork::df_to_tier(final,
                            textgrid = textgrid,
                            tier_name = new_tier_name,
                            overwrite = overwrite)
}
