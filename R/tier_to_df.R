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
#' \code{time_end}, \code{content}, , \code{tier_name}
#'
#' @examples
#' tier_to_df(system.file("extdata", "test.TextGrid",
#'                        package = "phonfieldwork"))
#' tier_to_df(system.file("extdata", "test.TextGrid",
#'                        package = "phonfieldwork"),
#'                        "intervals")
#' @export
#'

tier_to_df <- function(file_name, tier = 1, encoding = "unknown"){
  df <- phonfieldwork::textgrid_to_df(file_name = file_name,
                                      encoding = encoding)

  if(is.numeric(tier)){
    if(tier > max(df$tier)){
      stop(paste0("It looks like there is no tier number '", tier, "'"))
    }
    results <- df[df$tier == tier,]
  } else if(is.character(tier)){
    if(!(tier %in% unique(df$tier_name))){
      stop(paste0("It looks like there is no any tier with a name '",tier,"'"))
    }
    results <- df[df$tier_name == tier,]
  }
  return(results)
}
