#' EXMARaLDA's .exb file to dataframe
#'
#' Convert .exb file from EXMARaLDA to a dataframe.
#'
#' @author George Moroz <agricolamz@gmail.com>
#'
#' @param exb string with a filename or path to the .exb file
#' @return a dataframe with columns:  \code{tier}, \code{id}, \code{content}, \code{tier_name}, \code{tier_type}, \code{tier_category}, \code{tier_speaker}, \code{time_start}, \code{time_end}.
#'
#' @examples
#' exb_to_df(system.file("extdata", "test.exb", package = "phonfieldwork"))
#'
#' @export
#' @importFrom xml2 read_xml
#' @importFrom xml2 xml_find_all
#' @importFrom xml2 xml_attr
#' @importFrom xml2 xml_text
#' @importFrom xml2 xml_children

exb_to_df <- function(exb){
  # read file
  l <- xml2::read_xml(exb)
  # extract tiers
  t <- xml2::xml_find_all(l, "basic-body/tier")
  # tier names
  tier_names <- xml2::xml_attr(t, "display-name")
  # tier types
  tier_types <- xml2::xml_attr(t, "type")
  # tier category
  tier_categories <- xml2::xml_attr(t, "category")
  # tier category
  tier_speakers <- xml2::xml_attr(t, "speaker")

  # create list of dataframes
  lapply(seq_along(t), function(i){
    content <- xml2::xml_text(xml2::xml_find_all(t[[i]], "event"))
    ts1 <- xml2::xml_attr(xml2::xml_children(t[[i]]), "start")
    ts2 <- xml2::xml_attr(xml2::xml_children(t[[i]]), "end")
    data.frame(tier = i,
               id = 1:length(content),
               content = content,
               tier_name = tier_names[i],
               tier_type = tier_types[i],
               tier_category = tier_categories[i],
               tier_speaker = tier_speakers[i],
               ts_start = ts1,
               ts_end = ts2,
               stringsAsFactors = FALSE)
  }) ->
    r
  # merge list of dataframes  into dataframe
  r <- Reduce(rbind, r)

  # extract info about time
  tli <- xml2::xml_find_all(l,"basic-body/common-timeline/tli")

  ts <- data.frame(
    ts_id = xml2::xml_attr(tli, "id"),
    time_value = xml2::xml_attr(tli, "time"),
    stringsAsFactors = FALSE)

  # merge with time stamp df ts_start column
  r <- merge(r, ts, by.x = "ts_start", by.y = "ts_id")
  names(r)[names(r) == "time_value"] <- "time_start"
  # merge with time stamp df ts_end column
  r <- merge(r, ts, by.x = "ts_end", by.y = "ts_id")
  names(r)[names(r) == "time_value"] <- "time_end"

  # make sorting and remove some columns
  r <- r[order(r$tier, r$id), -c(1:2)]
  return(r)
}
