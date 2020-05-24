#' ELAN's .eaf file to dataframe
#'
#' Convert .eaf file from ELAN to a dataframe.
#'
#' @author George Moroz <agricolamz@gmail.com>
#'
#' @param eaf string with a filename or path to the TextGrid
#' @return a dataframe with columns:  \code{tier}, \code{id}, \code{content}, \code{tier_name}, \code{tier_type}, \code{ts_start}, \code{ts_end}, \code{a_id}, \code{ar}).
#'
#' @examples
#' # eaf_to_df(example_eaf)
#'
#' @export
#' @importFrom xml2 read_xml
#' @importFrom xml2 xml_find_all
#' @importFrom xml2 xml_attr
#' @importFrom xml2 xml_text
#' @importFrom xml2 xml_children

eaf_to_df <- function(eaf){
  # read file
  l <- xml2::read_xml(eaf)
  # extract tiers
  t <- xml2::xml_find_all(l, "TIER")
  # extract tiers
  tier_names <- xml2::xml_attr(t, "TIER_ID")
  # tier types
  tier_types <- xml2::xml_attr(t, "LINGUISTIC_TYPE_REF")

  # create list of dataframes
  lapply(seq_along(t), function(i){
    content <- xml2::xml_text(xml2::xml_find_all(t[[i]],
                                     "ANNOTATION/*/ANNOTATION_VALUE"))
    ts1 <- xml2::xml_attr(xml2::xml_children(xml2::xml_children(t[[i]])),
                    "TIME_SLOT_REF1")
    ts2 <- xml2::xml_attr(xml2::xml_children(xml2::xml_children(t[[i]])),
                    "TIME_SLOT_REF2")
    a_id <- xml2::xml_attr(xml2::xml_children(xml2::xml_children(t[[i]])),
                     "ANNOTATION_ID")
    ar <- xml2::xml_attr(xml2::xml_children(xml2::xml_children(t[[i]])),
                   "ANNOTATION_REF")
    data.frame(tier = i,
               id = 1:length(content),
               content = content,
               tier_name = tier_names[i],
               tier_type = tier_types[i],
               ts_start = ts1,
               ts_end = ts2,
               a_id = a_id,
               ar = ar,
               stringsAsFactors = FALSE)
  }) ->
    r
  # merge list of dataframes  into dataframe
  r <- Reduce(rbind, r)

  # extract info about time
  ts <- data.frame(ts_id = xml_attr(xml_find_all(l, "TIME_ORDER/TIME_SLOT"),
                                    "TIME_SLOT_ID"),
                   time_value = as.numeric(xml_attr(xml_find_all(l, "TIME_ORDER/TIME_SLOT"),
                                                    "TIME_VALUE")),
                   stringsAsFactors = FALSE)
  return(r)
}
