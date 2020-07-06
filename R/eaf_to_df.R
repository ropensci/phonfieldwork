#' ELAN's .eaf file to dataframe
#'
#' Convert .eaf file from ELAN to a dataframe.
#'
#' @author George Moroz <agricolamz@gmail.com>
#'
#' @param file_name string with a filename or path to the .eaf file
#' @param eafs_from_folder path to a folder with multiple .eaf files. If this argument is not \code{NULL}, then the function goes through all files and create picture for all of them.
#' @return a dataframe with columns:  \code{tier}, \code{id}, \code{content}, \code{tier_name}, \code{tier_type}, \code{time_start}, \code{time_end}, \code{source}).
#'
#' @examples
#' eaf_to_df(system.file("extdata", "test.eaf", package = "phonfieldwork"))
#'
#' @export
#' @importFrom xml2 read_xml
#' @importFrom xml2 xml_find_all
#' @importFrom xml2 xml_attr
#' @importFrom xml2 xml_text
#' @importFrom xml2 xml_children

eaf_to_df <- function(file_name, eafs_from_folder = NULL){
  if(is.null(eafs_from_folder)){
    # read file
    l <- xml2::read_xml(file_name)
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
      if(length(content) > 0){
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
      }
    }) ->
      r
    # merge list of dataframes  into dataframe
    r <- do.call(rbind, r)

    if (length(r) > 0) {
      # extract info about time
      ts <- data.frame(
        ts_id = xml2::xml_attr(
          xml2::xml_find_all(l, "TIME_ORDER/TIME_SLOT"), "TIME_SLOT_ID"),
        time_value = as.numeric(xml2::xml_attr(
          xml2::xml_find_all(l, "TIME_ORDER/TIME_SLOT"), "TIME_VALUE"))/1000,
        stringsAsFactors = FALSE)


      # df with time markers
      tm <- r[is.na(r$ar), c("ts_start", "ts_end", "a_id")]
      # df without time markers
      wtm <- r[!is.na(r$ar), c("a_id", "ar")]

      # create df with all time stamp
      while(nrow(tm) < nrow(r)){
        df <- unique(merge(x = wtm,
                           y = tm,
                           by.x = "ar",
                           by.y = "a_id")[, c("ts_start", "ts_end", "a_id")])
        tm <- unique(rbind(tm, df))
      }

      # result df with time stamps
      r <- merge(r[,-c(6:7)], tm)
      # merge with time stamp df ts_start column
      r <- merge(r, ts, by.x = "ts_start", by.y = "ts_id")
      names(r)[names(r) == "time_value"] <- "time_start"
      # merge with time stamp df ts_end column
      r <- merge(r, ts, by.x = "ts_end", by.y = "ts_id")
      names(r)[names(r) == "time_value"] <- "time_end"

      # make sorting and remove some columns
      r <- r[order(r$time_start, r$tier), -c(1:3, 9)]
      source <- unlist(strsplit(normalizePath(file_name), "/"))
      r$source <- source[length(source)]
      return(r)
    }
  } else{
    files <- paste0(normalizePath(eafs_from_folder),
                    "/",
                    list.files(normalizePath(eafs_from_folder), ".eaf$"))
    return(do.call(rbind, lapply(files, eaf_to_df)))
  }
}
