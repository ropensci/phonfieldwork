#' ELAN's .eaf file to dataframe
#'
#' Convert .eaf file from ELAN to a dataframe.
#'
#' @author George Moroz <agricolamz@gmail.com>
#'
#' @param file_name string with a filename or path to the .eaf file
#' @return a dataframe with columns:  \code{tier}, \code{id}, \code{content},
#' \code{tier_name}, \code{tier_type}, \code{tier_ref}, \code{dependent_on}, 
#' \code{time_start}, \code{time_end}, \code{source}).
#'
#' @examples
#' eaf_to_df_mod(system.file("extdata", "test.eaf", package = "phonfieldwork"))
#' @export
#' @importFrom xml2 read_xml
#' @importFrom xml2 xml_find_all
#' @importFrom xml2 xml_attr
#' @importFrom xml2 xml_text
#' @importFrom xml2 xml_children
#' 

eaf_to_df_mod <- function(file_name) {
  # read file
  l <- xml2::read_xml(file_name)
  # extract tiers
  t <- xml2::xml_find_all(l, "TIER")
  # extract tiers
  tier_names <- xml2::xml_attr(t, "TIER_ID")
  # tier types
  tier_types <- xml2::xml_attr(t, "LINGUISTIC_TYPE_REF")
  #tier parent
  tier_parent <- xml2::xml_attr(t, "PARENT_REF")
  #tier_stereotype
  tier_stereotype <- data.frame(xml2::xml_attr(xml2::xml_find_all(l, "LINGUISTIC_TYPE"), 
                                    "LINGUISTIC_TYPE_ID"), xml2::xml_attr(xml2::xml_find_all(l, "LINGUISTIC_TYPE"), 
                                    "CONSTRAINTS"))
  colnames(tier_stereotype) <- c("tier_type", "stereotype")
  
  
  # create list of dataframes
  r <- lapply(seq_along(t), function(i) {
    content <- xml2::xml_text(xml2::xml_find_all(
      t[[i]],
      "ANNOTATION/*/ANNOTATION_VALUE"
    ))
    ts1 <- xml2::xml_attr(
      xml2::xml_children(xml2::xml_children(t[[i]])),
      "TIME_SLOT_REF1"
    )
    ts2 <- xml2::xml_attr(
      xml2::xml_children(xml2::xml_children(t[[i]])),
      "TIME_SLOT_REF2"
    )
    a_id <- xml2::xml_attr(
      xml2::xml_children(xml2::xml_children(t[[i]])),
      "ANNOTATION_ID"
    )
    ar <- xml2::xml_attr(
      xml2::xml_children(xml2::xml_children(t[[i]])),
      "ANNOTATION_REF"
    )
    if (length(content) > 0) {
      data.frame(
        tier = i,
        id = seq_along(content),
        content = content,
        tier_name = tier_names[i],
        tier_type = tier_types[i],
        tier_ref = tier_parent[i],
        ts_start = ts1,
        ts_end = ts2,
        a_id = a_id,
        ar = ar,
        stringsAsFactors = FALSE
      )
    }
  })
  
  # merge list of dataframes  into dataframe
  r <- do.call(rbind, r)
  r$id_  <- 1:nrow(r)
  r <- merge(r, tier_stereotype, by = 'tier_type')
  r <- r[order(r$id_), ]
  r <- subset(r, select = -c(id_))
  r <- subset(r, select = c(2, 3, 4, 5, 1, 11, 6, 7, 8, 9, 10))
  
  #connected file
  attr(r, 'MEDIA_URL') <- xml2::xml_attr(xml2::xml_children(xml2::xml_find_all(l, 'HEADER'))[1], 'MEDIA_URL')
  attr(r, 'MIME_TYPE') <- xml2::xml_attr(xml2::xml_children(xml2::xml_find_all(l, 'HEADER'))[1], 'MIME_TYPE')
  attr(r, 'RELATIVE_MEDIA_URL') <- xml2::xml_attr(xml2::xml_children(xml2::xml_find_all(l, 'HEADER'))[1], 'RELATIVE_MEDIA_URL')
  
  if (length(r) > 0) {
    # extract info about time
    ts <- data.frame(
      ts_id = xml2::xml_attr(
        xml2::xml_find_all(l, "TIME_ORDER/TIME_SLOT"), "TIME_SLOT_ID"
      ),
      time_value = as.numeric(xml2::xml_attr(
        xml2::xml_find_all(l, "TIME_ORDER/TIME_SLOT"), "TIME_VALUE"
      )) / 1000,
      stringsAsFactors = FALSE
    )
    
    
    # df with time markers
    tm <- r[is.na(r$ar), c("ts_start", "ts_end", "a_id")]
    # df without time markers
    wtm <- r[!is.na(r$ar), c("a_id", "ar")]
    
    # create df with all time stamp
    while (nrow(tm) < nrow(r)) {
      df <- unique(merge(
        x = wtm,
        y = tm,
        by.x = "ar",
        by.y = "a_id"
      )[, c("ts_start", "ts_end", "a_id")])
      tm <- unique(rbind(tm, df))
    }
    
    # result df with time stamps
    r <- merge(r[, -c(8:9)], tm)
    # merge with time stamp df ts_start column
    r <- merge(r, ts, by.x = "ts_start", by.y = "ts_id")
    names(r)[names(r) == "time_value"] <- "time_start"
    # merge with time stamp df ts_end column
    r <- merge(r, ts, by.x = "ts_end", by.y = "ts_id")
    names(r)[names(r) == "time_value"] <- "time_end"
    
    #fix issues with Symbolic Subdivision stereotype
    r_sub <- r[which(r$stereotype == 'Symbolic_Subdivision'), ]
    un_r_sub <- unique(r_sub$ar)
    loc_res <- lapply(un_r_sub, function(i){
      df_loc <- r_sub[r_sub$ar == i, ]
      time_start <- df_loc[1, ]$time_start
      time_end <- df_loc[1, ]$time_end
      round(seq(from = time_start, to = time_end, length.out = nrow(df_loc)+1), digits = 3)
      })
    
    c <- 1
    for (i in seq(length.out = length(loc_res))) {
      for (j in seq(length.out = length(loc_res[[i]])-1)) {
        r_sub[c, 12] <- loc_res[[i]][j]
        r_sub[c, 13] <- loc_res[[i]][j+1]
        c <- c+1
      }
    }

    lapply(row.names(r_sub), function(i) {
      r[i, ] <<- r_sub[i, ]
    })
    
    #fix issues with Symbolic Association stereotype
    r_sub <- r[which(r$stereotype == 'Symbolic_Association'), ]
    
    lapply(row.names(r_sub), function(i) {
      r_sub[i, 12] <<- r[r$a_id == r_sub[i, ]$ar, ]$time_start
      r_sub[i, 13] <<- r[r$a_id == r_sub[i, ]$ar, ]$time_end
    })
    
    lapply(row.names(r_sub), function(i) {
      r[i, ] <<- r_sub[i, ]
    })
    
    # make sorting and remove some columns
    r <- r[order(r$time_start, r$tier), -c(1:2)]
    names(r)[names(r) == 'ar'] <- 'dependent_on'
    names(r)[names(r) == 'a_id'] <- 'tier_local_id'
    r <- r[, c(2,3,4,5,6,7,8,1,9,10,11)]
    r$source <- basename(file_name)
    return(r)
  }
}

