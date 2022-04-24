#' ELAN's .eaf file to dataframe
#'
#' Convert .eaf file from ELAN to a dataframe.
#'
#' @author George Moroz <agricolamz@gmail.com>
#' @author Kudrjashov Sergej <xenomirant@gmail.com>
#'
#' @param file_name string with a filename or path to the .eaf file
#' @return a dataframe with columns:  \code{tier}, \code{id}, \code{content},
#' \code{tier_name}, \code{tier_type}, \code{tier_ref}, \code{dependent_on}, 
#' \code{time_start}, \code{time_end}, \code{source},
#' and attributes: \code{MEDIA_URL}, \code{MIME_TYPE}, \code{RELATIVE_MEDIA_URL}.
#'
#' @examples
#' eaf_to_df(system.file("extdata", "test.eaf", package = "phonfieldwork"))
#' @export
#' @importFrom xml2 read_xml
#' @importFrom xml2 xml_find_all
#' @importFrom xml2 xml_attr
#' @importFrom xml2 xml_text
#' @importFrom xml2 xml_children
#' 

eaf_to_df <- function(file_name) {
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
    

    loc <- list()
    r_sub[, 12] <- unlist(lapply(loc_res, function(i){
      loc <- append(loc, i[1:length(i)-1])
    }))
    
    r_sub[, 13] <- unlist(lapply(loc_res, function(i){
      loc <- append(loc, i[2:length(i)])
    }))

    r[row.names(r) %in% row.names(r_sub), ] <- r_sub
    
    #fix issues with Symbolic Association stereotype
    r_sub <- r[which(r$stereotype == 'Symbolic_Association'), ]
    
    r_sub[, 12] <- unlist(lapply(row.names(r_sub), function(i) {
      r[r$a_id == r_sub[i, ]$ar, ]$time_start
    }))
    
    r_sub[, 13] <- unlist(lapply(row.names(r_sub), function(i) {
      r[r$a_id == r_sub[i, ]$ar, ]$time_end
    }))
    
    r[row.names(r) %in% row.names(r_sub), ] <- r_sub
    
    #fix issues with Time Subdivision stereotype
    
    r_sub <- r[which(r$stereotype == 'Time_Subdivision'), ]
    r_sub <- r_sub[order(r_sub$tier_name, decreasing = FALSE), ]
    if (sum(is.na(r_sub$time_start)) + sum(is.na(r_sub$time_end)) != 0) {
      
      r_loc_sub <- r_sub[which(is.na(r_sub$time_start) | is.na(r_sub$time_end)), ]
      na_end <- r_loc_sub[which(!is.na(r_loc_sub$time_end)), ]
      na_start <- r_loc_sub[which(!is.na(r_loc_sub$time_start)), ]
      loc_res <- lapply(seq(length.out = nrow(na_start)), function(i) {
        
        id_start <- na_start[i, 5]
        id_end <- na_end[i, 5]
        time_start <- na_start[i, 12]
        time_end <- na_end[i, 13]
        round(seq(from = time_start, to = time_end, length.out = id_end-id_start+2), digits = 3)
        
      })
      
      loc <- list()
      r_loc_sub[, 12] <- unlist(lapply(loc_res, function(i){
        loc <- append(loc, i[1:length(i)-1])
      }))
      
      r_loc_sub[, 13] <- unlist(lapply(loc_res, function(i){
        loc <- append(loc, i[2:length(i)])
      }))
      
      r_sub[row.names(r_sub) %in% row.names(r_loc_sub), ] <- r_loc_sub
      
      r[row.names(r) %in% row.names(r_sub), ] <- r_sub
      
      } 
    
    # make sorting and remove some columns
    r <- r[order(r$time_start, r$tier), -c(1:2)]
    names(r)[names(r) == 'ar'] <- 'dependent_on'
    names(r)[names(r) == 'a_id'] <- 'tier_local_id'
    r <- r[, c(2,3,4,5,6,7,8,1,9,10,11)]
    
    #connected file
    attr(r, 'MEDIA_URL') <- xml2::xml_attr(xml2::xml_children(xml2::xml_find_all(l, 'HEADER'))[1], 'MEDIA_URL')
    attr(r, 'MIME_TYPE') <- xml2::xml_attr(xml2::xml_children(xml2::xml_find_all(l, 'HEADER'))[1], 'MIME_TYPE')
    attr(r, 'RELATIVE_MEDIA_URL') <- xml2::xml_attr(xml2::xml_children(xml2::xml_find_all(l, 'HEADER'))[1], 'RELATIVE_MEDIA_URL')
    
    r$source <- basename(file_name)
    r$media_url <- attr(r, 'MEDIA_URL')
    return(r)
  }
}

