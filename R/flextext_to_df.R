#' FLEX's .flextext file to dataframe
#'
#' Convert .flextext file from FLEX to a dataframe.
#'
#' @author George Moroz <agricolamz@gmail.com>
#'
#' @param file_name string with a filename or path to the .flextext file
#' @return a dataframe with columns: \code{s_id} (that has structure
#' paragraph_id.phrase_id), \code{txt}, \code{cf}, \code{hn}, \code{gls},
#' \code{msa}, \code{morph}, \code{word}, \code{phrase}, \code{paragraph},
#' \code{free_trans}, \code{text}, \code{text_title}
#'
#' @export
#' @importFrom xml2 read_xml
#' @importFrom xml2 xml_find_all
#' @importFrom xml2 xml_attr
#' @importFrom xml2 xml_text
#' @importFrom xml2 xml_children
#' @importFrom xml2 xml_child

flextext_to_df <- function(file_name){
  l <- xml2::read_xml(file_name)
  l <- xml2::xml_find_all(l, 'interlinear-text')
  lapply(seq_along(l), function(i){
    t <- xml2::xml_find_all(l[[i]],
                            "paragraphs/paragraph/phrases/phrase/words/word")
    lapply(seq_along(t), function(j){
      word <- xml2::xml_attr(t[[j]], "guid")
free_trans <- xml2::xml_text(xml2::xml_child(xml2::xml_parents(t[[j]])[[2]], 3))
      if(length(xml2::xml_children(xml2::xml_parents(t[[j]])[[2]])) > 3){
lit_trans <- xml2::xml_text(xml2::xml_child(xml2::xml_parents(t[[j]])[[2]], 4))
        free_trans <- paste0(free_trans, "(", lit_trans, ")")
      }
title <- xml2::xml_text(xml2::xml_children(xml2::xml_parents(t[[j]])[[6]])[1])
      m <- xml2::xml_find_all(t[[j]], "morphemes/morph")
      if(length(m) == 0){
        other <- unlist(xml2::xml_attrs(xml2::xml_parents(t[[j]]), "guid"))
        data.frame(txt = xml2::xml_text(t[[j]]),
                   cf = NA,
                   hn = NA,
                   gls = NA,
                   msa = NA,
                   free_trans = free_trans,
                   text_title = title,
                   morph = NA,
                   word = word,
                   phrase = other[1],
                   paragraph = other[2],
                   text = other[3])


      } else {
        morphemes <- lapply(m, function(morpheme){
          morph <- xml2::xml_attr(morpheme, "guid")
          other <- unlist(xml2::xml_attrs(xml2::xml_parents(morpheme), "guid"))
          values <- xml2::xml_text(xml2::xml_children(morpheme))
          data.frame(txt = values[1],
                     cf = values[2],
                     hn = values[3],
                     gls = values[4],
                     msa = values[5],
                     free_trans = free_trans,
                     text_title = title,
                     morph = morph,
                     word = other[1],
                     phrase = other[2],
                     paragraph = other[3],
                     text = other[4])
        })
        do.call(rbind, morphemes)
      }
    }) ->
      result_df
    df <- do.call(rbind, result_df)
    index <- data.frame(s_id = as.numeric(factor(df$phrase,
                                                 levels = unique(df$phrase))),
                        p_id = as.numeric(factor(df$paragraph,
                                              levels = unique(df$paragraph))))
    cbind(index, df)
  }) ->
    text_df
  text_df <- do.call(rbind, text_df)
  rownames(text_df) <- seq_along(text_df$text)
  text_df <- as.data.frame(apply(text_df, 2,
                                 function(x){ifelse(is.na(x), "", x)}))
  text_df$s_id <- as.numeric(text_df$s_id)
  text_df$p_id <- as.numeric(text_df$p_id)
  return(text_df)
}
