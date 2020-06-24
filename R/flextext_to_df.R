#' FLEX's .flextext file to dataframe
#'
#' Convert .flextext file from FLEX to a dataframe.
#'
#' @author George Moroz <agricolamz@gmail.com>
#'
#' @param file_name string with a filename or path to the .flextext file
#' @return a dataframe with columns: \code{s_id} (that has structure paragraph_id.phrase_id), \code{txt}, \code{cf}, \code{hn}, \code{gls}, \code{msa}, \code{morph}, \code{word}, \code{phrase}, \code{paragraph}, \code{free_trans}, \code{text}, \code{text_title}
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
    t <- xml2::xml_find_all(l[[i]], "paragraphs/paragraph/phrases/phrase/words/word/morphemes/morph")
    lapply(seq_along(t), function(j){
      morph <- xml2::xml_attr(t[[j]], "guid")
      other <- unlist(xml2::xml_attrs(xml2::xml_parents(t[[j]]), "guid"))
      values <- xml2::xml_text(xml2::xml_children(t[[j]]))
      title <- xml2::xml_text(xml2::xml_child(xml2::xml_parents(t[[j]])[[8]]))
      free_trans <- xml2::xml_text(xml2::xml_child(xml2::xml_parents(t[[j]])[[4]], 3))
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
    }) ->
      result_df
    df <- Reduce(rbind, result_df)
    index <- data.frame(s_id = as.numeric(factor(df$phrase,
                                                 levels = unique(df$phrase))),
                        p_id = as.numeric(factor(df$paragraph,
                                                 levels = unique(df$paragraph))))
    cbind(index, df)
  }) ->
    text_df
  text_df <- Reduce(rbind, text_df)
  rownames(text_df) <- seq_along(text_df$text)
  text_df <- as.data.frame(apply(text_df, 2, function(x){ifelse(is.na(x), "", x)}))
  text_df$s_id <- as.numeric(text_df$s_id)
  text_df$p_id <- as.numeric(text_df$p_id)
  return(text_df)
}
