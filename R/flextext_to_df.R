#' FLEX's .flextext file to dataframe
#'
#' Convert .flextext file from FLEX to a dataframe.
#'
#' @author George Moroz <agricolamz@gmail.com>
#'
#' @param file_name string with a filename or path to the .flextext file
#' @return a dataframe with columns: \code{p_id}, \code{s_id}, \code{w_id},
#' \code{txt}, \code{cf}, \code{hn}, \code{gls},
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

flextext_to_df <- function(file_name) {
  l <- xml2::read_xml(file_name)
  l <- xml2::xml_find_all(l, "interlinear-text")
  text_df <- lapply(seq_along(l), function(i) {
    t <- xml2::xml_find_all(
      l[[i]],
      "paragraphs/paragraph/phrases/phrase/words/word"
    )
    if (length(t) == 0) {
      stop("It looks like this .flextext doesn't have any word-level annotations.")
    }
    result_df <- lapply(seq_along(t), function(j) {
      word <- xml2::xml_attr(t[[j]], "guid")
      p_at <- xml2::xml_attr(
        xml2::xml_children(xml2::xml_parents(t[[j]])[[2]]),
        "type"
      )

      free_trans <- ifelse("gls" %in% p_at,
        xml2::xml_text(
          xml2::xml_children(
            xml2::xml_parents(
              t[[j]]
            )[[2]]
          )[which(p_at %in% "gls")]
        ),
        ""
      )

      lit_trans <- ifelse("lit" %in% p_at,
        xml2::xml_text(
          xml2::xml_children(
            xml2::xml_parents(
              t[[j]]
            )[[2]]
          )[which(p_at %in% "lit")]
        ),
        ""
      )

      if (free_trans == "" & lit_trans == "") {
        free_trans <- ""
      } else if (free_trans != "" & lit_trans == "") {
        free_trans <- free_trans
      } else if (free_trans == "" & lit_trans != "") {
        free_trans <- lit_trans
      } else if (free_trans != "" & lit_trans != "") {
        free_trans <- paste0(free_trans, " (", lit_trans, ")")
      }

      title <- xml2::xml_text(xml2::xml_children(xml2::xml_parents(t[[j]])[[6]])[1])
      m <- xml2::xml_find_all(t[[j]], "morphemes/morph")
      if (length(m) == 0) {
        other <- unlist(xml2::xml_attrs(xml2::xml_parents(t[[j]]), "guid"))
        data.frame(
          txt = xml2::xml_text(t[[j]]),
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
          text = other[3]
        )
      } else {
        morphemes <- lapply(m, function(morpheme) {
          morph <- xml2::xml_attr(morpheme, "guid")
          other <- unlist(xml2::xml_attrs(xml2::xml_parents(morpheme), "guid"))
          values <- xml2::xml_text(xml2::xml_children(morpheme))
          attrs <- xml2::xml_attr(xml2::xml_children(morpheme), "type")
          data.frame(
            txt = ifelse("txt" %in% attrs,
              values[which(attrs == "txt")],
              NA
            ),
            cf = ifelse("cf" %in% attrs,
              values[which(attrs == "cf")],
              NA
            ),
            hn = ifelse("hn" %in% attrs,
              values[which(attrs == "hn")],
              NA
            ),
            gls = ifelse("gls" %in% attrs,
              values[which(attrs == "gls")],
              NA
            ),
            msa = ifelse("msa" %in% attrs,
              values[which(attrs == "msa")],
              NA
            ),
            free_trans = free_trans,
            text_title = title,
            morph = morph,
            word = other[1],
            phrase = other[2],
            paragraph = other[3],
            text = other[4]
          )
        })
        do.call(rbind, morphemes)
      }
    })
    df <- do.call(rbind, result_df)

    # change empty puntuation ids to number
    df$word[which(is.na(df$word))] <- seq_along(which(is.na(df$word)))


    index <- data.frame(
      p_id = as.numeric(factor(df$paragraph,
        levels = unique(df$paragraph)
      )),
      s_id = as.numeric(factor(df$phrase,
        levels = unique(df$phrase)
      )),
      w_id = as.numeric(factor(df$word,
        levels = unique(df$word)
      ))
    )
    cbind(index, df)
  })
  text_df <- do.call(rbind, text_df)
  rownames(text_df) <- seq_along(text_df$text)
  text_df <- as.data.frame(apply(
    text_df, 2,
    function(x) {
      ifelse(is.na(x), "", x)
    }
  ))
  text_df$s_id <- as.numeric(text_df$s_id)
  text_df$p_id <- as.numeric(text_df$p_id)
  text_df$w_id <- as.numeric(text_df$w_id)
  return(text_df)
}
