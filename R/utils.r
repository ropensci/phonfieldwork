#' Reads textgrid with all encoding
#'
#' @author George Moroz <agricolamz@gmail.com>
#' @param file_name string with a filename or path to the TextGrid
#' @noRd
#' @importFrom readr guess_encoding
#'

read_textgrid <- function(file_name) {
  if (grepl("TextGrid", file_name[2])) {
    tg <- file_name
  } else {
    # thanks to Artem Klevtsov for this code
    con <- file(file_name,
                encoding = readr::guess_encoding(file_name)$encoding[1])
    tg <- readLines(con)
    close(con)
  }
  return(tg)
}

#' Create indices padded with zeros
#'
#' Create indices padded with zeros. This is important for creating appropriate
#' for sorting names.
#'
#' @author George Moroz <agricolamz@gmail.com>
#' @param file_names vector of any values.
#' @return A string with numbers padded with leadinng zero.

add_leading_symbols <- function(file_names) {
  n_digits <- nchar(length(file_names))
  sprintf(paste0("%0", n_digits, "d"), seq_along(file_names))
}

#' Create audio play objects for html viewer
#'
#' @author George Moroz <agricolamz@gmail.com>
#'
#' @param snd_src string or vector of strings with a image(s) path(s).
#' @param text string o vector of strings that will be displayed as view link.
#' By default it is ear emoji (&#x1f442;).
#' @return a string or vector of strings
#'

create_sound_play <- function(snd_src,
                              text = "&#x1f442;") {
  paste0(
    "<a ",
    "onmouseover=\"resize(this, '200%')\" ",
    "onmouseout=\"resize(this, '100%')\" ",
    "onclick = 'sound_play(\"",
    snd_src,
    "\")'> ",
    text,
    "<a>"
  )
}

#' Create image look_up objects for html viewer
#'
#' @author George Moroz <agricolamz@gmail.com>
#'
#' @param img_src string or vector of strings with a image(s) path(s).
#' @param img_caption string or vector of strings that will be displayed when
#' image is clicked.
#' @param text string o vector of strings that will be displayed as view link.
#' By default it is eye emoji (&#x1f441;).
#' @return a string or vector of strings
#'

create_image_look_up <- function(img_src,
                                 img_caption = NULL,
                                 text = "&#x1f441;") {
  if (is.null(img_caption)) {
    img_caption <- rep("", length(img_src))
  }

  if (length(img_src) != length(img_caption)) {
    stop(paste0(
      "It looks like the img_src variable contains ",
      length(img_src),
      " objects and the img_caption variable contains ",
      length(img_caption)
    ))
  }

  paste0(
    "<a ",
    "onmouseover=\"resize(this, '200%')\" ",
    "onmouseout=\"resize(this, '100%')\" ",
    "onclick = 'pic_appear",
    "(\"",
    img_src,
    "\", \"",
    img_caption,
    "\")'> ",
    text,
    "<a>"
  )
}
