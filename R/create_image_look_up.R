#' Create image look_up objects for html viewer
#'
#' @author George Moroz <agricolamz@gmail.com>
#'
#' @param img_src string or vector of strings with a image(s) path(s).
#' @param img_caption string or vector of strings that will be displayed when image is clicked.
#' @param text string o vector of strings that will be displayed as view link. By default it is eye emoji (&#x1f441;).
#' @return a string or vector of strings
#'
#' @examples
#' create_image_look_up("path/to/your/file")
#'
#' @export
#'

create_image_look_up <- function(img_src,
                                 img_caption = NULL,
                                 text = "&#x1f441;"){
  if(is.null(img_caption)){
    img_caption <- rep("", length(img_src))
  }


  if(length(img_src) != length(img_caption)){
    stop(paste0("It looks like the img_src variable contains ",
                length(img_src),
                " objects and the img_caption variable contains ",
                length(img_caption)))
  }

  paste0("<a ",
        "onmouseover=\"resize(this, '200%')\" ",
        "onmouseout=\"resize(this, '100%')\" ",
        "onclick = 'pic_appear(\"",
        img_src,
        "\", \"",
        img_caption,
        "\")'> ",
        text,
        "<a>")
}
