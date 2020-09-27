#' Create audio play objects for html viewer
#'
#' @author George Moroz <agricolamz@gmail.com>
#'
#' @param snd_src string or vector of strings with a image(s) path(s).
#' @param text string o vector of strings that will be displayed as view link.
#' By default it is ear emoji (&#x1f442;).
#' @return a string or vector of strings
#'
#' @examples
#' create_sound_play("path/to/your/file")
#' @export
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
