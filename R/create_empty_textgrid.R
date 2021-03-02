#' Create an empty TextGrid
#'
#' Creates an empty Praat TextGrid in the same folder as a reference sound file. It is possible to manage with predefined number of tiers, their names and their types.
#'
#' @author George Moroz <agricolamz@gmail.com>
#'
#' @param duration integer. Duration of the textgrid. If you do not know the duration of your audio file use the \code{get_sound_duration()} function.
#' @param tier_name a vector that contain tier names.
#' @param point_tier a vector that defines which tiers should be made point tiers. This argument excepts numeric values (e. g. \code{c(2, 4)} means second and forth tiers) or character (e. g. \code{c("a", "b")} means tiers with names "a" and "b")
#' @param path path to the directory with soundfiles.
#' @param result_file_name name of the result and annotation files.
#'
#' @return The function returns no output, just creates a Praat TextGrid in the same folder as a reference sound file.
#' @examples
#' tmp <- tempfile(fileext = ".TextGrid")
#' create_empty_textgrid(1, path = dirname(tmp), result_file_name = basename(tmp))
#'
#' @export
#'

create_empty_textgrid <- function(duration,
                                  tier_name = NULL,
                                  point_tier = NULL,
                                  path,
                                  result_file_name = "new_textgrid") {

  # in case of empty tier_name and point tier_name ----------------------------------

  if (is.null(tier_name) & is.null(point_tier)) {
    tier_name <- "1"
  }

  if (is.null(tier_name) & !is.null(point_tier)) {
    tier_name <- as.character(seq_along(point_tier))
  }

  if(!grepl("\\.TextGrid$", result_file_name)){
    result_file_name <- paste0(result_file_name, ".TextGrid")
  }


  # get path to the file ----------------------------------------------------
  textgrid_path <- paste0(
    normalizePath(path),
    "/",
    result_file_name
  )

  # get info about which tier_name are point tier_name ------------------------------
  if (typeof(point_tier) == "character") {
    point_tier_m <- which(tier_name %in% point_tier)
    if (length(point_tier_m) == 0) {
      message(
        "There is no tier_name '",
        paste0(point_tier, collapse = "', '"),
        "' in the tier_name argument, so there is nothing to make a point tier."
      )
    }
  } else {
    point_tier_g <- which(point_tier > length(tier_name))
    if (length(point_tier_g) > 0) {
      message(
        "There are only ",
        length(tier_name),
        " tier_name in the tier_name argument, so tier '",
        paste0(point_tier[point_tier_g], collapse = "', '"),
        "' is out of bound."
      )
    }
    point_tier_m <- point_tier[-point_tier_g]
  }


  # create a textgrid_dataframe ---------------------------------------------
  textgrid_df <- data.frame(
    item = paste0(
      "    item [",
      seq_along(tier_name),
      "]:\n"
    ),
    class_line = '        class = "IntervalTier"\n',
    name_line = paste0('        name = "', tier_name, '"\n'),
    xmin = "        xmin = 0\n",
    xmax = paste0("        xmax = ", duration, "\n"),
    interval_size = "        intervals: size = 1\n",
    intervals = paste0("        intervals [1]:\n"),
    intervals_xmin = "            xmin = 0\n",
    intervals_xmax = paste0(
      "            xmax = ",
      duration, "\n"
    ),
    intervals_text = '            text = ""\n'
  )
  textgrid_df$class_line[point_tier_m] <- '        class = "TextTier"\n'
  textgrid_df$interval_size[point_tier_m] <- "        points: size = 0\n"
  textgrid_df$intervals[point_tier_m] <- ""
  textgrid_df$intervals_xmin[point_tier_m] <- ""
  textgrid_df$intervals_xmax[point_tier_m] <- ""
  textgrid_df$intervals_text[point_tier_m] <- ""


  # merge everything together -----------------------------------------------
  text <- paste0(
    'File type = "ooTextFile"\nObject class = "TextGrid"\n\nxmin = 0 \n',
    "xmax = ",
    duration,
    " \ntiers? <exists> \n",
    "size = ",
    length(tier_name),
    " \nitem []:\n",
    paste0(textgrid_df$item,
      textgrid_df$class_line,
      textgrid_df$name_line,
      textgrid_df$xmin,
      textgrid_df$xmax,
      textgrid_df$interval_size,
      textgrid_df$intervals,
      textgrid_df$intervals_xmin,
      textgrid_df$intervals_xmax,
      textgrid_df$intervals_text,
      collapse = ""
    )
  )
  writeLines(text, textgrid_path)
}
