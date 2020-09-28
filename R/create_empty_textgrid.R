#' Create an empty TextGrid
#'
#' Creates an empty Praat TextGrid in the same folder as a reference sound file. It is possible to manage with predefined number of tiers, their names and their types.
#'
#' @author George Moroz <agricolamz@gmail.com>
#'
#' @param file_name a reference sound file.
#' @param tiers a vector that contain tier names.
#' @param point_tiers a vector that defines which tiers should be made point tiers. This argument excepts numeric values (e. g. \code{c(2, 4)} means second and forth tiers) or character (e. g. \code{c("a", "b")} means tiers with names "a" and "b")
#'
#' @return The function returns no output, just creates a Praat TextGrid in the same folder as a reference sound file.
#'
#' @export
#'

create_empty_textgrid <- function(file_name,
                                  tiers = NULL,
                                  point_tiers = NULL) {

  # in case of empty tiers and point tiers ----------------------------------

  if(is.null(tiers) & is.null(point_tiers)){
    tiers <- "1"
  }

  if(is.null(tiers) & !is.null(point_tiers)){
    tiers <- as.character(seq_along(point_tiers))
  }


  # get path to the file ----------------------------------------------------
  file_name <- normalizePath(file_name)
  spl <- unlist(strsplit(basename(file_name), "\\."))
  name <- substring(
    basename(file_name),
    1,
    nchar(basename(file_name)) - nchar(spl[length(spl)]) - 1
  )
  textgrid_path <- paste0(dirname(file_name), "/", name, ".TextGrid")

  # get duration of the sound -----------------------------------------------
  dur <- get_sound_duration(file_name)

  # get info about which tiers are point tiers ------------------------------
  if (typeof(point_tiers) == "character") {
    point_tiers_m <- which(tiers %in% point_tiers)
    if (length(point_tiers_m) == 0) {
      message(
        "There is no tiers '",
        paste0(point_tiers, collapse = "', '"),
        "' in the tiers argument, so there is nothing to make a point tier."
      )
    }
  } else {
    point_tiers_g <- which(point_tiers > length(tiers))
    if (length(point_tiers_g) > 0) {
      message(
        "There are only ",
        length(tiers),
        " tiers in the tiers argument, so tier '",
        paste0(point_tiers[point_tiers_g], collapse = "', '"),
        "' is out of bound."
      )
    }
    point_tiers_m <- point_tiers[-point_tiers_g]
  }


  # create a textgrid_dataframe ---------------------------------------------
  textgrid_df <- data.frame(
    item = paste0(
      "    item [",
      seq_along(tiers),
      "]:\n"
    ),
    class_line = '        class = "IntervalTier"\n',
    name_line = paste0('        name = "', tiers, '"\n'),
    xmin = "        xmin = 0\n",
    xmax = paste0("        xmax = ", dur$duration, "\n"),
    interval_size = "        intervals: size = 1\n",
    intervals = paste0("        intervals [1]:\n"),
    intervals_xmin = "            xmin = 0\n",
    intervals_xmax = paste0(
      "            xmax = ",
      dur$duration, "\n"
    ),
    intervals_text = '            text = ""\n'
  )
  textgrid_df$class_line[point_tiers_m] <- '        class = "TextTier"\n'
  textgrid_df$interval_size[point_tiers_m] <- "        points: size = 0\n"
  textgrid_df$intervals[point_tiers_m] <- ""
  textgrid_df$intervals_xmin[point_tiers_m] <- ""
  textgrid_df$intervals_xmax[point_tiers_m] <- ""
  textgrid_df$intervals_text[point_tiers_m] <- ""


  # merge everything together -----------------------------------------------
  text <- paste0(
    'File type = "ooTextFile"\nObject class = "TextGrid"\n\nxmin = 0\n',
    "xmax = ",
    dur$duration,
    "\ntiers? <exists>\n",
    "size = ",
    length(tiers),
    "\nitem []:\n",
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
