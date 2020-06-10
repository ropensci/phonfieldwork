#' Draw Oscilogram, Spectrogram and annotation
#' Create oscilogram and spectrogram plot. Based on \code{phonTools::spectrogram}.
#'
#' @author George Moroz <agricolamz@gmail.com>
#'
#' @param file_name a soundfile
#' @param textgrid a source for TextGrid annotation plot
#' @param spectrum_colors if TRUE, a color spectrogram will be displayed. If FALSE, greyscale is used. If a vector of colors is provided, these colors are used to create the spectrogram.
#' @param title the title for the plot
#' @param maximum_frequency the maximum frequency to be displayed for the spectrogram up to a maximum of fs/2. This is set to 5000 Hz by default
#' @param dynamic_range values greater than this many dB below the maximum will be displayed in the same color
#' @param window_length the desired analysis window length in milliseconds.
#' @param output_file the name of the output file
#' @param output_width the width of the device
#' @param output_height the height of the device
#' @param output_units the units in which height and width are given. Can be "px" (pixels, the default), "in" (inches), "cm" or "mm".
#' @param sounds_from_folder path to a folder with multiple .wav files. If this argument is not \code{NULL}, then the function goes through all files and create picture for all of them.
#' @param textgrids_from_folder path to a folder with multiple .TextGrid files. If this argument is not \code{NULL}, then the function goes through all files and create picture for all of them.
#' @param pic_folder_name name for a folder, where all pictures will be stored in case \code{sounds_from_folder} argument is not \code{NULL}
#' @param prefix prefix for all file names for created pictures in case \code{sounds_from_folder} argument is not \code{NULL}
#' @param suffix suffix for all file names for created pictures in case \code{sounds_from_folder} argument is not \code{NULL}
#' @param autonumber if TRUE automatically add number of extracted sound to the file_name. Prevents from creating a duplicated files and wrong sorting.
#'
#' @return Oscilogram and spectrogram plot (and possibly TextGrid annotation).
#'
#' @export
#'
#' @importFrom tuneR readWave
#' @importFrom grDevices png
#' @importFrom grDevices dev.off
#' @importFrom graphics par
#' @importFrom graphics axis
#' @importFrom graphics text
#' @importFrom graphics abline
#' @importFrom graphics segments
#'

draw_sound <- function(file_name,
                       textgrid = NULL,
                       output_file = NULL,
                       title = NULL,
                       spectrum_colors = FALSE,
                       maximum_frequency = 5000,
                       dynamic_range = 50,
                       window_length = 5,
                       output_width = 750,
                       output_height = 500,
                       output_units = "px",
                       sounds_from_folder = NULL,
                       textgrids_from_folder = NULL,
                       pic_folder_name = "pics",
                       prefix = NULL,
                       suffix = NULL,
                       autonumber = FALSE){
  if(is.null(sounds_from_folder)){
    if(is.null(output_file)){
      # read file and convert to phonTools format -------------------------------
      s <- tuneR::readWave(file_name)

      # if there is no title no need to have a space for it ---------------------
      title_space <- ifelse(is.null(title), 0, 2)

      # plot oscilogram ---------------------------------------------------------
      graphics::par(oma=c(0,0,title_space,0),
                    mai=c(0, 0, 0, 0),
                    fig=c(0.1,0.98,0.75,1))

      n <- max(abs(range(s@left)))
      s_range <- floor(n/10^(nchar(n)-1))*10^(nchar(n)-1)

      plot(y = s@left,
           x = seq(0, length(s@left)/s@samp.rate*1000,
                   length.out = length(s@left)),
           type = "l",
           xaxt = 'n',
           yaxt = 'n',
           ylab = "",
           xlab = "",
           xaxs = "i")
      title(as.character(title), outer = TRUE)
      # plot spectrogram --------------------------------------------------------
      low_boundary <- ifelse(!is.null(textgrid), 0.27, 0.08)
      graphics::par(fig=c(0.1,0.98,low_boundary,0.75), new=TRUE)
      draw_spectrogram(s@left,
                       fs = s@samp.rate,
                       windowlength = window_length,
                       colors = spectrum_colors,
                       maxfreq = maximum_frequency,
                       dynamicrange = dynamic_range,
                       x_axis = is.null(textgrid))
      # plot textgrid -----------------------------------------------------------
      if(!is.null(textgrid)){
        graphics::par(fig=c(0.1, 0.98, 0.07, 0.27), new=TRUE)
        df <- textgrid_to_df(textgrid)
        df$start <- df$start*1000
        df$end <- df$end*1000
        df$mid_point <- df$start + (df$end - df$start)/2
        df$fake_y <- max(df$tier) - min(df$tier)
        df$tier <- -df$tier
        plot(x = df$mid_point,
             y = df$fake_y,
             xlim = c(df$start[1], length(s@left)/s@samp.rate*1000),
             ylim = range(df$tier)+c(-0.4, 0.4),
             cex = 0,
             yaxt='n',
             xlab = "time(ms)",
             ylab = "",
             xaxs="i")
        graphics::text(x = df$mid_point, y = df$tier, labels = df$annotation, cex = 1)
        graphics::abline(h = unique(df$tier)[-length(unique(df$tier))]-0.5)
        graphics::segments(x0 = df$start, x1 = df$start, y0 = df$tier-0.5, y1 = df$tier+0.5)
        graphics::segments(x0 = df$end, x1 = df$end, y0 = df$tier-0.5, y1 = df$tier+0.5)
      }
      # reset graphical parameters to default -----------------------------------
      graphics::par(oma=c(0,0,0,0),
                    mai=c(1.02, 0.82, 0.82, 0.42),
                    fig=c(0,1,0,1))
    } else {
      # save a file -------------------------------------------------------------
      grDevices::png(filename = paste0(output_file, ".png"),
                     width = output_width,
                     height = output_height,
                     units = output_units)
      draw_sound(file_name,
                 textgrid,
                 title,
                 spectrum_colors,
                 maximum_frequency,
                 dynamic_range,
                 window_length,
                 output_file = NULL)
      grDevices::dev.off()
    }
    # in case of multuple files -----------------------------------------------
  } else {
    # get a correct picture folder path ---------------------------------------
    sounds_from_folder <- normalizePath(sounds_from_folder)
    list.files(sounds_from_folder)
    slashes <- unlist(gregexpr("/", sounds_from_folder))
    cut <- unlist(gregexpr("/", sounds_from_folder))[length(slashes)]
    pic_path <- paste0(substr(sounds_from_folder, 1, cut), pic_folder_name)

    # create a directory if it is not exists ----------------------------------
    if(!dir.exists(pic_path)){
      dir.create(pic_path)
    }

    # get list of sounds and future pictures ----------------------------------
    sounds <- paste0(sounds_from_folder, "/", list.files(sounds_from_folder))

    unlist(
      lapply(seq_along(sounds), function(i){
        res <- unlist(strsplit(sounds[i], "\\."))
        res[length(res)]
      })) ->
      extension
    files_from_list <- which(tolower(extension) %in% "wav")

    sounds <- sounds[files_from_list]

    if(isTRUE(autonumber)){
      prefix <- paste0(add_leading_symbols(1:length(sounds)), "_", prefix)
    }

    pics <- paste0(pic_path, "/",
                   prefix,
                   list.files(sounds_from_folder)[files_from_list])
    pics <- substr(pics, 1, nchar(pics)-4)

    # loop over the draw_sound function ---------------------------------------

    lapply(seq_along(sounds), function(i){
      draw_sound(sounds[i],
                 output_file = paste0(pics[i], suffix[i]),
                 title = list.files(sounds_from_folder)[files_from_list][i],
                 spectrum_colors = spectrum_colors,
                 maximum_frequency = maximum_frequency,
                 dynamic_range = dynamic_range,
                 window_length = window_length,
                 output_width = output_width,
                 output_height = output_height,
                 output_units = output_units)
    })
  }
}


