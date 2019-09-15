#' Draw Oscilogram and Spectrogram
#'
#' Create oscilogram and spectrogram plot. Based on \code{phonTools::spectrogram}.
#'
#' @author George Moroz <agricolamz@gmail.com>
#'
#' @param file_name a soundfile
#' @param spectrum_colors if TRUE, a color spectrogram will be displayed. If FALSE, greyscale is used. If a vector of colors is provided, these colors are used to create the spectrogram.
#' @param title the title for the plot
#' @param maximum_frequency the maximum frequency to be displayed for the spectrogram up to a maximum of fs/2. This is set to 5000 Hz by default
#' @param dynamic_range values greater than this many dB below the maximum will be displayed in the same color
#' @param window_length the desired analysis window length in milliseconds.
#' @param output_file the name of the output file
#' @param output_width the width of the device
#' @param output_height the height of the device
#' @param output_units the units in which height and width are given. Can be "px" (pixels, the default), "in" (inches), "cm" or "mm".
#' @return Oscilogram and Spectrogram plot.
#'
#' @export
#' @importFrom tuneR readWave
#' @importFrom tuneR plot
#' @importFrom phonTools makesound
#' @importFrom phonTools spectrogram
#' @importFrom grDevices png
#' @importFrom grDevices dev.off

draw_sound <- function(file_name,
                       output_file = NULL,
                       title = NULL,
                       spectrum_colors = FALSE,
                       maximum_frequency = 5000,
                       dynamic_range = 50,
                       window_length = 5,
                       output_width = 750,
                       output_height = 500,
                       output_units = "px"){
  if(is.null(output_file)){
# read file and convert to phonTools format -------------------------------
    s <- tuneR::readWave(file_name)
    sound <- phonTools::makesound(s@left, fs = s@samp.rate)

# if there is no title no need to have a space for it ---------------------
    title_space <- ifelse(is.null(title), 0, 2)

# set viewer parameters ---------------------------------------------------
    par(oma=c(0,0,title_space,0), mai=c(0, 0, 0, 0), fig=c(0.05,1,0.8,1))

# plot oscilogram ---------------------------------------------------------
    tuneR::plot(s, xlab = "", las=0, ann = FALSE, xaxt = "n", main = title)
    title(title, outer = TRUE)
    axis(1, at=1:round(sound$duration/100)/10,labels=1:round(sound$duration/100)*100,
         las=1)

# plot spectrogram --------------------------------------------------------
    par(fig=c(0.05,1,0.08,0.72), new=TRUE, las=0)
    phonTools::spectrogram(sound,
                           windowlength = window_length,
                           colors = spectrum_colors,
                           maxfreq = maximum_frequency,
                           dynamicrange = dynamic_range)
    axis(1, at=1:round(sound$duration/100)*100,labels=1:round(sound$duration/100)*100,
         las=1)

# reset graphical parameters to default -----------------------------------
    par(oma=c(0,0,0,0), mai=c(1.02, 0.82, 0.82, 0.42), fig=c(0,1,0,1))

  } else {

# save a file -------------------------------------------------------------
    grDevices::png(filename = paste0(output_file, ".png"),
        width = output_width,
        height = output_height,
        units = output_units)
    draw_sound(file_name,
               title,
               spectrum_colors,
               maximum_frequency,
               dynamic_range,
               window_length,
               output_file = NULL)
    grDevices::dev.off()
  }
}





