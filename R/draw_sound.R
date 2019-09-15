#' Draw Oscilogram and Spectrogram
#'
#' Create oscilogram and spectrogram plot. Based on \code{phonTools::spectrogram}.
#'
#' @author George Moroz <agricolamz@gmail.com>
#'
#' @param path a path to the soundfile
#' @param colores If TRUE, a color spectrogram will be displayed. If FALSE, greyscale is used. If a vector of colors is provided, these colors are used to create the spectrogram.
#' @param maximum_frequency he maximum frequency to be displayed for the spectrogram up to a maximum of fs/2. This is set to 5000 Hz by default
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

draw_sound <- function(path,
                       title = NULL,
                       colores = FALSE,
                       maximum_frequency = 5000,
                       dynamic_range = 50,
                       window_length = 5,
                       output_file = NULL,
                       output_width = 750,
                       output_height = 500,
                       output_units = "px"){
  if(is.null(output_file)){
    s <- tuneR::readWave(path)
    sound <- phonTools::makesound(s@left, fs = s@samp.rate)
    par(oma=c(0,0,2,0), mai=c(0, 0, 0, 0), fig=c(0.05,1,0.8,1))
    tuneR::plot(s, xlab = "", las=0, ann = FALSE, xaxt = "n", main = title)
    title(title, outer = TRUE)
    axis(1, at=1:round(sound$duration/100)/10,labels=1:round(sound$duration/100)*100,
         las=1)
    par(fig=c(0.05,1,0.08,0.72), new=TRUE, las=0)
    phonTools::spectrogram(sound,
                           windowlength = window_length,
                           colors = colores,
                           maxfreq = maximum_frequency,
                           dynamicrange = dynamic_range)
    axis(1, at=1:round(sound$duration/100)*100,labels=1:round(sound$duration/100)*100,
         las=1)
    # reset graphical parameters to default
    par(oma=c(0,0,0,0), mai=c(1.02, 0.82, 0.82, 0.42), fig=c(0,1,0,1))
  } else {
    png(filename = output_file,
        width = output_width,
        height = output_height,
        units = output_units)
    draw_sound(path,
               title,
               colores,
               maximum_frequency,
               dynamic_range,
               window_length)
    dev.off()
  }
}
