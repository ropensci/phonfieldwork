#' Draw Oscilogram and Spectrogram
#'
#' Create oscilogram and spectrogram plot. Based on \code{phonTools::spectrogram}.
#'
#' @author George Moroz <agricolamz@gmail.com>
#'
#' @param path a path to the soundfile
#' @param colores If TRUE, a color spectrogram will be displayed. If FALSE, greyscale is used. If a vector of colors is provided, these colors are used to create the spectrogram.
#' @param maximum_frequency he maximum frequency to be displayed for the spectrogram up to a maximum of fs/2. This is set to 5000 Hz by default
#' @param dynamic_range Values greater than this many dB below the maximum will be displayed in the same color
#' @param window_length The desired analysis window length in milliseconds.
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
                       window_length = 5){
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
}
