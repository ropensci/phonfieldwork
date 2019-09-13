#' Draw Oscilogram and Spectrogram
#'
#' Create oscilogram and spectrogram plot. Based on \code{phonTools::spectrogram}.
#'
#' @author George Moroz <agricolamz@gmail.com>
#'
#' @param path a path to the soundfile
#' @param colores If TRUE, a color spectrogram will be displayed. If FALSE, greyscale is used. If a vector of colors is provided, these colors are used to create the spectrogram.
#' @param maxfreq he maximum frequency to be displayed for the spectrogram up to a maximum of fs/2. This is set to 5000 Hz by default
#' @param dynamicrange Values greater than this many dB below the maximum will be displayed in the same color
#' @return Oscilogram and Spectrogram plot.
#'
#' @export
#' @importFrom tuneR readWave
#' @importFrom phonTools makesound
#' @importFrom phonTools spectrogram

draw_sound <- function(path,
                       colores = FALSE,
                       maxfreq = 5000,
                       dynamicrange = 50){
  s <- tuneR::readWave(path)
  sound <- phonTools::makesound(s@left, fs = s@samp.rate)
  par(mai=c(0, 0, 0, 0), fig=c(0.1,1,0.8,1))
  plot(s, xlab = "", las=0, ann = FALSE, xaxt = "n")
  axis(1, at=1:round(sound$duration/100)/10,labels=1:round(sound$duration/100)*100,
       las=1)
  par(fig=c(0.1,1,0.1,0.7), new=TRUE, las=0)
  phonTools::spectrogram(sound,
                         colors = colores,
                         maxfreq = maxfreq,
                         dynamicrange = dynamicrange)
}
