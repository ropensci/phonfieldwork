#' Draw spectrograms
#'
#' This function was slightly changed from \code{phonTools::spectrogram()}. Argument description is copied from \code{phonTools::spectrogram()}.
#'
#' @author Santiago Barreda <sbarreda@ucdavis.edu>
#'
#' @param sound Either a numeric vector representing a sequence of samples taken from a sound wave or a sound object created with the loadsound() or makesound() functions.
#' @param fs 	The sampling frequency in Hz. If a sound object is passed this does not need to be specified.
#' @param text_size numeric, text size (default = 1).
#' @param windowlength The desired analysis window length in milliseconds.
#' @param timestep If a negative value is given, -N, then N equally-spaced time steps are calculated. If a positive number is given, this is the spacing between adjacent analyses, in milliseconds.
#' @param padding The amount of zero padding for each window, measured in units of window length. For example, if the window is 50 points, and padding = 10, 500 zeros will be appended to each window.
#' @param preemphasisf Preemphasis of 6 dB per octave is added to frequencies above the specified frequency. For no preemphasis, set to a frequency higher than the sampling frequency.
#' @param maxfreq the maximum frequency to be displayed for the spectrogram up to a maximum of fs/2. This is set to 5000 Hz by default.
#' @param dynamicrange Values greater than this many dB below the maximum will be displayed in the same color.
#' @param nlevels The number of divisions to be used for the z-axis of the spectrogram. By default it is set equal to the dynamic range, meaning that a single color represents 1 dB on the z-axis.
#' @param show If FALSE, no spectrogram is plotted. This is useful if the user would like to perform an action on an existing spectrogram plot without having to redraw it.
#' @param window A string indicating the type of window desired. Supported types are: rectangular, hann, hamming, cosine, bartlett, gaussian, and kaiser.
#' @param windowparameter The parameter necessary to generate the window, if appropriate. At the moment, the only windows that require parameters are the Kaiser and Gaussian windows. By default, these are set to 2 for kaiser and 0.4 for gaussian windows.
#' @param x_axis If TRUE then draw x axis.
#' @param title Character with the title.
#'
#' @examples
#' draw_spectrogram(system.file("extdata", "test.wav", package = "phonfieldwork"))
#'
#' @export
#'
#' @importFrom stats fft
#' @importFrom tuneR readWave
#' @importFrom tuneR readMP3
#' @importFrom grDevices colorRampPalette
#' @importFrom graphics image
#'

draw_spectrogram <- function (sound,
                              fs = 22050,
                              text_size = 1,
                              windowlength = 5,
                              timestep = -1000,
                              padding = 10,
                              preemphasisf = 50,
                              maxfreq = 5000,
                              dynamicrange = 50,
                              nlevels = dynamicrange,
                              show = TRUE,
                              window = "kaiser",
                              windowparameter = -1,
                              x_axis = TRUE,
                              title = NULL){

  # This function is slightly modification of phonTools::spectrogram() by Santiago Barreda <sbarreda@ucdavis.edu>
  if(class(sound) != "integer"){
    ext <- unlist(strsplit(normalizePath(sound), "\\."))
    ext <- ext[length(ext)]

    if(ext == "wave"|ext == "wav"){
      s <- tuneR::readWave(sound)
    } else if(ext == "mp3"){
      s <- tuneR::readMP3(sound)
    } else{
      stop("The draw_spectrogram() functions works only with .wav(e) or .mp3 formats")
    }

    fs <- s@samp.rate
    sound <- s@left
  }
  n = ceiling((fs/1000) * windowlength)
  if (n%%2) {n = n + 1}
  if (timestep > 0) {timestep = floor(timestep/1000 * fs)}
  if (timestep <= 0) {timestep = floor(length(sound)/-timestep)}
  if (preemphasisf > 0){
    sound = phonTools::preemphasis(sound, preemphasisf, fs)
  }
  spots = seq(floor(n/2), length(sound) - n, timestep)
  padding = n * padding
  if ((n + padding)%%2){
    padding = padding + 1
    }
  N = n + padding
  spect = sapply(spots, function(x) {
    tmp = sound[x:(x + n - 1)] * phonTools::windowfunc(sound[x:(x + n - 1)],
                                                       window,
                                                       windowparameter)
    tmp = c(tmp, rep(0, padding))
    tmp = tmp - mean(tmp)
    tmp = stats::fft(tmp)[1:(N/2 + 1)]
    tmp = abs(tmp)^2
    tmp = log(tmp, 10) * 10
  })
  spect = t(spect)
  for (i in 1:nrow(spect)) spect[i, 1] = min(spect[i, -1])
  hz = (0:(N/2)) * (fs/N)
  times = spots * (1000/fs)
  if (maxfreq > (fs/2)){
    maxfreq = fs/2
    }
  spect = spect - max(spect)

  xlim = c(0, length(sound)/fs*1000)
  ylim = c(0, maxfreq)
  zcolors = grDevices::colorRampPalette(c("white", "black"))
  zrange = c(-dynamicrange, 0)
  nlevels = abs(zrange[1] - zrange[2]) * 1.2
  levels = pretty(zrange, nlevels)
  zcolors = zcolors(length(levels) - 1)
  spect[which(spect < (-1 * dynamicrange))] = -1 * dynamicrange

  if(windowparameter == -1 & window == "kaiser"){
    parameter_info <- paste0(", \u03B1: ", 2)
  } else if(windowparameter == -1 & window == "gaussian"){
    parameter_info <- paste0(", \u03C3: ", 0.4)
  } else if(windowparameter != -1 & window == "kaiser"){
    parameter_info <- paste0(", \u03B1: ", windowparameter)
  } else if(windowparameter != -1 & window == "gaussian"){
    parameter_info <- paste0(", \u03C3: ", windowparameter)
  } else {
    parameter_info <- ""
  }

  graphics::image(as.double(times),
        as.double(hz),
        spect,
        useRaster = FALSE,
        col = zcolors,
        ylim = ylim,
        xlim = xlim,
        main = as.character(title)[1],
        yaxt='n',
        xaxt='n',
        xlab = "",
        ylab = "")
  graphics::axis(2, cex.axis=text_size, las=1)
  graphics::title(ylab = paste0("Frequency (Hz)"))
  graphics::mtext(text = paste0(toupper(substring(window, 1, 1)),
                      tolower(substring(window, 2, nchar(window))),
                      " window (length: ",
                      windowlength,
                      " ms",
                      parameter_info,
                      "), dynamic range: ",
                      dynamicrange, " (dB)"),
        side = 4, cex = 0.6)
  if(x_axis){
    graphics::axis(1, cex.axis=text_size, las=1)
    graphics::title(xlab = "Time (s)")
  }
}
