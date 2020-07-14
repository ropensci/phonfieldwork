#' Draw spectrograms
#'
#' This function was slightly changed from \code{phonTools::spectrogram()}.
#' Argument description is copied from \code{phonTools::spectrogram()}.
#'
#' @author Santiago Barreda <sbarreda@ucdavis.edu>
#'
#' @param sound Either a numeric vector representing a sequence of samples taken
#' from a sound wave or a sound object created with the loadsound() or
#' makesound() functions.
#' @param fs 	The sampling frequency in Hz. If a sound object is passed this
#' does not need to be specified.
#' @param text_size numeric, text size (default = 1).
#' @param window_length The desired analysis window length in milliseconds.
#' @param spectrum_info logical. If \code{TRUE} then add information about
#' window method and params.
#' @param freq_scale a string indicating the type of frequency scale. Supported
#' types are: "Hz" and "kHz".
#' @param timestep If a negative value is given, -N, then N equally-spaced time
#' steps are calculated. If a positive number is given, this is the spacing
#' between adjacent analyses, in milliseconds.
#' @param padding The amount of zero padding for each window, measured in units
#' of window length. For example, if the window is 50 points, and padding = 10,
#' 500 zeros will be appended to each window.
#' @param preemphasisf Preemphasis of 6 dB per octave is added to frequencies
#' above the specified frequency. For no preemphasis, set to a frequency higher
#' than the sampling frequency.
#' @param frequency_range vector with the range of frequencies to be displayed
#' for the spectrogram up to a maximum of \code{fs}/2. This is set to 0-5 kHz by
#' default.
#' @param dynamic_range Values greater than this many dB below the maximum will
#' be displayed in the same color.
#' @param nlevels The number of divisions to be used for the z-axis of the
#' spectrogram. By default it is set equal to the dynamic range, meaning that a
#' single color represents 1 dB on the z-axis.
#' @param window A string indicating the type of window desired. Supported types
#' are: rectangular, hann, hamming, cosine, bartlett, gaussian, and kaiser.
#' @param windowparameter The parameter necessary to generate the window, if
#' appropriate. At the moment, the only windows that require parameters are the
#' Kaiser and Gaussian windows. By default, these are set to 2 for kaiser and
#' 0.4 for gaussian windows.
#' @param x_axis If \code{TRUE} then draw x axis.
#' @param title Character with the title.
#' @param raven_annotation Raven (Center for Conservation Bioacoustics) style
#' annotations (boxes over spectrogram). The dataframe that contains
#' \code{time_start}, \code{time_end}, \code{freq_low} and \code{freq_high}
#' columns. Optional columns are \code{colors} and \code{content}.
#'
#' @examples
#' \dontrun{
#' draw_spectrogram(system.file("extdata", "test.wav",
#'                              package = "phonfieldwork"))
#' }
#'
#' @export
#'
#' @importFrom stats fft
#' @importFrom tuneR readWave
#' @importFrom tuneR readMP3
#' @importFrom grDevices colorRampPalette
#' @importFrom graphics image
#' @importFrom graphics rect
#'

draw_spectrogram <- function (sound,
                              fs = 22050,
                              text_size = 1,
                              window_length = 5,
                              dynamic_range = 50,
                              window = "kaiser",
                              windowparameter = -1,
                              freq_scale = "kHz",
                              spectrum_info = TRUE,
                              timestep = -1000,
                              padding = 10,
                              preemphasisf = 50,
                              frequency_range = c(0, 5),
                              nlevels = dynamic_range,
                              x_axis = TRUE,
                              title = NULL,
                              raven_annotation = NULL){

# This function is slightly modification of phonTools::spectrogram()
# by Santiago Barreda <sbarreda@ucdavis.edu>

  if(class(sound) != "integer"){
    ext <- unlist(strsplit(normalizePath(sound), "\\."))
    ext <- ext[length(ext)]

    if(ext == "wave"|ext == "wav"){
      s <- tuneR::readWave(sound)
    } else if(ext == "mp3"){
      s <- tuneR::readMP3(sound)
    } else{
      stop("The draw_spectrogram() functions works only with .wav(e) or
           .mp3 formats")
    }
    fs <- s@samp.rate
    sound <- s@left
  }
  n <- ceiling((fs/1000) * window_length)
  if (n%%2) {n <- n + 1}
  if (timestep > 0) {timestep <- floor(timestep/1000 * fs)}
  if (timestep <= 0) {timestep <- floor(length(sound)/-timestep)}
  if (preemphasisf > 0){
    sound <- phonTools::preemphasis(sound, preemphasisf, fs)
    preemphasisf_text <- paste0("\nThe spectral slope is increased by
                                6 dB. per octave above ",
      preemphasisf,
      " Hz")
    preemphasisf_line <- 0.5
  } else {
    preemphasisf_text <- ""
    preemphasisf_line <- 0
  }
  spots <- seq(floor(n/2), length(sound) - n, timestep)
  padding <- n * padding
  if ((n + padding)%%2){
    padding <- padding + 1
    }
  N <- n + padding
  spect <- sapply(spots, function(x) {
    tmp <- sound[x:(x + n - 1)] * phonTools::windowfunc(sound[x:(x + n - 1)],
                                                       window,
                                                       windowparameter)
    tmp <- c(tmp, rep(0, padding))
    tmp <- tmp - mean(tmp)
    tmp <- stats::fft(tmp)[1:(N/2 + 1)]
    tmp <- abs(tmp)^2
    tmp <- log(tmp, 10) * 10
  })
  spect <- t(spect)
  for (i in 1:nrow(spect)){
    spect[i, 1] <- min(spect[i, -1])
  }

  if(freq_scale == "kHz"){
    hz <- (0:(N/2)) * (fs/N)/1000
  } else if(freq_scale == "Hz"){
    hz <- (0:(N/2)) * (fs/N)
  } else {
    stop("The only possible values for the freq_scale argument are
         'kHz' and 'Hz'")
  }

  times <- spots * (1000/fs)
  if (frequency_range[2] > (fs/2) & freq_scale == "Hz"){
    frequency_range[2] <- fs/2
  } else if(frequency_range[2] > (fs/2)/1000 & freq_scale == "kHz"){
    frequency_range[2] <- (fs/2)/1000
  }

  spect <- spect - max(spect)

  xlim <- c(0, length(sound)/fs*1000)
  ylim <- c(frequency_range[1], frequency_range[2])
  zcolors <- grDevices::colorRampPalette(c("white", "black"))
  zrange <- c(-dynamic_range, 0)
  nlevels <- abs(zrange[1] - zrange[2]) * 1.2
  levels <- pretty(zrange, nlevels)
  zcolors <- zcolors(length(levels) - 1)
  spect[which(spect < (-1 * dynamic_range))] <- -1 * dynamic_range

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
  graphics::title(ylab = paste0("Frequency (", freq_scale, ")"), cex.lab = 0.7)
  if(spectrum_info){
  graphics::mtext(text = paste0(toupper(substring(window, 1, 1)),
                      tolower(substring(window, 2, nchar(window))),
                      " window (length: ",
                      window_length,
                      " ms",
                      parameter_info,
                      "), dynamic range: ",
                      dynamic_range, " (dB)",
                      preemphasisf_text),
        side = 4, cex = 0.6, line = preemphasisf_line)
  }
  if(x_axis){
    graphics::axis(1, cex.axis=text_size, las=1)
    graphics::title(xlab = "time (ms)")
  }
  if(!is.null(raven_annotation)){
    if(is.null(raven_annotation$colors)){
      raven_annotation$colors <- "black"
    }
    if("time_start" %in% names(raven_annotation) &
       "time_end" %in% names(raven_annotation) &
       "freq_low" %in% names(raven_annotation) &
       "freq_high" %in% names(raven_annotation)){
      graphics::rect(xleft = raven_annotation$time_start,
                     xright = raven_annotation$time_end,
                     ybottom = raven_annotation$freq_low,
                     ytop = raven_annotation$freq_high,
                     lwd = 2,
                     border = raven_annotation$colors)
      if("content" %in% names(raven_annotation)){
        graphics::text(x = raven_annotation$time_start,
                       y = raven_annotation$freq_high,
                       labels = raven_annotation$content,
                       pos = 3,
                       offset = 0.2,
                       cex = text_size,
                       col = raven_annotation$colors)
      }
    } else{
      warning("raven_annotation should have time_start, time_end,
              freq_low and freq_high columns")
    }

  }
}
