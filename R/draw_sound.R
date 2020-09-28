#' Draw Oscilogram, Spectrogram and annotation
#'
#' Create oscilogram and spectrogram plot.
#'
#' @author George Moroz <agricolamz@gmail.com>
#'
#' @param file_name a sound file
#' @param annotation a source for annotation files (e. g. TextGrid)
#' @param from Time in seconds at which to start extraction.
#' @param to Time in seconds at which to stop extraction.
#' @param zoom numeric vector of zoom window time (in seconds). It will draw
#' the whole oscilogram and part of the spectrogram.
#' @param text_size numeric, text size (default = 1).
#' @param title the title for the plot
#' @param freq_scale a string indicating the type of frequency scale.
#' Supported types are: "Hz" and "kHz".
#' @param spectrum_info logical. If \code{TRUE} then add information about
#' window method and params.
#' @param preemphasisf Preemphasis of 6 dB per octave is added to frequencies
#' above the specified frequency. For no preemphasis, set to a frequency higher
#' than the sampling frequency.
#' @param frequency_range vector with the range of frequencies to be displayed
#' for the spectrogram up to a maximum of fs/2. By default this is set to 0-5
#' kHz.
#' @param dynamic_range values greater than this many dB below the maximum will
#' be displayed in the same color
#' @param window_length the desired analysis window length in milliseconds.
#' @param window A string indicating the type of window desired. Supported types
#' are: "rectangular", "hann", "hamming", "cosine", "bartlett", "gaussian", and
#' "kaiser".
#' @param windowparameter The parameter necessary to generate the window, if
#' appropriate. At the moment, the only windows that require parameters are the
#' Kaiser and Gaussian windows. By default, these are set to 2 for kaiser and
#' 0.4 for gaussian windows.
#' @param pitch path to the Praat `.Pitch` file or result of
#' \code{pitch_to_df()} function. This variable provide data for visualisation
#' of a pitch contour exported from Praat.
#' @param pitch_range vector with the range of frequencies to be displayed.
#' By default this is set to 75-350 Hz.
#' @param intensity path to the Praat `.Intensity` file or result of
#' \code{intensity_to_df()} function. This variable provide data for
#' visualisation of an intensity contour exported from Praat.
#' @param output_file the name of the output file
#' @param output_width the width of the device
#' @param output_height the height of the device
#' @param output_units the units in which height and width are given.
#' Can be "px" (pixels, the default), "in" (inches), "cm" or "mm".
#' @param sounds_from_folder path to a folder with multiple sound files.
#' If this argument is not \code{NULL}, then the function goes through all
#' files and creates picture for all of them.
#' @param textgrids_from_folder path to a folder with multiple .TextGrid files.
#' If this argument is not \code{NULL}, then the function goes through all files
#' and create picture for all of them.
#' @param pic_folder_name name for a folder, where all pictures will be stored
#' in case \code{sounds_from_folder} argument is not \code{NULL}
#' @param title_as_filename logical. If true adds filename title to each picture
#' @param prefix prefix for all file names for created pictures in case
#' \code{sounds_from_folder} argument is not \code{NULL}
#' @param suffix suffix for all file names for created pictures in case
#' \code{sounds_from_folder} argument is not \code{NULL}
#' @param autonumber if TRUE automatically add number of extracted sound to the
#' file_name. Prevents from creating a duplicated files and wrong sorting.
#' @param raven_annotation Raven (Center for Conservation Bioacoustics) style
#' annotations (boxes over spectrogram). The dataframe that contains
#' \code{time_start}, \code{time_end}, \code{freq_low} and \code{freq_high}
#' columns. Optional columns are \code{colors} and \code{content}.
#' @param formant_df dataframe with formants from \code{formant_to_df()} function
#'
#' @return Oscilogram and spectrogram plot (and possibly TextGrid annotation).
#'
#' @examples
#' \dontrun{
#' draw_sound(system.file("extdata", "test.wav", package = "phonfieldwork"))
#'
#' draw_sound(
#'   system.file("extdata", "test.wav", package = "phonfieldwork"),
#'   system.file("extdata", "test.TextGrid",
#'     package = "phonfieldwork"
#'   )
#' )
#'
#' draw_sound(system.file("extdata", "test.wav", package = "phonfieldwork"),
#'   system.file("extdata", "test.TextGrid", package = "phonfieldwork"),
#'   pitch = system.file("extdata", "test.Pitch",
#'     package = "phonfieldwork"
#'   ),
#'   pitch_range = c(50, 200)
#' )
#' draw_sound(system.file("extdata", "test.wav", package = "phonfieldwork"),
#'   system.file("extdata", "test.TextGrid", package = "phonfieldwork"),
#'   pitch = system.file("extdata", "test.Pitch",
#'     package = "phonfieldwork"
#'   ),
#'   pitch_range = c(50, 200),
#'   intensity = intensity_to_df(system.file("extdata", "test.Intensity",
#'     package = "phonfieldwork"
#'   ))
#' )
#' draw_sound(system.file("extdata", "test.wav", package = "phonfieldwork"),
#'   formant_df = formant_to_df(system.file("extdata", "e.Formant",
#'     package = "phonfieldwork"
#'   ))
#' )
#' }
#' @export
#'
#' @importFrom tuneR readWave
#' @importFrom tuneR readMP3
#' @importFrom tuneR extractWave
#' @importFrom stats sd
#' @importFrom grDevices png
#' @importFrom grDevices dev.off
#' @importFrom grDevices rgb
#' @importFrom graphics par
#' @importFrom graphics axis
#' @importFrom graphics text
#' @importFrom graphics points
#' @importFrom graphics abline
#' @importFrom graphics segments
#' @importFrom graphics rect
#' @importFrom graphics plot
#'

draw_sound <- function(file_name,
                       annotation = NULL,
                       from = NULL,
                       to = NULL,
                       zoom = NULL,
                       text_size = 1,
                       output_file = NULL,
                       title = NULL,
                       freq_scale = "kHz",
                       frequency_range = c(0, 5),
                       dynamic_range = 50,
                       window_length = 5,
                       window = "kaiser",
                       windowparameter = -1,
                       preemphasisf = 50,
                       spectrum_info = TRUE,
                       raven_annotation = NULL,
                       formant_df = NULL,
                       pitch = NULL,
                       pitch_range = c(75, 350),
                       intensity = NULL,
                       output_width = 750,
                       output_height = 500,
                       output_units = "px",
                       sounds_from_folder = NULL,
                       textgrids_from_folder = NULL,
                       pic_folder_name = "pics",
                       title_as_filename = TRUE,
                       prefix = NULL,
                       suffix = NULL,
                       autonumber = FALSE) {
  if (is.null(sounds_from_folder)) {
    if (is.null(output_file)) {
      # read file and convert to phonTools format -------------------------------
      if (class(file_name) == "Wave") {
        s <- file_name
      } else {
        ext <- tolower(tools::file_ext(file_name))

        if (ext == "wave" | ext == "wav") {
          s <- tuneR::readWave(file_name)
        } else if (ext == "mp3") {
          s <- tuneR::readMP3(file_name)
        } else {
          graphics::par(
            oma = c(0, 0, 0, 0),
            mai = c(1.02, 0.82, 0.82, 0.42),
            fig = c(0, 1, 0, 1)
          )
          stop("The draw_sound() functions works only with .wav(e) or
               .mp3 formats")
        }
      }

      if (!is.null(from) & !is.null(to)) {
        if (from >= to) {
          graphics::par(
            oma = c(0, 0, 0, 0),
            mai = c(1.02, 0.82, 0.82, 0.42),
            fig = c(0, 1, 0, 1)
          )
          stop("Argument from should be smaler then argument to.")
        }
        if (to > length(s@left) / s@samp.rate) {
          to <- length(s@left) / s@samp.rate
        }
      } else if (!is.null(from) & is.null(to)) {
        to <- length(s@left) / s@samp.rate
      } else if (is.null(from) & !is.null(to)) {
        from <- 0
        if (to > length(s@left) / s@samp.rate) {
          to <- length(s@left) / s@samp.rate
        }
      } else if (is.null(from) & is.null(to)) {
        from <- 0
        to <- length(s@left) / s@samp.rate
      }

      s <- tuneR::extractWave(s, from = from, to = to, xunit = "time")

      # if there is no title no need to have a space for it ---------------------
      title_space <- ifelse(is.null(title), 0, 2)

      # plot oscilogram ---------------------------------------------------------
      low_boundary <- ifelse(is.null(zoom), 0.83, 0.91)

      graphics::par(
        oma = c(0, 0, title_space, 0),
        mai = c(0, 0.8, 0, 0.2),
        fig = c(0, 0.97, low_boundary, 1)
      )

      n <- max(abs(range(s@left)))
      s_range <- floor(n / 10^(nchar(n) - 1)) * 10^(nchar(n) - 1)
      graphics::plot(
        y = s@left,
        x = seq(0, length(s@left) / s@samp.rate * 1000,
          length.out = length(s@left)
        ),
        type = "l",
        xaxt = "n",
        yaxt = "n",
        ylab = "",
        xlab = "",
        xaxs = "i"
      )
      title(as.character(title), outer = TRUE, cex.main = text_size + 0.2)
      if (!is.null(zoom)) {
        graphics::rect(
          xleft = zoom[1] * 1000,
          xright = zoom[2] * 1000,
          ybottom = min(s@left) - stats::sd(s@left),
          ytop = max(s@left) + stats::sd(s@left),
          border = "darkgrey",
          col = grDevices::rgb(0, 0, 0.5, alpha = 0.05)
        )
        graphics::axis(1, las = 1)
      }
      # plot spectrogram --------------------------------------------------------

      if (is.null(annotation) & is.null(pitch) & is.null(intensity)) {
        low_boundary <- 0.1
      } else if (is.null(annotation) & (!is.null(pitch) | !is.null(intensity))) {
        low_boundary <- 0.1 + 0.17
      } else if (!is.null(annotation) & is.null(pitch) & is.null(intensity)) {
        low_boundary <- 0.1 + 0.17
      } else if (!is.null(annotation) & (!is.null(pitch) | !is.null(intensity))) {
        low_boundary <- 0.1 + 0.17 + 0.17
      }

      graphics::par(fig = c(0, 0.97, low_boundary, 0.83), new = TRUE)
      if (!is.null(zoom)) {
        for_spectrum <- tuneR::extractWave(s,
          from = zoom[1],
          to = zoom[2],
          xunit = "time"
        )
      } else {
        for_spectrum <- s
      }
      draw_spectrogram(for_spectrum@left,
        fs = for_spectrum@samp.rate,
        text_size = text_size,
        window_length = window_length,
        window = window,
        windowparameter = windowparameter,
        freq_scale = freq_scale,
        frequency_range = frequency_range,
        dynamic_range = dynamic_range,
        preemphasisf = preemphasisf,
        spectrum_info = spectrum_info,
        raven_annotation = raven_annotation,
        formant_df = formant_df,
        x_axis = low_boundary == 0.1
      )


      # plot pitch --------------------------------------------------------------
      if (is.null(annotation) & (!is.null(pitch) | !is.null(intensity))) {
        upper_boundary <- 0.1 + 0.17
        low_boundary <- 0.1
      } else if (!is.null(annotation) & is.null(pitch) & is.null(intensity)) {
        upper_boundary <- 0.1 + 0.17
        low_boundary <- 0.1
      } else if (!is.null(annotation) & (!is.null(pitch) | !is.null(intensity))) {
        upper_boundary <- 0.1 + 0.17 + 0.17
        low_boundary <- 0.1 + 0.17
      }

      if (!is.null(pitch)) {
        graphics::par(fig = c(0, 0.97, low_boundary, upper_boundary), new = TRUE)
        if (class(pitch) != "data.frame") {
          pitch <- phonfieldwork::pitch_to_df(pitch)
        }

        graphics::plot(pitch$time_start * 1000, pitch$frequency,
          ylim = c(pitch_range[1], pitch_range[2] + pitch_range[2] / 10),
          type = "l",
          lwd = 1.5,
          cex = 0,
          xaxt = "n",
          yaxt = "n",
          ylab = "Frequency (Hz)",
          xlab = "",
          las = 1,
          xaxs = "i",
          yaxs = "i",
          cex.lab = 0.7
        )

        graphics::axis(2,
          cex.axis = text_size, las = 1,
          at = pretty(pitch_range, n = 4)
        )
        if (low_boundary == 0.1) {
          graphics::axis(1, cex.axis = text_size)
          graphics::title(xlab = "time (ms)", cex.lab = 0.7)
        }
        if (!is.null(intensity)) {
          if (!("data.frame" %in% class(intensity))) {
            intensity <- phonfieldwork::intensity_to_df(intensity)
          }
          graphics::legend(
            x = 0, y = pitch_range[2] + pitch_range[2] / 10,
            legend = c("pitch", "intensity"),
            lty = c(1, 3), cex = 0.8, box.lty = 0
          )
          graphics::par(new = TRUE)
          graphics::plot(intensity$time_start * 1000, intensity$intensity,
            type = "l", xlab = "", ylab = "", axes = FALSE,
            lty = 3
          )
          graphics::axis(4,
            cex.axis = text_size, las = 1,
            at = pretty(intensity$intensity, n = 4)
          )
          graphics::mtext(
            text = "Intensity (Db)",
            side = 4, cex = 0.6, padj = -3
          )
        }
      } else if (!is.null(intensity)) {
        graphics::par(fig = c(0, 0.97, low_boundary, upper_boundary), new = TRUE)
        if ("data.frame" %in% class(intensity)) {
          intensity <- phonfieldwork::intensity_to_df(intensity)
        }
        graphics::plot(intensity$time_start * 1000, intensity$intensity,
          type = "l",
          cex = 0,
          xaxt = "n",
          yaxt = "n",
          ylab = "Intensity (Db)",
          xlab = "",
          las = 1,
          xaxs = "i",
          yaxs = "i",
          cex.lab = 0.7
        )
        graphics::axis(2,
          cex.axis = text_size, las = 1,
          at = pretty(intensity$intensity, n = 4)
        )
        if (low_boundary == 0.1) {
          graphics::axis(1, cex.axis = text_size)
          graphics::title(xlab = "time (ms)", cex.lab = 0.7)
        }
      }

      # plot textgrid -----------------------------------------------------------
      if (!is.null(annotation)) {
        graphics::par(fig = c(0, 0.97, 0.1, 0.1 + 0.17), new = TRUE)

        if (!("data.frame" %in% class(annotation))) {
          df <- phonfieldwork::textgrid_to_df(annotation)
        } else {
          df <- annotation
        }

        if (sum(c("time_start", "time_end", "content") %in% names(df)) != 3) {
          graphics::par(
            oma = c(0, 0, 0, 0),
            mai = c(1.02, 0.82, 0.82, 0.42),
            fig = c(0, 1, 0, 1)
          )
          stop('data.frame that you provide to annotation argument should
               contain "time_start", "time_end" and "content" columns')
        }

        if (!("tier" %in% names(df))) {
          df$tier <- 1
        }

        if (!is.null(zoom)) {
          from <- zoom[1]
          to <- zoom[2]
        }

        # remove annotation that are out of scope
        df <- df[df$time_start >= from, ]
        df <- df[df$time_end <= to + 0.000000001, ]

        df$time_start <- (df$time_start - from) * 1000
        df$time_end <- (df$time_end - from) * 1000

        # in case annotation exceed the length of the sound,
        # change its value to sound length

        df$time_end <- ifelse(
          df$time_start < length(for_spectrum@left) / for_spectrum@samp.rate * 1000 &
            df$time_end > length(for_spectrum@left) / for_spectrum@samp.rate * 1000,
          length(for_spectrum@left) / for_spectrum@samp.rate * 1000,
          df$time_end
        )

        if (nrow(df) < 1) {
          graphics::par(
            oma = c(0, 0, 0, 0),
            mai = c(1.02, 0.82, 0.82, 0.42),
            fig = c(0, 1, 0, 1)
          )
          stop("There is no annotion in selected time interval.")
        }

        if (from != 0) {
          lapply(unique(df$tier), function(i) {
            extended <- data.frame(
              id = NA,
              time_start = 0,
              time_end = min(df[df$tier == i, ]$time_start),
              content = "",
              tier = i,
              tier_name = "",
              source = unique(df$source)
            )
            df <<- rbind(extended, df)
          })
        }
        df <- df[order(df$tier), ]
        df$mid_point <- df$time_start + (df$time_end - df$time_start) / 2
        df$fake_y <- max(df$tier) - min(df$tier)
        df$tier <- -df$tier
        graphics::plot(
          x = df$mid_point,
          y = df$fake_y,
          xlim = c(
            df$time_start[1],
            length(for_spectrum@left) / for_spectrum@samp.rate * 1000
          ),
          ylim = range(df$tier) + c(-0.4, 0.4),
          cex = 0,
          yaxt = "n",
          xaxt = "n",
          xlab = "time (ms)",
          ylab = "",
          xaxs = "i"
        )
        graphics::abline(h = unique(df$tier)[-length(unique(df$tier))] - 0.5)
        graphics::segments(
          x0 = df$time_start, x1 = df$time_start,
          y0 = df$tier - 0.5, y1 = df$tier + 0.5
        )
        graphics::segments(
          x0 = df$time_end, x1 = df$time_end,
          y0 = df$tier - 0.5, y1 = df$tier + 0.5
        )
        graphics::points(
          x = df[df$content != "", "mid_point"],
          y = df[df$content != "", "tier"],
          col = "white", pch = 19, cex = text_size + 1.5
        )
        graphics::text(
          x = df$mid_point, y = df$tier, labels = df$content,
          cex = text_size
        )
        graphics::axis(1, cex.axis = text_size)
      }
      # reset graphical parameters to default -----------------------------------
      graphics::par(
        oma = c(0, 0, 0, 0),
        mai = c(1.02, 0.82, 0.82, 0.42),
        fig = c(0, 1, 0, 1)
      )
    } else {
      # save a file -------------------------------------------------------------
      grDevices::png(
        filename = paste0(output_file, ".png"),
        width = output_width,
        height = output_height,
        units = output_units
      )
      draw_sound(
        file_name = file_name,
        annotation = annotation,
        from = from,
        to = to,
        zoom = zoom,
        text_size = text_size,
        title = title,
        window_length = window_length,
        window = window,
        windowparameter = windowparameter,
        freq_scale = freq_scale,
        frequency_range = frequency_range,
        dynamic_range = dynamic_range,
        preemphasisf = preemphasisf,
        spectrum_info = spectrum_info,
        raven_annotation = raven_annotation,
        formant_df = formant_df,
        output_file = NULL
      )
      supress_message <- grDevices::dev.off()
    }
    # in case of multuple files -----------------------------------------------
  } else {
    # get a correct picture folder path ---------------------------------------
    sounds_from_folder <- normalizePath(sounds_from_folder)
    if (!is.null(textgrids_from_folder)) {
      textgrids_from_folder <- normalizePath(textgrids_from_folder)
    }
    slashes <- unlist(gregexpr("/", sounds_from_folder))
    cut <- unlist(gregexpr("/", sounds_from_folder))[length(slashes)]
    pic_path <- paste0(substr(sounds_from_folder, 1, cut), pic_folder_name)

    # create a directory if it is not exists ----------------------------------
    if (!dir.exists(pic_path)) {
      dir.create(pic_path)
    }

    # get list of sounds and future pictures ----------------------------------
    sounds <- paste0(
      sounds_from_folder,
      "/",
      list.files(
        sounds_from_folder,
        pattern = "(\\.WAVE?$)|(\\.wave?$)|(\\.MP3?$)|
                       (\\.mp3?$)"
      )
    )

    if (!is.null(textgrids_from_folder)) {
      textgrids <- paste0(
        sounds_from_folder,
        "/",
        list.files(
          textgrids_from_folder,
          pattern = "\\.TextGrid$"
        )
      )
    } else {
      textgrids <- NULL
    }

    if (isTRUE(autonumber)) {
      prefix <- paste0(
        phonfieldwork::add_leading_symbols(seq_along(sounds)),
        "_", prefix
      )
    }

    names <- list.files(
      sounds_from_folder,
      "(\\.WAVE?$)|(\\.wave?$)|(\\.MP3?$)|(\\.mp3?$)"
    )
    names <- unlist(strsplit(
      names,
      "(\\.WAVE?$)|(\\.wave?$)|(\\.MP3?$)|(\\.mp3?$)"
    ))

    pics <- paste0(
      pic_path, "/",
      prefix,
      names
    )

    # loop over the draw_sound function ---------------------------------------

    supress_message <- lapply(seq_along(sounds), function(i) {
      draw_sound(sounds[i],
        annotation = textgrids[i],
        output_file = paste0(pics[i], suffix[i]),
        title = switch(title_as_filename + 1, NULL, names[i]),
        text_size = text_size,
        window_length = window_length,
        window = window,
        windowparameter = windowparameter,
        freq_scale = freq_scale,
        frequency_range = frequency_range,
        dynamic_range = dynamic_range,
        preemphasisf = preemphasisf,
        spectrum_info = spectrum_info,
        raven_annotation = raven_annotation,
        formant_df = formant_df,
        output_width = output_width,
        output_height = output_height,
        output_units = output_units,
        title_as_filename = FALSE
      )
    })
  }
}
