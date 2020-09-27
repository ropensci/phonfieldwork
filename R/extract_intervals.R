#' Extract intervals
#'
#' Extract sound according to non-empty annotated intervals from TextGrid and
#' create soundfiles with correspondent names.
#'
#' @author George Moroz <agricolamz@gmail.com>
#'
#' @param file_name path to the soundfile
#' @param textgrid path to the TextGrid
#' @param tier tier number or name that should be used as base for extraction
#' and names
#' @param prefix character vector containing prefix(es) for file names
#' @param suffix character vector containing suffix(es) for file names
#' @param autonumber if TRUE automatically add number of extracted sound to the
#' file_name. Prevents from creating a duplicated files and wrong sorting.
#' @param path path to the directory where create extracted soundfiles.
#'
#' @return no output
#' @examples
#' # create two files in a temprary folder "test_folder"
#' s <- system.file("extdata", "test.wav", package = "phonfieldwork")
#' tdir <- tempdir()
#' file.copy(s, tdir)
#'
#' # Extract intervals according the TextGrid into the path
#' extract_intervals(
#'   file_name = paste0(tdir, "/test.wav"),
#'   textgrid = system.file("extdata", "test.TextGrid",
#'     package = "phonfieldwork"
#'   ),
#'   path = tdir
#' )
#'
#' list.files(tdir)
#' # [1] "e-2.wav" "s-3.wav" "t-1.wav" "t-4.wav" "test.TextGrid" "test.wav"
#' @export
#' @importFrom tuneR readWave
#' @importFrom tuneR readMP3
#' @importFrom tuneR bind
#' @importFrom tuneR writeWave
#'

extract_intervals <- function(file_name,
                              textgrid,
                              tier = 1,
                              prefix = NULL,
                              suffix = NULL,
                              autonumber = TRUE,
                              path) {
  ext <- tolower(substring(file_name, regexpr("\\..*$", file_name) + 1))

  if (ext == "wave" | ext == "wav") {
    s <- tuneR::readWave(file_name)
  } else if (ext == "mp3") {
    s <- tuneR::readMP3(file_name)
  } else {
    stop("The draw_sound() functions works only with .wav(e) or .mp3 formats")
  }

  # get start, end and annotation -------------------------------------------

  tg_df <- tier_to_df(textgrid, tier)
  starts <- tg_df[tg_df$content != "", "time_start"]
  ends <- tg_df[tg_df$content != "", "time_end"]
  if (isTRUE(autonumber)) {
    prefix <- paste0(
      add_leading_symbols(seq_along(tg_df$time_start)), "_",
      prefix
    )
  }
  annotations <- paste0(
    prefix,
    tg_df[tg_df$content != "", "content"],
    suffix
  )

  supress_message <- lapply(seq_along(starts), function(i) {
    s_fragment <- tuneR::extractWave(s,
      from = starts[i],
      to = ends[i],
      xunit = "time"
    )
    results <- tuneR::writeWave(
      s_fragment,
      paste0(path, "/", annotations[i], ".wav")
    )
    Sys.sleep(0.05)
  })
}
