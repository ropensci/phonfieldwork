#' Extract intervals
#'
#' Extract sound according to non-empty annotated intervals from TextGrid and create soundfiles with correspondent names.
#'
#' @author George Moroz <agricolamz@gmail.com>
#'
#' @param filename path to the soundfile
#' @param textgrid path to the TextGrid
#' @param tier tier number or name that should be used as base for extraction and names
#' @param prefix character vector containing prefix(es) for file names
#' @param suffix character vector containing suffix(es) for file names
#' @param autonumber if TRUE automatically add number of extracted sound to the filename. Prevents from creating a duplicated files
#' @param path path to the directory where create extracted soundfiles.
#' @return no output
#' @examples
#' # create two files in a temprary folder "test_folder"
#' s <-  system.file("extdata", "test.wav", package = "phonfieldwork")
#' tdir <- tempdir()
#' file.copy(s, tdir)
#'
#' # Extract intervals according the TextGrid into the path
#' extract_intervals(filename = paste0(tdir, "/test.wav"),
#'                   textgrid = example_textgrid,
#'                   path = tdir)
#'
#' list.files(tdir)
#' # [1] "e-2.wav" "s-3.wav" "t-1.wav" "t-4.wav" "test.TextGrid" "test.wav"
#'
#' @export
#' @importFrom tuneR readWave
#' @importFrom tuneR bind
#' @importFrom tuneR writeWave
#'

extract_intervals <- function(filename,
                              textgrid,
                              tier = 1,
                              prefix = NULL,
                              suffix = NULL,
                              autonumber = TRUE,
                              path){
  s <- tuneR::readWave(filename)
  if(grepl("TextGrid", textgrid[2])){
    tg <- textgrid
  } else{
    tg <- readLines(textgrid)
  }

# get start, end and annotation -------------------------------------------

  tg_df <- tier_to_df(textgrid, tier)
  starts <- tg_df[tg_df$annotation != "", "start"]
  ends <- tg_df[tg_df$annotation != "", "end"]
  if(isTRUE(autonumber)){
    auto <- 1:nrow(tg_df)
  } else {
    auto <- NULL
  }
  annotations <- paste0(prefix,
                        tg_df[tg_df$annotation != "", "annotation"],
                        suffix,
                        "-",
                        auto)

  lapply(seq_along(starts), function(i){
    s_fragment <- tuneR::extractWave(s,
                                     from = starts[i],
                                     to = ends[i],
                                     xunit = "time")
    tuneR::writeWave(s_fragment, paste0(path, "/", annotations[i], ".wav")) ->
      results
  }
  )
}
