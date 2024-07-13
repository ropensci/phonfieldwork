#' Concatenate TextGrids
#'
#' Creates a merged TextGrids from TextGrids files in a folder.
#'
#' @author George Moroz <agricolamz@gmail.com>
#'
#' @param path path to the directory with soundfiles.
#' @param result_file_name name of the result and annotation files.
#'
#' @examples
#' # create two files in a temprary folder "test_folder"
#' t1 <- system.file("extdata", "test.TextGrid", package = "phonfieldwork")
#' t2 <- system.file("extdata", "post.TextGrid", package = "phonfieldwork")
#' tdir <- tempdir()
#' file.copy(c(t1, t2), tdir)
#'
#' # here are two .wav files in a folder
#' list.files(tdir)
#' # [1] "post.TextGrid" "test.TextGrid" ...
#'
#' # Concatenate all TextGrids from the folder into concatenated.TextGrid
#' concatenate_textgrids(path = tdir, result_file_name = "concatenated")
#'
#' list.files(tdir)
#' # [1] "concatenated.TextGrid" "post.TextGrid" "test.TextGrid" ...
#' @return no output
#' @export
#'


concatenate_textgrids <- function(path,
                                  result_file_name = "concatenated") {

  # get list of files -------------------------------------------------------
  path <- normalizePath(path)
  files <- list.files(path, "\\.TextGrid$", full.names = TRUE)

  # read all textgrids and converge them into one df ------------------------
  start <- 0
  results <- do.call(
    rbind,
    lapply(files, function(i) {
      df <- textgrid_to_df(file_name = i)
      df$time_start <- df$time_start + start
      df$time_end <- df$time_end + start
      assign("start", max(df$time_end), parent.frame(n = 2))
      return(df)
    })
  )

  # concatenate textgrids ---------------------------------------------------
  if (length(unique(results$tier)) == length(unique(results$tier_name))) {
    tier_names <- unique(results$tier_name)
  } else {
    tier_names <- unique(results$tier)
  }

  create_empty_textgrid(duration = max(results$time_end),
                        path = path,
                        result_file_name = result_file_name)

  s <- split(results[, c("time_start", "time_end", "content")], results$tier)

  l <- lapply(seq_along(s), function(j) {
    df_to_tier(
      df = s[[j]],
      textgrid = paste0(path, "/", result_file_name, ".TextGrid"),
      tier_name = tier_names[j]
    )
  })
}
