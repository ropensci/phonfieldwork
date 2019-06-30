#' Annotate textgrid
#'
#' Annotates textgrids. It is possible to difine step in the argument "each", so each second element will be annotated.
#'
#' @author George Moroz <agricolamz@gmail.com>
#'
#' @param annotation vector of stimuli (obligatory)
#' @param textgrid character with a filename or path to the TextGrid
#' @param tier value that could be either ordinal number of the tier either name of the tier
#' @param each non-negative integer. Each element of x is repeated each times
#' @param backup logical. If TRUE (by default) it creates a backup tier.
#'
#' @examples
#' ## Not run:
#' annotate_textgrid(annotation = c("a", "b"), textgrid = "exampl.TextGrid")
#'
#' @export
#'

  annotate_textgrid <- function(annotation,
                              textgrid,
                              tier = 1,
                              each = 1,
                              backup = TRUE) {

# read TextGrid -----------------------------------------------------------
  tg <- readLines(textgrid)

# get start and end info about tiers --------------------------------------
  starts <- grep("item \\[\\d{1,}\\]:", tg)
  ends <- c(starts[-1]-1, length(tg))

# extract tier by number --------------------------------------------------
  if(is.numeric(tier)){
    if(tier > length(starts)){
      stop(paste0("It looks like there is no tier number '", tier, "'"))
    }
    tier_number <- tier
  } else {

# extract tier by name ----------------------------------------------------
    names <- sub('"\\s*',
                 "",
                 sub('\\s*name = "',
                     "",
                     tg[grep('name = ".*"', tg)]))
    if(!(tier %in% names)){
      stop(paste0("It looks like there is no any tier with a name '", tier, "'"))
    }
        tier_number <- which(names %in% tier)
  }
  w_tier <- tg[starts[tier_number]:ends[tier_number]]

# create a backup ---------------------------------------------------------
  if(isTRUE(backup)){
    backup_tier <- w_tier
    backup_tier[1] <- paste0('    item [', length(starts)+1, ']:')
    backup_tier[3] <- paste0('        name = "backup ',
                             sub('"\\s*',
                                 "",
                                 sub('\\s*name = "',
                                     "",
                                     backup_tier[3])),
                             '" ')
    tg <- append(tg, backup_tier)
    tg[7] <- paste0("size = ", length(starts)+1, " ")
  }

# annotate ----------------------------------------------------------------
  n_an <- length(w_tier[grep("text = ", w_tier)])
  w_tier[grep("text = ", w_tier)][seq_along(1:n_an) %% each == 0] <- paste0(
    '            text = "',
    annotation,
    '" ')
  tg[starts[tier_number]:ends[tier_number]] <- w_tier

# write the result TextGrid -----------------------------------------------
  writeLines(tg, textgrid)
}
