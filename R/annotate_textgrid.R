#' Annotate textgrid
#'
#' Annotates textgrids. It is possible to difine step in the argument "each", so each second element of the tier will be annotated.
#'
#' @author George Moroz <agricolamz@gmail.com>
#'
#' @param annotation vector of stimuli
#' @param textgrid character with a filename or path to the TextGrid
#' @param tier value that could be either ordinal number of the tier either name of the tier
#' @param each non-negative integer. Each element of x is repeated each times
#' @param backup logical. If TRUE (by default) it creates a backup tier.
#' @param write logical. If TRUE (by dafault) it overwrites an existing tier.
#'
#' @examples
#' annotate_textgrid(annotation = c("", "t", "e", "s", "t"),
#'                   textgrid = example_textgrid, tier = 2, write = FALSE)
#'
#' @export
#'

annotate_textgrid <- function(annotation,
                              textgrid,
                              tier = 1,
                              each = 1,
                              backup = TRUE,
                              write = TRUE) {

# read TextGrid -----------------------------------------------------------
  if(grepl("TextGrid", textgrid[2])){
    tg <- textgrid
  } else{
    tg <- readLines(normalizePath(textgrid))
  }


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
  ifelse(grepl("IntervalTier", w_tier[2]),
         anotation_prefix <- "text = ", # for intervaltiers
         anotation_prefix <- "mark = ") # for pointtiers

  n_an <- length(w_tier[grep(anotation_prefix, w_tier)])
  w_tier[grep(anotation_prefix, w_tier)][1:n_an %% each == 0] <- paste0(
    '            ',
    anotation_prefix,
    '"',
    annotation,
    '" ')

  # merge annotation with TextGrid
  tg[starts[tier_number]:ends[tier_number]] <- w_tier

# write the result TextGrid -----------------------------------------------
  if (isTRUE(write)) {
    writeLines(tg, normalizePath(textgrid))
  } else {
    return(tg)
  }
}
