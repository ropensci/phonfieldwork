#' Dataframe to EXMARaLDA's .exb
#'
#' Convert a dataframe to EXMARaLDA's .exb
#'
#' @author Valeria Buntiakova <valleriabun@gmail.com>
#'
#' @param df an R dataframe object that contains columns named 'tier', 'tier_name', 'content', 'time_start', 'time_end' and 'id'
#' @param name transcription name
#' @param output_file the name of the result .html file
#' @param output_dir the output directory for the rendered file
#' @param referenced_file a filepath for .wav
#' @param ud_meta a vector ('key':'value') of meta information (not obligatory)
#' @param speaker_table a table with speaker information; must include columns 'id', 'abbreviation', 'sex' (not obligatory)
#' @return .xml file
#' @examples
#' meta <- c('Type of communication' = 'Fernsehinterview',
#'           'Source' = 'Parkinson Talkshow auf BBC',
#'           'Background information' = 'Interview mit den Beckhams',
#'           'Code' = 'Beckhams')
#'
#' speaker_data <- data.frame('id' = c('SPK0', 'SPK1', 'SPK2'),
#'                            'abbreviation' = c('PAR', 'VIC', 'DAV'),
#'                            'sex' = c('m', 'f', 'm'),
#'                            'Family: Marital status' = c('Verheiratet',
#'                                                         'Verheiratet',
#'                                                         'Verheiratet'),
#'                            'Birth' = c('28. März 1935 in Cudworth',
#'                                        '14. April 1974 in Hertfordshire',
#'                                        '2. Mail 1975 in London'),
#'                            'Occupation' = c('Fernsehmoderator, Journalist, Autor',
#'                                             'Sängerin',
#'                                             'Professioneller Fußballspieler'),
#'                            'Family: Children' = c(3, '3 Söhn, 1 Tochter', '3 Söhne, 1 Tochter'),
#'                            'Name' = c('Michael Parkinson', 'Victoria Beckham', 'David Beckham'))
#'
#' df <- exb_to_df(system.file("extdata", "demo_Beckhams.exb", package = "phonfieldwork"))
#'
#' df_to_exb(df = df,
#'           name = 'Beckhams',
#'           output_file = 'beck.xml',
#'           referenced_file = 'beck.wav',
#'           ud_meta = meta,
#'           speaker_table = speaker_data)
#'
#' # Remove file in order to pass checks
#'
#' file.remove("beck.xml")
#'
#' @export

df_to_exb <- function(df,
                      name,
                      output_file,
                      output_dir = '',
                      referenced_file='',
                      ud_meta=NULL,
                      speaker_table=NULL){

  #--- stencils

  #--- files
  files   <- paste(
    '            <referenced-file url="%s" />',
    sep='\n')

  #--- ud-meta
  udmeta <- paste(
    '                <ud-information attribute-name="%s">%s</ud-information>',
    sep='\n')

  #--- meta
  middle_meta <- paste(
    '            <ud-meta-information>',
    '%s', # ud-meta
    '            </ud-meta-information>',
    sep='\n')

  meta    <- paste(
    '        <meta-information>',
    '            <project-name></project-name>',
    '            <transcription-name>%s</transcription-name>',
    '%s', # files
    '%s', # middle_meta
    '            <comment></comment>',
    '            <transcription-convention></transcription-convention>',
    '        </meta-information>',
    sep='\n')

  #--- speaker
  sp_line <- paste(
    '                    <ud-information attribute-name=\"%s\">%s</ud-information>',
    sep='\n')

  feature_base <- paste(
    '                <ud-speaker-information>\n%s\n                </ud-speaker-information>',
    sep='\n')

  speaker  <- paste(
    '            <speaker id="%s">',
    '                <abbreviation>%s</abbreviation>',
    '                <sex value="%s" />',
    '                <languages-used></languages-used>',
    '                <l1 />',
    '                <l2 />',
    '%s', # ud-speaker-information aka feature_base
    '                <comment />',
    '            </speaker>',
    sep='\n')

  speaker_demo  <- paste(
    '            <speaker id="%s">',
    '                <abbreviation></abbreviation>',
    '                <sex value="" />',
    '                <languages-used></languages-used>',
    '                <l1 />',
    '                <l2 />',
    '                <ud-speaker-information></ud-speaker-information>',
    '                <comment />',
    '            </speaker>',
    sep='\n')

  #--- speakertable
  speakertable  <- paste(
    '        <speakertable>',
    '%s', # speaker
    '        </speakertable>',
    sep='\n')

  #--- head
  head    <- paste(
    '    <head>',
    '%s', # meta
    '%s', # speakers
    '    </head>',
    sep='\n')

  #--- timeline
  timeline <- paste(
    '        <common-timeline>',
    '%s', # tli
    '        </common-timeline>',
    sep='\n')

  tli <- paste(
    '            <tli id="%s" time="%s"/>',
    sep='\n')

  #--- tier
  tier <- paste(
    '        <tier id="%s" speaker="%s" category="%s" type="%s" display-name="%s" >',
    '%s', # events
    '        </tier>',
    sep='\n')

  #--- events
  events <- paste(
    '            <event start="%s" end="%s">%s</event>',
    sep='\n')

  #--- basic body
  basic_body    <- paste(
    '    <basic-body>',
    '%s', # timeline
    '%s', # tiers
    '    </basic-body>',
    sep='\n')

  #--- exb
  myEXB <- paste(
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
    '<!-- (c) http://www.rrz.uni-hamburg.de/exmaralda -->',
    '<basic-transcription>',
    '%s', # head
    '%s', # basic body
    '</basic-transcription>',
    sep='\n')


  #--- get columns
  wanted_columns <- c('tier', 'tier_name', 'content', 'time_start', 'time_end', 'id')

  if (!all(wanted_columns %in% colnames(df))) {
    stop(paste('Missing columns. Annotations need to contain: ', paste(wanted_columns, collapse = ", ", sep="")))
  }

  t_category <- FALSE
  t_type <- FALSE
  sp_names <- FALSE

  columns <- colnames(df)
  if ('tier_speaker' %in% columns) {
    sp_names <- TRUE
    wanted_columns <- c(wanted_columns, 'tier_speaker')
  }
  if (('tier_type' %in% columns) & (!FALSE %in% (unique(df$tier_type) %in% c('t', 'd', 'a')))) {
    t_type <- TRUE
    wanted_columns <- c(wanted_columns, 'tier_type')
  }
  if ('tier_category' %in% columns) {
    t_category <- TRUE
    wanted_columns <- c(wanted_columns, 'tier_category')
  }


  #--- create working table
  table <- df[wanted_columns]
  if (!t_type) {
    table$tier_type <- rep(c('t'), length(table$content))
  }

  if (!t_category) {
    table$tier_category <- rep(c('v'), length(table$content))
  }

  if (!sp_names) {
    table$tier_speaker <- rep(c('SPK0'), length(table$content))
  }

  #--- collect times
  if (sum(is.na(table$time_start)) + sum(is.na(table$time_end)) != 0) {
    warning(paste('Missing timestamps in rows: ', paste(which(is.na(table$time_end)), collapse = " ", sep=""), paste(which(is.na(table$time_start)), collapse = " ", sep="")))
    bad_rows <- c(which(is.na(table$time_start)), which(is.na(table$time_end)))
    table <- table[-bad_rows,]
  }

  alltimes <- c(table$time_start, table$time_end)
  alltimes <- alltimes[order(alltimes)]
  alltimes <- unique(alltimes)
  alltimes <- as.data.frame(cbind(ts=paste("T", 0:length(alltimes[-1]),sep=""),
                                  value=as.character(alltimes)),
                            stringsAsFactors=FALSE)


  #--- merge table and times
  table <- merge(table, alltimes, by.x = 'time_start', by.y='value')
  colnames(table)[which(names(table) == "ts")] <- "ts_start"
  table <- merge(table, alltimes, by.x = 'time_end', by.y='value')
  colnames(table)[which(names(table) == "ts")] <- "ts_end"


  #--- fill files
  files   <- sprintf(files, referenced_file)
  files   <- paste(files, collapse='\n')


  #--- fill meta
  if (length(ud_meta) != 0) {
    me <- sprintf(udmeta, names(ud_meta), ud_meta)
    me <- paste(me, collapse="\n")
    middle_meta <- sprintf(middle_meta, me)
    middle_meta <- paste(middle_meta, collapse='\n')
  } else {
    middle_meta <- paste(
      '            <ud-meta-information></ud-meta-information>',
      sep='\n')
  }
  meta <- sprintf(meta, name, files, middle_meta)


  #--- create list of speakers
  n <- unique(table$tier_speaker)
  n <- n[!is.na(n)]
  numberofspeakers <- max(1, length(n))-1
  speakernames <- paste("SPK", 0:numberofspeakers, sep="")

  #--- fill speakertable
  if (length(speaker_table)) {
    features <- colnames(speaker_table)
    remove <- c('id', 'abbreviation', 'sex')
    features <- setdiff(features, remove)

    l <- list()
    for (sp in speaker_table$id) {
      spd <- speaker_table[which(grepl(sp, speaker_table$id)),]

      if (length(features)) {
        feature_block <- sprintf(sp_line, features, spd[features])
        feature_block <- paste(feature_block, collapse = '\n')
        fbase <- sprintf(feature_base, feature_block)
        fbase <- paste(fbase, collapse='\n')
      } else {
        fbase <- paste(
          '                <ud-speaker-information></ud-speaker-information>',
          sep='\n')
      }

      if (length(spd$abbreviation)) {
        abbr <- spd$abbreviation
      } else {
        abbr <- c('')
      }

      if (length(spd$sex)) {
        sex <- spd$sex
      } else {
        sex <- c('')
      }

      speaker_block <- sprintf(speaker, spd$id, abbr, sex, fbase)
      l[sp] <- paste(speaker_block, collapse='\n')
    }

    speakertable <- sprintf(speakertable, paste(unlist(l), collapse="\n"))
  }
  else {
    speaker   <- sprintf(speaker_demo, speakernames)
    speaker   <- paste(speaker, collapse='\n')
    speakertable   <- sprintf(speakertable, speaker)
  }


  #--- fill head
  head  <- sprintf(head, meta, speakertable)


  #--- fill timeline
  tli <- sprintf(tli, alltimes$ts, alltimes$value)
  tli <- paste(tli, collapse="\n")
  timeline <- sprintf(timeline, tli)
  timeline <- paste(timeline, collapse="\n")


  #--- fill tiers
  tierNr <- 1
  tier_block <- list()
  ntiers <- max(table$tier)
  for (tierNr in 1:ntiers) 	{
    speaker_name <- table$tier_speaker[table$tier==tierNr]

    event_block <- sprintf(events,
                           table$ts_start[table$tier==tierNr],
                           table$ts_end[table$tier==tierNr],
                           table$content[table$tier==tierNr]
    )
    event_block <- paste(event_block, collapse="\n")

    thing <- sprintf(tier,
                     table$tier_name[table$tier==tierNr],
                     speaker_name,
                     table$tier_category[table$tier==tierNr],
                     table$tier_type[table$tier==tierNr],
                     table$tier_name[table$tier==tierNr],
                     event_block)[1]

    tier_block[tierNr] <- paste(thing, collapse="\n")
  }
  tiers <- paste(unlist(tier_block), collapse="\n")


  #--- fill basic body
  basic_body <- sprintf(basic_body, timeline, tiers)


  #--- fill exb
  myEXB <- sprintf(myEXB, head, basic_body)

  if (output_dir != '') {
    path <- normalizePath(paste(output_dir, output_file, sep = '/'))
  } else {
    output_dir <- getwd()
    path <- normalizePath(paste(output_dir, output_file, sep = '/'))
  }

  fileConn <- file(path, open="wb")
  writeBin(charToRaw(myEXB), fileConn, endian="little")
  close(fileConn)
}
