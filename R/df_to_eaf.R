#' Dataframe to .eaf
#'
#' Convert a dataframe to Elan file .exb
#'
#' @author Sergej Kudrjashov <xenomirant@gmail.com>
#'
#' @param df an R dataframe object that contains columns named 'tier', 'id', 'tier_name',
#' 'content', 'time_start', 'time_end' and preferably also 'tier_type', 'stereotype',
#' 'tier_ref', 'event_local_id', 'dependent_on' that are specific for eaf file
#' @param output_file the name of the result .xml file
#' @param output_dir the output directory for the rendered file (defalut is used if not spectified)
#' @param ref_file a filepath for connected media file (not obligatory)
#' @param mime_type a MIME type of connected media file (not obligatory)
#' @return .xml file
#' @examples
#'
#' df <- eaf_to_df(system.file("extdata", "test.eaf", package = "phonfieldwork"))
#'
#' df_to_eaf(df = df,
#'           output_file = 'test.eaf',
#'           ref_file = 'test.wav')
#'
#' @importFrom mime guess_type
#' @importFrom stats na.omit
#' @export
#'

df_to_eaf <- function(df, output_file, output_dir = '', ref_file = '', mime_type = '') {

  #--- main body (maybe add date)
  my_eaf <- paste(
    '<?xml version="1.0" encoding=\"UTF-8\"?>',
    '<ANNOTATION_DOCUMENT AUTHOR=\"\" DATE=\"2022-04-21T20:19:16+03:00\"',  #here
        '\tFORMAT=\"3.0\" VERSION=\"3.0\"',
        '\txmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:noNamespaceSchemaLocation=\"http://www.mpi.nl/tools/elan/EAFv3.0.xsd\">',
      '%s',  #header
      '%s',  #time slots
      '%s',  #tiers
      '%s',  #tier types
      '\t<CONSTRAINT',
      '\t\tDESCRIPTION=\"Time subdivision of parent annotation\'s time interval, no time gaps allowed within this interval\" STEREOTYPE=\"Time_Subdivision\"/>',
        '\t<CONSTRAINT',
      '\t\tDESCRIPTION=\"Symbolic subdivision of a parent annotation. Annotations refering to the same parent are ordered\" STEREOTYPE=\"Symbolic_Subdivision\"/>',
        '\t<CONSTRAINT ',
      '\t\tDESCRIPTION=\"1-1 association with a parent annotation\" STEREOTYPE=\"Symbolic_Association\"/>',
        '\t<CONSTRAINT',
      '\t\tDESCRIPTION=\"Time alignable annotations within the parent annotation\'s time interval, gaps are allowed\" STEREOTYPE=\"Included_In\"/>',
    '</ANNOTATION_DOCUMENT>',
  sep = '\n')


  #--- header
  header <- paste(
    '\t<HEADER MEDIA_FILE=\"\" TIME_UNITS=\"milliseconds\">',
          '\t\t<MEDIA_DESCRIPTOR',
              '\t\t\tMEDIA_URL="%s"',
              '\t\t\tMIME_TYPE="%s" RELATIVE_MEDIA_URL="%s"/>',
          '\t\t<PROPERTY NAME=\"URN\">urn:nl-mpi-tools-elan-eaf:e7d15769-9e52-4663-aa66-5033ddad8142</PROPERTY>',
          '\t\t<PROPERTY NAME=\"lastUsedAnnotationId\"></PROPERTY>',
      '\t</HEADER>',
  sep = '\n')

  header_v2 <- paste(
    '\t<HEADER MEDIA_FILE=\"\" TIME_UNITS=\"milliseconds\">',
    '\t\t<PROPERTY NAME=\"URN\">urn:nl-mpi-tools-elan-eaf:e7d15769-9e52-4663-aa66-5033ddad8142</PROPERTY>',
    '\t\t<PROPERTY NAME=\"lastUsedAnnotationId\"></PROPERTY>',
    '\t</HEADER>',
    sep = '\n')

  #--- time order
  time_slots <- paste(
    '\t<TIME_ORDER>',
    '%s',
    '\t</TIME_ORDER>',
  sep = '\n')

  slot <- paste(
    '\t\t<TIME_SLOT TIME_SLOT_ID="%s" TIME_VALUE="%s"/>',
  sep = '\n')

  #--- tiers
  indep_tier <- paste(
    '\t<TIER LINGUISTIC_TYPE_REF="%s"',
      '\t\tTIER_ID="%s">',
    "%s",  #annotations
    '\t</TIER>',
  sep = '\n')

  dep_tier <- paste(
    '\t<TIER LINGUISTIC_TYPE_REF="%s" PARENT_REF="%s"',
      '\t\tTIER_ID="%s">',
    "%s",  #annotations
    '\t</TIER>',
    sep = '\n')

  #--- annotations
  align_annotation <- paste(
    '\t\t<ANNOTATION>',
        '\t\t\t<ALIGNABLE_ANNOTATION ANNOTATION_ID="%s"',
          '\t\t\t\tTIME_SLOT_REF1="%s" TIME_SLOT_REF2="%s">',
          '\t\t\t\t<ANNOTATION_VALUE>%s</ANNOTATION_VALUE>',
        '\t\t\t</ALIGNABLE_ANNOTATION>',
      '\t\t</ANNOTATION>',
  sep = '\n')

  ref_annotation <- paste(
    '\t\t<ANNOTATION>',
      '\t\t\t<REF_ANNOTATION ANNOTATION_ID="%s" ANNOTATION_REF="%s">',
        '\t\t\t\t<ANNOTATION_VALUE>%s</ANNOTATION_VALUE>',
      '\t\t\t</REF_ANNOTATION>',
    '\t\t</ANNOTATION>',
  sep = '\n')

  spec_ref_annotation <- paste(
    '\t\t<ANNOTATION>',
      '\t\t\t<REF_ANNOTATION ANNOTATION_ID="%s" ANNOTATION_REF="%s" PREVIOUS_ANNOTATION="%s">' ,  #for symb.subdivision layer > 1
        '\t\t\t\t<ANNOTATION_VALUE>%s</ANNOTATION_VALUE>',
      '\t\t\t</REF_ANNOTATION>',
    '\t\t</ANNOTATION>',
  sep = '\n')

  #--- linguistic types
  indep_type <- paste(
    '\t<LINGUISTIC_TYPE GRAPHIC_REFERENCES=\"false\"',
          '\t\tLINGUISTIC_TYPE_ID="%s" TIME_ALIGNABLE=\"true\"/>',
  sep = '\n')

  dep_type <- paste(
    '\t<LINGUISTIC_TYPE CONSTRAINTS="%s"',
        '\t\tGRAPHIC_REFERENCES=\"false\" LINGUISTIC_TYPE_ID="%s" TIME_ALIGNABLE="%s"/>',
  sep = '\n')

  if (is.na(ref_file)) {
    ref_file = ''
  }

  #--- get columns
  wanted_columns <- c('tier', 'id', 'content', 'tier_name', 'time_start', 'time_end')

  if (!all(wanted_columns %in% colnames(df))) {

    stop(paste('Missing columns. Annotations need to contain: ', paste(wanted_columns, collapse = ", ", sep="")))

    }

  bool_tier_type <- FALSE
  bool_tier_stereotype <- FALSE
  bool_tier_ref <- FALSE
  bool_tier_event_id <- FALSE
  bool_dependent_on <- FALSE

  columns <- colnames(df)
  if ('tier_type' %in% columns) {
    bool_tier_type <- TRUE
    wanted_columns <- c(wanted_columns, 'tier_type')
  }

  if ('stereotype' %in% columns) {
    if (!FALSE %in% (unique(df$stereotype) %in% c(NA, "Symbolic_Subdivision",
        "Symbolic_Association", "Included_In", "Time_Subdivision"))) {
    bool_tier_stereotype<- TRUE
    wanted_columns <- c(wanted_columns, 'stereotype')
    } else {
      stop(paste('Unknown stereotype. Make sure the stereotype is one of: ',
                 paste(NA, "Symbolic_Subdivision",
                  "Symbolic_Association", "Included_In", "Time_Subdivision", sep = ', ')))
    }
  }

  if ('tier_ref' %in% columns) {
    if (!FALSE %in% unique(stats::na.omit(df$tier_ref)) %in% df$tier_name) {
    bool_tier_ref <- TRUE
    wanted_columns <- c(wanted_columns, 'tier_ref')
    } else {
      stop(paste('Some dependent tiers match none of local ids: ',
                 paste(unique(stats::na.omit(df$tier_ref)[!stats::na.omit(df$tier_ref) %in% df$tier_name]), collapse = ', ')))
    }
  }

  if ('event_local_id' %in% columns) {
    bool_tier_event_id<- TRUE
    wanted_columns <- c(wanted_columns, 'event_local_id')

    if ('dependent_on' %in% columns) {
      if (!FALSE %in% unique(stats::na.omit(df$dependent_on)) %in% df$event_local_id) {
        bool_dependent_on <- TRUE
        wanted_columns <- c(wanted_columns, 'dependent_on')
      } else {
        stop(paste('Some dependent events match none of local ids: ',
                   paste(stats::na.omit(df$dependent_on)[!stats::na.omit(df$dependent_on) %in% df$event_local_id], collapse = ', ')))
      }
    }
  }

  if (ref_file == '') {
    if (is.na(attributes(df)$MEDIA_URL)) {
      warning(paste('MEDIA_URL not specialized. Writing with no file connected'))
      relative_ref_file <- ''
    } else {
      ref_file <- attributes(df)$MEDIA_URL
      relative_ref_file <- paste0('./',gsub(".+/", "", ref_file))
    }
  } else {
    ref_file <- paste0('file:///', ref_file)
    relative_ref_file <- paste0('./',gsub(".+/", "", ref_file))
  }

  #--- external package function used
  if (mime_type == '') {
    if (is.null(attributes(df)$MIME_TYPE)) {
      warning(paste('MIME_TYPE not specialized. This may cause some problems connecting mediafile'))
      if (ref_file != '') {
        paste('Guessing MIME TYPE based on provided media file...')
        mime_type <- mime::guess_type(ref_file)
      }
    } else {
      mime_type <- attributes(df)$MIME_TYPE
    }
  }

  #--- create table for endfile
  table <- df[wanted_columns]

  if (!bool_tier_stereotype & !bool_tier_type & bool_tier_ref){
    stop(paste('Tier types and stereotypes specification is needed in order to write.'))
  }

  if (!bool_tier_type) {
    warning(paste('Tier types not specified. Writing with independent default tier values'))
    table$tier_type <- rep(c('default'), length(table$content))
    table$stereotype <- rep(NA, length(table$content))
    table$tier_ref <- rep(NA, length(table$content))
    table$event_local_id <- rep(NA, length(table$content))
    table$dependent_on <- rep(NA, length(table$content))
  }

  if (!bool_tier_stereotype){
    warning(paste('Tier stereotypes not specified. Writing as independent tiers'))
    table$stereotype <- rep(NA, length(table$content))
    table$tier_ref <- rep(NA, length(table$content))
    table$event_local_id <- rep(NA, length(table$content))
    table$dependent_on <- rep(NA, length(table$content))
  }

  #--- collect times
  if (sum(is.na(table$time_start)) + sum(is.na(table$time_end)) != 0) {
    warning(paste('Missing timestamps in rows: ',
                  paste(which(is.na(table$time_end)), collapse = " ", sep=""),
                  paste(which(is.na(table$time_start)), collapse = " ", sep=""),
                  'They will be skipped.'))
    bad_rows <- c(which(is.na(table$time_start)), which(is.na(table$time_end)))
    table <- table[-bad_rows,]
  }



  #--- getting rid of irrelevant timecodes
  time_table <- subset(table, subset = table$stereotype %in% c(NA, 'Included_In'))
  allTimes <- c(time_table$time_start, time_table$time_end)

  time_table <- subset(table, subset = table$stereotype %in% c('Time_Subdivision'))
  allTimes_Subdiv <- c(time_table$time_start, time_table$time_end)
  allTimes <- c(allTimes, setdiff(allTimes_Subdiv, allTimes))
  allTimes <- allTimes[order(allTimes)]
  allTimes <- as.data.frame(cbind(ts=paste("ts", 1:length(allTimes),sep=""),
                                  value=as.character(allTimes)),
                            stringsAsFactors=FALSE)

  #--- acquiring symbolic connections
  symb_connections <- sapply(row.names(table[which(table$stereotype %in%
                        c('Symbolic_Subdivision', 'Symbolic_Association')), ]), function(i){
      event_ref <- table[i, 11]
      row.names(table[which(table$event_local_id == event_ref), ])
                        })

  #--- setting up new symbolic connections
  new_event_codes <- c(paste0('a', seq_along(along.with = row.names(table))))
  table$event_local_id <- new_event_codes
  loc_table <- table[which(table$stereotype %in%
                             c('Symbolic_Subdivision', 'Symbolic_Association')), ]
  ref_event <- sapply(row.names(loc_table), function(i) {
    row_ref <- symb_connections[i]
    table[row_ref, 10]
  })
  loc_table$dependent_on <- ref_event
  table[row.names(table) %in% row.names(loc_table), ] <- loc_table

  #--- acquiring tier_type stereotypes
  stereotable <- table[c('tier_type', 'stereotype')]
  stereotable <- unique(stereotable)


  #--- fill head
  if (ref_file == '') {
    header <- sprintf(header_v2)
  } else {
    header <- sprintf(header, ref_file, mime_type, relative_ref_file)
  }


  #--- fill timecodes
  slot <- sprintf(slot, allTimes$ts, format(as.numeric(allTimes$value)*1000, scientific = FALSE, trim = TRUE))
  slot <- paste(slot, collapse="\n")
  time_slots <- sprintf(time_slots, slot)
  time_slots <- paste(time_slots, collapse="\n")

  #--- fill tiers
  tiers_vec <- c(rep_len(NA, length.out = length(unique(table$tier))))
  table$ts_start <- NA
  table$ts_end <- NA
  table <- rbind(subset(table, subset = !(table$stereotype %in% c('Time_Subdivision'))),
                 subset(table, subset = (table$stereotype %in% c('Time_Subdivision'))))

  cur.env <- new.env()
  cur.env$allTimes <- allTimes
  cur.env$table <- table
  cur.env$tiers_vec <- tiers_vec
  cur.env$prev_symb_value <- ''

  tiers <- sapply(unique(cur.env$table$tier), function(i) {
    cur_tier <- cur.env$table[which(cur.env$table$tier == i), ]

    #--- indep tiers
    if (is.na(cur_tier[1, ]$stereotype)) {

      tier <- sapply(order(cur_tier$id), function(j) {
        cur <- cur_tier[which(cur_tier$id == j), ]
        ts_start <- cur.env$allTimes[which(cur.env$allTimes$value == cur$time_start), 1][1]
        ts_end <- cur.env$allTimes[which(cur.env$allTimes$value == cur$time_end), 1][1]
        cur.env$allTimes <- cur.env$allTimes[-c(which(cur.env$allTimes$ts %in% c(ts_start, ts_end))), ]
        cur$ts_start <- ts_start
        cur$ts_end <- ts_end
        cur.env$table[row.names(cur.env$table) %in% row.names(cur), ] <- cur
        sprintf(align_annotation, cur$event_local_id, cur$ts_start, cur$ts_end, cur$content)
      })
      tier <- paste(tier, collapse = '\n')

      tier_paste <- sprintf(indep_tier, cur_tier[1, ]$tier_type, cur_tier[1, ]$tier_name,
                            tier)
      cur.env$tiers_vec[cur_tier[1, ]$tier] <- tier_paste
    }

    #--- Included in stereotype
    if (cur_tier[1, ]$stereotype %in% c('Included_In')) {

      tier <- sapply(order(cur_tier$id), function(j) {
        cur <- cur_tier[which(cur_tier$id == j), ]
        ts_start <- cur.env$allTimes[which(cur.env$allTimes$value == cur$time_start), 1][1]
        ts_end <- cur.env$allTimes[which(cur.env$allTimes$value == cur$time_end), 1][1]
        cur.env$allTimes <- cur.env$allTimes[-c(which(cur.env$allTimes$ts %in% c(ts_start, ts_end))), ]
        cur$ts_start <- ts_start
        cur$ts_end <- ts_end
        cur.env$table[row.names(cur.env$table) %in% row.names(cur), ] <- cur
        sprintf(align_annotation, cur$event_local_id, cur$ts_start, cur$ts_end, cur$content)
      })
      tier <- paste(tier, collapse = '\n')

      tier_paste <- sprintf(dep_tier, cur_tier[1, ]$tier_type, cur_tier[1, ]$tier_ref,
                            cur_tier[1, ]$tier_name, tier)
      cur.env$tiers_vec[cur_tier[1, ]$tier] <- tier_paste
    }

    #--- Time Subdiv. stereotype
    if (cur_tier[1, ]$stereotype %in% c('Time_Subdivision')) {

      refer_tier <- cur.env$table[which(cur.env$table$tier_name == cur_tier[1, ]$tier_ref), ]

      tier <- sapply(order(cur_tier$id), function(j) {
        cur <- cur_tier[which(cur_tier$id == j), ]

        if (!cur$time_start %in% cur.env$allTimes$value) {
          ts_start <- refer_tier[which(refer_tier$time_start == cur$time_start), ]$ts_start
        } else {
          ts_start <- cur.env$allTimes[which(cur.env$allTimes$value == cur$time_start), 1][1]
        }

        if (!cur$time_end %in% cur.env$allTimes$value) {
          ts_end <- refer_tier[which(refer_tier$time_end == cur$time_end), ]$ts_end
        } else {
          ts_end <- cur.env$allTimes[which(cur.env$allTimes$value == cur$time_end), 1][1]
        }

        cur$ts_start <- ts_start
        cur$ts_end <- ts_end

        cur.env$table[row.names(cur.env$table) %in% row.names(cur), ] <- cur
        sprintf(align_annotation, cur$event_local_id, cur$ts_start, cur$ts_end, cur$content)
      })
      tier <- paste(tier, collapse = '\n')

      tier_paste <- sprintf(dep_tier, cur_tier[1, ]$tier_type, cur_tier[1, ]$tier_ref,
                            cur_tier[1, ]$tier_name, tier)
      cur.env$tiers_vec[cur_tier[1, ]$tier] <- tier_paste
    }

    #--- Symb. Assoc. Stereotype
    if (cur_tier[1, ]$stereotype %in% c('Symbolic_Association')) {

      tier <- sapply(order(cur_tier$id), function(j) {
        cur <- cur_tier[which(cur_tier$id == j), ]

        sprintf(ref_annotation, cur$event_local_id, cur$dependent_on, cur$content)

      })
      tier <- paste(tier, collapse = '\n')

      tier_paste <- sprintf(dep_tier, cur_tier[1, ]$tier_type, cur_tier[1, ]$tier_ref,
                            cur_tier[1, ]$tier_name, tier)
      cur.env$tiers_vec[cur_tier[1, ]$tier] <- tier_paste
    }

    #--- Symb. Subdivision Stereotype
    if (cur_tier[1, ]$stereotype %in% c('Symbolic_Subdivision')) {

      cur.env$cur_dep = ''

      tier <- sapply(order(cur_tier$id), function(j) {
        cur <- cur_tier[which(cur_tier$id == j), ]

        if (j == 1) {
          loc <- sprintf(ref_annotation, cur$event_local_id, cur$dependent_on, cur$content)
          cur.env$prev_symb_value <- cur$event_local_id
          cur.env$cur_dep <- cur$dependent_on
        } else {
          if (cur.env$cur_dep != cur$dependent_on) {
            loc <- sprintf(ref_annotation, cur$event_local_id, cur$dependent_on, cur$content)
            cur.env$prev_symb_value <- cur$event_local_id
            cur.env$cur_dep <- cur$dependent_on
          } else {
          loc <- sprintf(spec_ref_annotation, cur$event_local_id, cur$dependent_on,
                  cur.env$prev_symb_value, cur$content)
          cur.env$prev_symb_value <- cur$event_local_id
          cur.env$cur_dep <- cur$dependent_on
          }
        }
        loc
      })
      tier <- paste(tier, collapse = '\n')

      tier_paste <- sprintf(dep_tier, cur_tier[1, ]$tier_type, cur_tier[1, ]$tier_ref,
                            cur_tier[1, ]$tier_name, tier)
      cur.env$tiers_vec[cur_tier[1, ]$tier] <- tier_paste
    }

  })

  tiers <- paste(cur.env$tiers_vec, collapse = '\n')


  #--- fill tier type info
  ling_type <- sapply(row.names(stereotable), function(i){
    time_alignable <- 'true'
    if (is.na(stereotable[i, 2])) {
      sprintf(indep_type, stereotable[i, 1])
    } else {
      if (stereotable[i, 2] %in% c('Symbolic_Subdivision', 'Symbolic_Association')) {
        time_alignable <- 'false'
      }
      sprintf(dep_type, stereotable[i, 2], stereotable[i, 1], time_alignable)
    }
  })
  ling_type <- paste(ling_type, collapse = '\n')

  #---fill the main body
  myEAF <- sprintf(my_eaf, header, time_slots, tiers, ling_type)

  if (output_dir != '') {
    path <- normalizePath(paste(output_dir, output_file, sep = '/'))
  } else {
    output_dir <- getwd()
    path <- normalizePath(paste(output_dir, output_file, sep = '/'))
  }

  fileConn <- file(path, open="wb")
  writeBin(charToRaw(myEAF), fileConn, endian="little")
  close(fileConn)
}
