#!/usr/bin/env -S r -i 


# Check and print args ----------------------------------------------------

if (is.null(argv) | length(argv) != 2) {
  cat("Usage: mkvsubs.r MKVFILE LANGUAGE\n")
  q()
}

mkvfile <- argv[1]
language <- argv[2]

cat('\nFile    : ', mkvfile, '\n')
cat('Language: ', language, '\n\n')


# Call mkvinfo ------------------------------------------------------------

info <- system2('mkvinfo', mkvfile, stdout = TRUE)


# Filter track info -------------------------------------------------------

pattern <- paste0(
  '\\+ (',
  '(Track number: )|',
  '(Name: )|',
  '(Language: )|',
  '(Track type: )',
  ').*$'
)

matches <- grep(
  pattern,
  info
)

info <- info[matches]


# Build df ----------------------------------------------------------------

track_no <- vector('numeric')
track_name <- vector('character')
track_lang <- vector('character')
track_type <- vector('character')

i <- 1
j <- 0

while (i <= length(info)) {
  
  # Look for track number
  found_trackno <- regexpr(
    '\\+ Track number: .*: ([[:digit:]]+)\\)$',
    info[i],
    perl = TRUE
  )
  
  # If not found, try next line
  if (found_trackno == -1) {
    i <- i + 1
    next
  }
  
  # Found track number: save
  j <- j + 1
  start <- attr(found_trackno, 'capture.start')[1]
  stop = start + attr(found_trackno, 'capture.length')[1] - 1
  track_no[j] <- substr(info[i], start, stop)

  # Must find name
  i <- i + 1
  found_name <- regexpr(
    '\\+ Name: (.+)',
    info[i],
    perl = TRUE
  )
  
  # If not found, error
  if (found_name == -1) {
    stop('Name not found for track ', track_no[j])
  }
  
  # Found name: save
  start <- attr(found_name, 'capture.start')[1]
  stop = start + attr(found_name, 'capture.length')[1] - 1
  track_name[j] <- substr(info[i], start, stop)

  # Must find language
  i <- i + 1
  found_lang <- regexpr(
    '\\+ Language: (.+)',
    info[i],
    perl = TRUE
  )
  
  # If not found, error
  if (found_lang == -1) {
    stop('Language not found for track ', track_no[j])
  }
  
  # Found language: save
  start <- attr(found_lang, 'capture.start')[1]
  stop = start + attr(found_lang, 'capture.length')[1] - 1
  track_lang[j] <- substr(info[i], start, stop)
  
  # Must find type
  i <- i + 1
  found_type <- regexpr(
    '\\+ Track type: (.+)',
    info[i],
    perl = TRUE
  )
  
  # If not found, error
  if (found_type == -1) {
    stop('Type not found for track ', track_no[j])
  }
  
  # Found type: save
  start <- attr(found_type, 'capture.start')[1]
  stop = start + attr(found_type, 'capture.length')[1] - 1
  track_type[j] <- substr(info[i], start, stop)
  
  # Next block
  i <- i + 1
  
}

attributes(track_no) <- NULL
attributes(track_name) <- NULL
attributes(track_lang) <- NULL
attributes(track_type) <- NULL

df <- data.frame(
  track_no,
  track_name,
  track_lang,
  track_type
)


# Filter language ---------------------------------------------------------

found <- subset(df, track_lang == language & track_type == 'subtitles')


# If no matches, error ----------------------------------------------------

if (nrow(found) == 0)
  stop('No subtitle tracks for "', language, '" language found.')


# If more than one match, user chooses ------------------------------------

# Choose the first (hopefully the only) track
choice <-  1

# If more than one, show menu
if (nrow(found) > 1) {
  
  choice <- menu(
    found$track_name, title = 'Several tracks found. Choose one:'
  )
  
  if (choice == 0) {
    stop('No track selected.')
  }
  
}


# Assemble and run command ------------------------------------------------

srt_name <- paste0(
  substr(mkvfile, start = 1, stop = nchar(mkvfile) - 4),
  '.srt'
)

track_spec <- paste0(
  found[choice, 'track_no'],
  ':',
  srt_name
)

if (file.exists(srt_name)) {
  
  overwrite <- menu(
    choices = c('Yes', 'No'),
    title = paste0(
      '\nFile ',
      srt_name, 
      ' exists.\nOverwrite?'
    )
  )
  
  if (overwrite != 1)
    stop('Cancelled.')
  
}

system2(
  'mkvextract',
  c(
    mkvfile,
    'tracks',
    track_spec
  )
)
