# merge video -------------------------------------------------------------
setwd("/home/agricolamz/Desktop/test")

tmp <- tempfile(fileext = ".txt")
writeLines(paste0("file '",
       normalizePath(list.files(pattern = "(\\.mp4)|(\\.MP4)|(\\.avi)|(\\.AVI)")),
       "'"), tmp)
output_file <- "output.mp4"
system(paste0("ffmpeg -f concat -safe 0 -i ", tmp, " -c copy ", output_file))
