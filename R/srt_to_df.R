

file_name = "/home/agricolamz/Desktop/test.srt"

srt_to_df <- function(file_name){
  # after https://stackoverflow.com/a/36532461/6056442
  srt <- readLines(file_name, encoding = "unknown", warn = FALSE)
  s <- split(1:length(srt), cumsum(grepl("^\\s*$",srt)))
  lapply(split(1:length(srt), cumsum(grepl("^\\s*$",srt))), function(i){
    block <- srt[i]
    block <- block[!grepl("^\\s*$", block)]
    if(length(block) == 0){
      return(NULL)
    }
    if(length(block) < 3){
      warning("There are some non-standard blocks in .srt file")
    }
    return(data.frame(id=block[1],
                      times=block[2],
                      content=paste0(block[3:length(block)],collapse="\n")))
  }) ->
    result

  result <- do.call(rbind,result)
  result <- cbind(result,
                  do.call(rbind, strsplit(result[,'times'],' --> ')))
  names(result)[4:5] <- c("time_start", "time_end")
  result <- result[,-2]
}
