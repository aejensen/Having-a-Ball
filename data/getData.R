library("rvest")
library("tidyverse")

mineData <- function(url) {
  raw_data <- url %>% read_html() %>% html_table() %>% as.data.frame()
  names(raw_data) <- c("time", "text", "change", "score", "change2", "text2")

  #Need to fix times and scores
  #First we try to fix time
  val1 <- as.numeric(raw_data$change) 
  val2 <- as.numeric(raw_data$change2)

  keeptimes <- ((!is.na(val1)) | (!is.na(val2)))

  scorediff <- cumsum(ifelse(is.na(val1), 0, val1)) - cumsum(ifelse(is.na(val2), 0, val2))
  scorediff <- scorediff[keeptimes]
  
  splitl <- stringr::str_split(raw_data$time, ":")
  for (i in 1:length(splitl)) {
    splitl[[i]]
  }

  part1 <- sapply(stringr::str_split(raw_data$time, ":"), function(i) {
    ifelse(length(i)==1, NA, 12-as.numeric(i[1]) -
    			 	as.numeric(stringr::str_split(i[2], "\\.")[[1]][1])/60 -
    			 	as.numeric(stringr::str_split(i[2], "\\.")[[1]][2])/60/100)
  })

  # Now convert quarters
  quarter <- 0
  part2 <- part1
  for (i in 1:length(part1)) {
    if (!is.na(part2[i]) & part2[i]==0) {
      quarter <- quarter +1
    }
    part2[i] <- part1[i]+12*(quarter-1)
  }
  part1 <- part1[keeptimes]
   
  data.frame(time=part2[keeptimes], scorediff)
}

url <- "https://www.basketball-reference.com/boxscores/pbp/202010090LAL.html"

d <- mineData(url)
#plot(d$time, d$scorediff)

save(d, file = "data.RData")