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


#Miami Heat at Los Angeles Lakers Play-By-Play, October 2, 2020
d1 <- mineData("https://www.basketball-reference.com/boxscores/pbp/202010020LAL.html")
#Los Angeles Lakers at Miami Heat Play-By-Play, October 4, 2020
d2 <- mineData("https://www.basketball-reference.com/boxscores/pbp/202010040MIA.html")
#Los Angeles Lakers at Miami Heat Play-By-Play, October 6, 2020
d3 <- mineData("https://www.basketball-reference.com/boxscores/pbp/202010060MIA.html")
#Miami Heat at Los Angeles Lakers Play-By-Play, October 9, 2020
d4 <- mineData("https://www.basketball-reference.com/boxscores/pbp/202010090LAL.html")
#Los Angeles Lakers at Miami Heat Play-By-Play, October 11, 2020
d5 <- mineData("https://www.basketball-reference.com/boxscores/pbp/202010110MIA.html")


par(mfrow=c(3,2))
plot(d1$time, d1$scorediff, type="s")
plot(d2$time, d2$scorediff, type="s")
plot(d3$time, d3$scorediff, type="s")
plot(d4$time, d4$scorediff, type="s")
plot(d5$time, d5$scorediff, type="s")

range(d5$time)
d5

save(d1, d2, d3, d4, d5, file="test.RData")
