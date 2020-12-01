rm(list=ls())
library(parallel)
library(DEoptim)

load("../results/posterior_ETIs.RData")
load("../data/nba20192020.rda")

ETIs$date <- as.Date(sapply(results, function(q) attributes(q)$date), format="%B %e, %Y")
ETIs$home <- as.factor(sapply(results, function(q) attributes(q)$home))
ETIs$away <- as.factor(sapply(results, function(q) attributes(q)$away))

teamData <- rbind(data.frame(ETI = ETIs$median, team = ETIs$home),
                  data.frame(ETI = ETIs$median, team = ETIs$away))

LOOCV <- function(data, formula) {
  model <- glm(formula(formula), data = data)
  sqrt(mean(((data$ETI - predict(model))/(1 - boot::glm.diag(model)$h))^2))
}

cl <- makeForkCluster(64)

estPartitions <- function(nPart) {
  ctl <- DEoptim.control(itermax = 5000, trace = 10, cluster = cl)
  opt <- DEoptim(function(x) {
  	lev <- floor(x)
	  teamData$relevel <- teamData$team
	  levels(teamData$relevel) <- lev
	  
	  LOOCV(teamData, "ETI ~ relevel")
  }, lower = c(rep(0, 30)), upper = c(rep(nPart + 1 - 0.001, 30)), control = ctl)
  opt
}

set.seed(87374674)
part_2 <- estPartitions(1)
part_3 <- estPartitions(2)
part_4 <- estPartitions(3)
part_5 <- estPartitions(4)
part_6 <- estPartitions(5)
part_7 <- estPartitions(6)
part_8 <- estPartitions(7)

stopCluster(cl)

save(part_2, part_3, part_4, part_5, 
     part_6, part_7, part_8, teamData,
     file="../results/teams_grouped_by_ETI.RData")


######################################################
c(part_2$optim$bestval, part_3$optim$bestval,
  part_4$optim$bestval, part_5$optim$bestval,
  part_6$optim$bestval, part_7$optim$bestval,
  part_8$optim$bestval)

table(as.vector(floor(part_4$optim$bestmem)))
