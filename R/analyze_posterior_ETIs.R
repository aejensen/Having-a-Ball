rm(list=ls())
library(moments)
library(mclust)

load("results/est_ETIs.RData")
load("data/nba20192020.rda")

ETIs$date <- as.Date(sapply(results, function(q) attributes(q)$date), format="%B %e, %Y")
ETIs$home <- as.factor(sapply(results, function(q) attributes(q)$home))
ETIs$away <- as.factor(sapply(results, function(q) attributes(q)$away))

# Summary statistics
mean(ETIs$median)
sd(ETIs$median)
skewness(ETIs$median)
median(ETIs$median)
quantile(ETIs$median)

# Get team-specific average ETIs during season
team_averages <- rbind(data.frame(team = ETIs$home, ETI = ETIs$median),
                       data.frame(team = ETIs$away, ETI = ETIs$median))
team_summary <- t(sapply(split(team_averages, team_averages$team), function(q) {
  c(mean = mean(q$ETI), sd = sd(q$ETI))
}))
round(team_summary[order(team_summary[,"mean"], decreasing=TRUE),], 2)


# Fit normal mixture for marginal median ETIs at the match level
set.seed(12345)
nClust <- mclustBootstrapLRT(ETIs[,"median"], modelName = "V", nboot = 10 * 10^3)
nClust

mix_fit <- Mclust(ETIs[,"median"], G = 4, modelNames="V")
summary(mix_fit, parameters=TRUE)

lambda <- summary(mix_fit, parameters=TRUE)$pro
mu <- summary(mix_fit, parameters=TRUE)$mean
sigma2 <- summary(mix_fit, parameters=TRUE)$variance

mixture_parameters <- rbind(lambda, mu, sigma2)
colnames(mixture_parameters) <- paste("class", 1:4)
knitr::kable(round(mixture_parameters, 2))

save(nClust, lambda, mu, sigma2, file = "results/ETI_mixture_density.RData")
