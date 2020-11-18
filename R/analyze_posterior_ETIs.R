rm(list=ls())
library(moments)
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


# Cluster the ETIs on match level
library(mclust)
set.seed(12345)
nClust <- mclustBootstrapLRT(ETIs[,"median"], modelName = "V", nboot = 10 * 10^3, maxG=5)
nClust
mix <- Mclust(ETIs[,"median"], G = 4, modelNames="V")
summary(mix, parameters=TRUE)
plot(mix, what = "density", breaks = 40)

posterior <- summary(mix, parameters=TRUE)$classification
all(apply(mix$z, 1, which.max) == posterior)
table(posterior)

lambda <- summary(mix, parameters=TRUE)$pro
mu <- summary(mix, parameters=TRUE)$mean
sigma2 <- summary(mix, parameters=TRUE)$variance

parMat <- rbind(lambda, mu, sigma2)
colnames(parMat) <- paste("class", 1:4)
knitr::kable(round(parMat, 2))

mixDensEst <- function(x) {
  lambda[1]*dnorm(x, mu[1], sqrt(sigma2[1])) + 
  lambda[2]*dnorm(x, mu[2], sqrt(sigma2[2])) + 
  lambda[3]*dnorm(x, mu[3], sqrt(sigma2[3])) +
  lambda[4]*dnorm(x, mu[4], sqrt(sigma2[4]))
}

#####
pdf("figures/fig2-5.pdf", width = 8, height = 3)

par(mfrow=c(1,2), bty="n", mar = c(2.7, 2.7, 1, 0), mgp=c(1.6,0.4,0), 
    bty="n", cex.axis=0.9, cex.lab=1, cex.main=1)

hist(ETIs[,"median"], xlim=c(0,28), prob=TRUE, xaxt="n", xlab="Median posterior ETI", main="", yaxt="n", ylim=c(0,0.12))
title("ETI mixture model", font.main=2)
axis(1, seq(0, 28, length.out=8))
axis(2, seq(0, 0.12, length.out=4))
curve(mixDensEst(x), 0, 28, add=TRUE, lwd=3)

curve(lambda[1]*dnorm(x, mu[1], sqrt(sigma2[1])), 0, 28, add=TRUE, lty=1, lwd=1)
curve(lambda[2]*dnorm(x, mu[2], sqrt(sigma2[2])), 0, 28, add=TRUE, lty=1, lwd=1)
curve(lambda[3]*dnorm(x, mu[3], sqrt(sigma2[3])), 0, 28, add=TRUE, lty=1, lwd=1)
curve(lambda[4]*dnorm(x, mu[4], sqrt(sigma2[4])), 0, 28, add=TRUE, lty=1, lwd=1)

barplot(table(posterior) / nrow(ETIs) * 100, ylim=c(0,35),
				names.arg=paste("Class", 1:4), ylab="Proportion of matches [%]",
				main="Latent class", xlab="Maximum posterior classification")

dev.off()

ETIs$class <- as.numeric(posterior) - 1
ETIs$classBin <- ifelse(ETIs$class >= 2, 1, 0)
table(ETIs$classBin)
summary(glm(classBin ~ home, data = ETIs, family=binomial))
