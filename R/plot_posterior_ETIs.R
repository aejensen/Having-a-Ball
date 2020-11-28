rm(list=ls())
load("../data/nba20192020.rda")
load("../results/posterior_ETIs.RData")
load("../results/posterior_ETIs_mixture_density.RData")

ETIs$date <- as.Date(sapply(results, function(q) attributes(q)$date), format="%B %e, %Y")
ETIs$home <- as.factor(sapply(results, function(q) attributes(q)$home))
ETIs$away <- as.factor(sapply(results, function(q) attributes(q)$away))

mixtureDensity <- function(x) {
  lambda[1]*dnorm(x, mu[1], sqrt(sigma2[1])) + 
  lambda[2]*dnorm(x, mu[2], sqrt(sigma2[2])) + 
  lambda[3]*dnorm(x, mu[3], sqrt(sigma2[3])) +
  lambda[4]*dnorm(x, mu[4], sqrt(sigma2[4]))
}

pdf("../figures/fig2.pdf", width = 8, height = 3)

par(mfrow=c(1,2), bty="n", mar = c(2.7, 2.7, 1, 0), mgp=c(1.6,0.4,0), 
    bty="n", cex.axis=0.9, cex.lab=1, cex.main=1)

hist(ETIs[,"median"], xlim=c(0,28), prob=TRUE, xaxt="n", xlab="Median posterior ETI", main="", yaxt="n", ylim=c(0,0.12))
title("Distribution of ETI", font.main=1)
axis(1, seq(0, 28, length.out=8))
axis(2, seq(0, 0.12, length.out=4))
curve(mixtureDensity(x), 0, 28, add=TRUE, lwd=3)
#curve(lambda[1]*dnorm(x, mu[1], sqrt(sigma2[1])), 0, 28, add=TRUE, lty=1, lwd=1)
#curve(lambda[2]*dnorm(x, mu[2], sqrt(sigma2[2])), 0, 28, add=TRUE, lty=1, lwd=1)
#curve(lambda[3]*dnorm(x, mu[3], sqrt(sigma2[3])), 0, 28, add=TRUE, lty=1, lwd=1)
#curve(lambda[4]*dnorm(x, mu[4], sqrt(sigma2[4])), 0, 28, add=TRUE, lty=1, lwd=1)

plot(ETIs$date, ETIs$median, pch=19, cex=0.4, xlab="Calendar time", ylab="Median posterior ETI", bty="n",
		 ylim=c(0,28), yaxt="n", xlim=as.Date(c("2019-10-01", "2020-11-01")), xaxt="n")
axis(1, at = as.Date(c("2019-10-01", "2019-11-01", "2019-12-01", "2020-01-01", "2020-02-01",
                       "2020-03-01", "2020-04-01", "2020-05-01", "2020-06-01", "2020-07-01",
                       "2020-08-01", "2020-09-01", "2020-10-01", "2020-11-01")),
     labels=c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov"))
title("ETI over time", font.main=1)
axis(2, seq(0, 28, length.out=8))
dev.off()

