rm(list=ls())
load("../analysis/est_ETIs.RData")
load("../data/nba20192020.rda")

ETIs$date <- as.Date(sapply(results, function(q) attributes(q)$date), format="%B %e, %Y")
ETIs$home <- as.factor(sapply(results, function(q) attributes(q)$home))
ETIs$away <- as.factor(sapply(results, function(q) attributes(q)$away))

pdf("figures/fig2.pdf", width = 8, height = 3)

par(mfrow=c(1,3), bty="n", mar = c(2.7, 2.7, 1, 0), mgp=c(1.6,0.4,0), 
    bty="n", cex.axis=1.1, cex.lab=1.4, cex.main=1.3)

hist(ETIs[,"mean"], xlim=c(0,28), prob=TRUE, xaxt="n", xlab="ETI", main="")
title("Distribution of ETI", font.main=1)
axis(1, seq(0, 28, length.out=8))
dens <- density(ETIs[,"mean"], bw="SJ")
curve(approxfun(dens$x, dens$y)(x), 0, 28, add=TRUE, lwd=2)

plot(ETIs$date, ETIs$mean, pch=19, cex=0.4, col="gray", xlab="Calendar time", ylab="ETI", bty="n",
		 ylim=c(0,28), yaxt="n")
title("ETI over time", font.main=1)
axis(2, seq(0, 28, length.out=8))
#lines(lokern::lokerns(dates, ETIs[,"mean"]), lwd=2)

dev.off()