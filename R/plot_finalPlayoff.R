rm(list=ls())
library(rstan)

load("results/summary_finalPlayoff.RData")

band <- function(t, l, u, col) {
  polygon(c(t, rev(t)), c(l, rev(u)), col=col, border = NA)
}

pdf("figures/fig1.pdf", width = 8, height = 2.5)

par(mfrow=c(1,3), bty="n", mar = c(2.7, 2.7, 1, 0), mgp=c(1.6,0.4,0), 
    bty="n", cex.axis=1.1, cex.lab=1.4, cex.main=1.3)

plot(d$time, d$scorediff, pch = 19, xlab="Game time [minutes]", ylab="f | Y", 
     type="n", ylim=c(0, 40), xlim=c(0,48), xaxt="n")
title("Running score difference", font.main=1)
axis(1, seq(0, 48, length.out=9))
band(post_summary$tPred, post_summary$f_99_L, post_summary$f_99_U, col="gray85")
band(post_summary$tPred, post_summary$f_95_L, post_summary$f_95_U, col="gray65")
band(post_summary$tPred, post_summary$f_50_L, post_summary$f_50_U, col="gray45")
lines(post_summary$tPred, post_summary$f_mean, lwd=2)
points(d$time, d$scorediff, pch=19, col="black", cex=0.6)
legend("topleft", c("Mean", "50%", "95%", "99%"), 
       col = c("black", "gray45", "gray65", "gray85"), 
       lwd = 2, bty="n", cex=0.8, lty = c(1, NA, NA, NA), 
       pch = c(NA, 15, 15, 15), pt.cex=1.5)

plot(post_summary$tPred, post_summary$df_mean, ylim=c(-5, 5), type="n", lwd=2,
		 xlab="Game time [minutes]", ylab="df | Y", yaxt="n", xaxt="n")
title("Derivative of running score difference", font.main=1)
band(post_summary$tPred, post_summary$df_99_L, post_summary$df_99_U, col="gray85")
band(post_summary$tPred, post_summary$df_95_L, post_summary$df_95_U, col="gray65")
band(post_summary$tPred, post_summary$df_50_L, post_summary$df_50_U, col="gray45")
lines(post_summary$tPred, post_summary$df_mean, lwd=2)
lines(c(0, 48), c(0, 0), lty=2)
axis(1, seq(0, 48, length.out=9))
axis(2, seq(-5, 5, length.out=6))

plot(post_summary$tPred, post_summary$TDI_mean*100, ylim=c(0, 100), type="n", lwd=2,
		 xlab="Game time [minutes]", ylab="TDI [%]", xaxt="n")
title("Trend Direction Index", font.main=1)
band(post_summary$tPred, post_summary$TDI_99_L*100, post_summary$TDI_99_U*100, col="gray85")
band(post_summary$tPred, post_summary$TDI_95_L*100, post_summary$TDI_95_U*100, col="gray65")
band(post_summary$tPred, post_summary$TDI_50_L*100, post_summary$TDI_50_U*100, col="gray45")
lines(post_summary$tPred, post_summary$TDI_mean*100, lwd=2)
lines(c(0, 48), c(50, 50), lty=2)
axis(1, seq(0, 48, length.out=9))

dev.off()



## Also make a raw plot



pdf("figures/fig1-raw.pdf", width = 8, height = 5)

plot(d$time, d$scorediff, pch = 19, xlab="Game time [minutes]", ylab="Score difference", 
     type="n", ylim=c(0, 40), xlim=c(0,48), xaxt="n")
title("Running score difference", font.main=1)
axis(1, seq(0, 48, length.out=9))
#band(post_summary$tPred, post_summary$f_99_L, post_summary$f_99_U, col="gray85")
#band(post_summary$tPred, post_summary$f_95_L, post_summary$f_95_U, col="gray65")
#band(post_summary$tPred, post_summary$f_50_L, post_summary$f_50_U, col="gray45")
#lines(post_summary$tPred, post_summary$f_mean, lwd=2)
points(d$time, d$scorediff, pch=19, col="black", cex=0.6)
#legend("topleft", c("Mean", "50%", "95%", "99%"), 
#       col = c("black", "gray45", "gray65", "gray85"), 
#       lwd = 2, bty="n", cex=0.8, lty = c(1, NA, NA, NA), 
#       pch = c(NA, 15, 15, 15), pt.cex=1.5)


dev.off()