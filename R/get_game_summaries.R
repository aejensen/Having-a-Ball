library(rstan)

load(file="analysis/est_ETIs.RData")

getSummary <- function(fit) {
  posterior <- extract(fit$posterior, "pred")$pred	
  data.frame(t = fit$data$tPred,
             #Mean function
             f = apply(posterior[,,1], 2, mean),
             f_50_L = apply(posterior[,,1], 2, quantile, prob = 0.25),
             f_50_U = apply(posterior[,,1], 2, quantile, prob = 0.75),
             f_95_L = apply(posterior[,,1], 2, quantile, prob = 0.025),
             f_95_U = apply(posterior[,,1], 2, quantile, prob = 0.975),
             f_99_L = apply(posterior[,,1], 2, quantile, prob = 0.005),
             f_99_U = apply(posterior[,,1], 2, quantile, prob = 0.995),
             #Derivative
             df = apply(posterior[,,3], 2, mean),
             df_50_L = apply(posterior[,,3], 2, quantile, prob = 0.25),
             df_50_U = apply(posterior[,,3], 2, quantile, prob = 0.75),
             df_95_L = apply(posterior[,,3], 2, quantile, prob = 0.025),
             df_95_U = apply(posterior[,,3], 2, quantile, prob = 0.975),
             df_99_L = apply(posterior[,,3], 2, quantile, prob = 0.005),
             df_99_U = apply(posterior[,,3], 2, quantile, prob = 0.995),
             #Trend Direction Index
             TDI = apply(posterior[,,5], 2, mean),
             TDI_50_L = apply(posterior[,,5], 2, quantile, prob = 0.25),
             TDI_50_U = apply(posterior[,,5], 2, quantile, prob = 0.75),
             TDI_95_L = apply(posterior[,,5], 2, quantile, prob = 0.025),
             TDI_95_U = apply(posterior[,,5], 2, quantile, prob = 0.975),
             TDI_99_L = apply(posterior[,,5], 2, quantile, prob = 0.005),
             TDI_99_U = apply(posterior[,,5], 2, quantile, prob = 0.995))
}

minIndex <- which(ETIs[,"mean"] == min(ETIs[,"mean"]))
medianIndex <- which(ETIs[,"mean"] == median(ETIs[,"mean"]))
maxIndex <- which(ETIs[,"mean"] == max(ETIs[,"mean"]))

ETIs[c(minIndex, medianIndex, maxIndex),]

#####
minFile <- paste("analysis/output/", ETIs[minIndex, "file"], sep="")
load(minFile)
min_summary <- getSummary(fit)

#####
medianFile <- paste("analysis/output/", ETIs[medianIndex, "file"], sep="")
load(medianFile)
median_summary <- getSummary(fit)

#####
maxFile <- paste("analysis/output/", ETIs[maxIndex, "file"], sep="")
load(maxFile)
max_summary <- getSummary(fit)

#####
txtplot(min_summary$t, min_summary$f)
txtplot(median_summary$t, median_summary$f)
txtplot(max_summary$t, max_summary$f)

txtplot(min_summary$t, min_summary$df)
txtplot(median_summary$t, median_summary$df)
txtplot(max_summary$t, max_summary$df)

txtplot(min_summary$t, min_summary$TDI)
txtplot(median_summary$t, median_summary$TDI)
txtplot(max_summary$t, max_summary$TDI)


#####
save(min_summary, median_summary, max_summary,
     file="results/game_summaries.RData")


load("results/game_summaries.RData")
load("data/nba20192020.rda")

band <- function(t, l, u, col) {
  polygon(c(t, rev(t)), c(l, rev(u)), col=col, border = NA)
}

pdf("figures/fig3.pdf", width = 8, height = 7)

par(mfrow=c(3,3), bty="n", mar = c(2.7, 2.7, 2.5, 0), mgp=c(1.5,0.4,0), 
    bty="n", cex.axis=1, cex.lab=1, cex.main=1.3)

plot(min_summary$t, min_summary$f, type="n", xaxt="n", xlab="Game time [minutes]",
  ylab="Score difference", ylim=c(-50, 0))
points(results[[minIndex]]$time[results[[minIndex]]$time < 48], 
			 results[[minIndex]]$scorediff[results[[minIndex]]$time < 48], 
			 pch=19, cex=0.8)
axis(1, seq(0, 48, length.out=9))
band(min_summary$t, min_summary$f_99_L, min_summary$f_99_U, col="gray80")
band(min_summary$t, min_summary$f_95_L, min_summary$f_95_U, col="gray65")
band(min_summary$t, min_summary$f_50_L, min_summary$f_50_U, col="gray45")
lines(min_summary$t, min_summary$f, lwd=2)
legend("bottomleft", 
			 c("Mean", "50%", "95%", "99%"), 
			 col = c("black", "gray45", "gray65", "gray85"),
       lwd = 2, bty="n", cex=0.9, lty = c(1, NA, NA, NA), 
			 pch = c(NA, 15, 15, 15), pt.cex=1.5, seg.len=1.3)


plot(min_summary$t, min_summary$df, type="n", xaxt="n", xlab="Game time [minutes]",
     ylab="Trend", ylim=c(-10, 10))
axis(1, seq(0, 48, length.out=9))
band(min_summary$t, min_summary$df_99_L, min_summary$df_99_U, col="gray80")
band(min_summary$t, min_summary$df_95_L, min_summary$df_95_U, col="gray65")
band(min_summary$t, min_summary$df_50_L, min_summary$df_50_U, col="gray45")
lines(min_summary$t, min_summary$df, lwd=2)
lines(c(0,48), c(0,0), lty=2)
mtext(paste(attributes(results[[minIndex]])$home, 
            " vs ", 
            attributes(results[[minIndex]])$away,
            ", ETI = ", 
            round(ETIs[minIndex, "mean"], 2),
            #" (", attributes(results[[minIndex]])$date, ")",
            " (season minimum)",
            sep=""), cex=1, font=2, padj=-0.9)

plot(min_summary$t, min_summary$TDI*100, type="n", xaxt="n", xlab="Game time [minutes]",
    ylab="TDI [%]", ylim=c(0,100))
axis(1, seq(0, 48, length.out=9))
band(min_summary$t, min_summary$TDI_99_L*100, min_summary$TDI_99_U*100, col="gray80")
band(min_summary$t, min_summary$TDI_95_L*100, min_summary$TDI_95_U*100, col="gray65")
band(min_summary$t, min_summary$TDI_50_L*100, min_summary$TDI_50_U*100, col="gray45")
lines(min_summary$t, min_summary$TDI*100, lwd=2)
lines(c(0,48), c(50,50), lty = 2)

#####
plot(median_summary$t, median_summary$f, type="n", xaxt="n", xlab="Game time [minutes]",
		 ylab="Score difference", ylim=c(-5, 22))
points(results[[medianIndex]]$time[results[[medianIndex]]$time < 48], 
			 results[[medianIndex]]$scorediff[results[[medianIndex]]$time < 48], 
			 pch=19, cex=0.8)
axis(1, seq(0, 48, length.out=9))
band(median_summary$t, median_summary$f_99_L, median_summary$f_99_U, col="gray80")
band(median_summary$t, median_summary$f_95_L, median_summary$f_95_U, col="gray65")
band(median_summary$t, median_summary$f_50_L, median_summary$f_50_U, col="gray45")
lines(median_summary$t, median_summary$f, lwd=2)

plot(median_summary$t, median_summary$df, type="n", xaxt="n", xlab="Game time [minutes]",
     ylab="Trend", ylim=c(-10,10))
axis(1, seq(0, 48, length.out=9))
band(median_summary$t, median_summary$df_99_L, median_summary$df_99_U, col="gray80")
band(median_summary$t, median_summary$df_95_L, median_summary$df_95_U, col="gray65")
band(median_summary$t, median_summary$df_50_L, median_summary$df_50_U, col="gray45")
lines(median_summary$t, median_summary$df, lwd=2)
lines(c(0,48), c(0,0), lty=2)
mtext(paste(attributes(results[[medianIndex]])$home, 
            " vs ", 
            attributes(results[[medianIndex]])$away,
            ", ETI = ", 
            round(ETIs[medianIndex, "mean"], 2),
            #" (", attributes(results[[medianIndex]])$date, ")",
            " (season median)",
            sep=""), cex=1, font=2, padj=-0.9)

plot(median_summary$t, median_summary$TDI*100, type="n", xaxt="n",
    ylab="TDI [%]", ylim=c(0,100), xlab="Game time [minutes]")
axis(1, seq(0, 48, length.out=9))
band(median_summary$t, median_summary$TDI_99_L*100, median_summary$TDI_99_U*100, col="gray80")
band(median_summary$t, median_summary$TDI_95_L*100, median_summary$TDI_95_U*100, col="gray65")
band(median_summary$t, median_summary$TDI_50_L*100, median_summary$TDI_50_U*100, col="gray45")
lines(median_summary$t, median_summary$TDI*100, lwd=2)
lines(c(0,48), c(50,50), lty = 2)

####
plot(max_summary$t, max_summary$f, type="n", xaxt="n", xlab="Game time [minutes]",
		 ylab="Score difference", ylim=c(-10, 10))
points(results[[maxIndex]]$time[results[[maxIndex]]$time < 48], 
			 results[[maxIndex]]$scorediff[results[[maxIndex]]$time < 48],
			 pch=19, cex=0.8)
axis(1, seq(0, 48, length.out=9))
band(max_summary$t, max_summary$f_99_L, max_summary$f_99_U, col="gray80")
band(max_summary$t, max_summary$f_95_L, max_summary$f_95_U, col="gray65")
band(max_summary$t, max_summary$f_50_L, max_summary$f_50_U, col="gray45")
lines(max_summary$t, max_summary$f, lwd=2)

plot(max_summary$t, max_summary$df, type="n", xaxt="n", xlab="Game time [minutes]",
    ylab="Trend", ylim=c(-10,10))
axis(1, seq(0, 48, length.out=9))
band(max_summary$t, max_summary$df_99_L, max_summary$df_99_U, col="gray80")
band(max_summary$t, max_summary$df_95_L, max_summary$df_95_U, col="gray65")
band(max_summary$t, max_summary$df_50_L, max_summary$df_50_U, col="gray45")
lines(max_summary$t, max_summary$df, lwd=2)
lines(c(0,48), c(0,0), lty=2)
mtext(paste(attributes(results[[maxIndex]])$home, 
            " vs ", 
            attributes(results[[maxIndex]])$away,
            ", ETI = ", 
            round(ETIs[maxIndex, "mean"], 2),
            #" (", attributes(results[[maxIndex]])$date, ")",
            " (season maximum)",
            sep=""), cex=1, font=2, padj=-0.9)

plot(max_summary$t, max_summary$TDI, type="n", xaxt="n", xlab="Game time [minutes]",
     ylab="TDI [%]", ylim=c(0,100))
axis(1, seq(0, 48, length.out=9))
band(max_summary$t, max_summary$TDI_99_L*100, max_summary$TDI_99_U*100, col="gray80")
band(max_summary$t, max_summary$TDI_95_L*100, max_summary$TDI_95_U*100, col="gray65")
band(max_summary$t, max_summary$TDI_50_L*100, max_summary$TDI_50_U*100, col="gray45")
lines(max_summary$t, max_summary$TDI*100, lwd=2)
lines(c(0,48), c(50,50), lty = 2)

dev.off()
