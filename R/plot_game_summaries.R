rm(list=ls())

load("results/est_ETIs.RData")
load("results/game_summaries.RData")
load("data/nba20192020.rda")

band <- function(t, l, u, col) {
  polygon(c(t, rev(t)), c(l, rev(u)), col=col, border = NA)
}

pdf("figures/fig3.pdf", width = 8, height = 9)

########################################
# Min plot
########################################
par(mfrow=c(5,3), bty="n", mar = c(2.7, 2.7, 2.5, 0), mgp=c(1.5,0.4,0), 
    bty="n", cex.axis=1, cex.lab=1.3, cex.main=1.2)

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
            round(ETIs[minIndex, "median"], 2),
            #" (", attributes(results[[minIndex]])$date, ")",
            " (season minimum)",
            sep=""), cex=1, font=2, padj=-0.9)

plot(min_summary$t, min_summary$TDI_med*100, type="n", xaxt="n", xlab="Game time [minutes]",
    ylab="TDI [%]", ylim=c(0,100))
axis(1, seq(0, 48, length.out=9))
band(min_summary$t, min_summary$TDI_99_L*100, min_summary$TDI_99_U*100, col="gray80")
band(min_summary$t, min_summary$TDI_95_L*100, min_summary$TDI_95_U*100, col="gray65")
band(min_summary$t, min_summary$TDI_50_L*100, min_summary$TDI_50_U*100, col="gray45")
lines(min_summary$t, min_summary$TDI_med*100, lwd=2)
lines(c(0,48), c(50,50), lty = 2)

########################################
# Quant 25 plot
########################################
plot(quant25_summary$t, quant25_summary$f, type="n", xaxt="n", xlab="Game time [minutes]",
  ylab="Score difference", ylim=c(-15, 20))
points(results[[quant25Index]]$time[results[[quant25Index]]$time < 48], 
			 results[[quant25Index]]$scorediff[results[[quant25Index]]$time < 48], 
			 pch=19, cex=0.8)
axis(1, seq(0, 48, length.out=9))
band(quant25_summary$t, quant25_summary$f_99_L, quant25_summary$f_99_U, col="gray80")
band(quant25_summary$t, quant25_summary$f_95_L, quant25_summary$f_95_U, col="gray65")
band(quant25_summary$t, quant25_summary$f_50_L, quant25_summary$f_50_U, col="gray45")
lines(quant25_summary$t, quant25_summary$f, lwd=2)

plot(quant25_summary$t, quant25_summary$df, type="n", xaxt="n", xlab="Game time [minutes]",
     ylab="Trend", ylim=c(-10, 10))
axis(1, seq(0, 48, length.out=9))
band(quant25_summary$t, quant25_summary$df_99_L, quant25_summary$df_99_U, col="gray80")
band(quant25_summary$t, quant25_summary$df_95_L, quant25_summary$df_95_U, col="gray65")
band(quant25_summary$t, quant25_summary$df_50_L, quant25_summary$df_50_U, col="gray45")
lines(quant25_summary$t, quant25_summary$df, lwd=2)
lines(c(0,48), c(0,0), lty=2)
mtext(paste(attributes(results[[quant25Index]])$home, 
            " vs ", 
            attributes(results[[quant25Index]])$away,
            ", ETI = ", 
            round(ETIs[quant25Index, "median"], 2),
            #" (", attributes(results[[quant25Index]])$date, ")",
            " (season 25% percentile)",
            sep=""), cex=1, font=2, padj=-0.9)

plot(quant25_summary$t, quant25_summary$TDI_med*100, type="n", xaxt="n", xlab="Game time [minutes]",
    ylab="TDI [%]", ylim=c(0,100))
axis(1, seq(0, 48, length.out=9))
band(quant25_summary$t, quant25_summary$TDI_99_L*100, quant25_summary$TDI_99_U*100, col="gray80")
band(quant25_summary$t, quant25_summary$TDI_95_L*100, quant25_summary$TDI_95_U*100, col="gray65")
band(quant25_summary$t, quant25_summary$TDI_50_L*100, quant25_summary$TDI_50_U*100, col="gray45")
lines(quant25_summary$t, quant25_summary$TDI_med*100, lwd=2)
lines(c(0,48), c(50,50), lty = 2)

########################################
# Median plot
########################################
plot(median_summary$t, median_summary$f, type="n", xaxt="n", xlab="Game time [minutes]",
		 ylab="Score difference", ylim=c(-25, 5))
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
            round(ETIs[medianIndex, "median"], 2),
            #" (", attributes(results[[medianIndex]])$date, ")",
            " (season median)",
            sep=""), cex=1, font=2, padj=-0.9)

plot(median_summary$t, median_summary$TDI_med*100, type="n", xaxt="n",
    ylab="TDI [%]", ylim=c(0,100), xlab="Game time [minutes]")
axis(1, seq(0, 48, length.out=9))
band(median_summary$t, median_summary$TDI_99_L*100, median_summary$TDI_99_U*100, col="gray80")
band(median_summary$t, median_summary$TDI_95_L*100, median_summary$TDI_95_U*100, col="gray65")
band(median_summary$t, median_summary$TDI_50_L*100, median_summary$TDI_50_U*100, col="gray45")
lines(median_summary$t, median_summary$TDI_med*100, lwd=2)
lines(c(0,48), c(50,50), lty = 2)

########################################
# Quqnat75 plot
########################################
plot(quant75_summary$t, quant75_summary$f, type="n", xaxt="n", xlab="Game time [minutes]",
		 ylab="Score difference", ylim=c(-15, 10))
points(results[[quant75Index]]$time[results[[quant75Index]]$time < 48], 
			 results[[quant75Index]]$scorediff[results[[quant75Index]]$time < 48], 
			 pch=19, cex=0.8)
axis(1, seq(0, 48, length.out=9))
band(quant75_summary$t, quant75_summary$f_99_L, quant75_summary$f_99_U, col="gray80")
band(quant75_summary$t, quant75_summary$f_95_L, quant75_summary$f_95_U, col="gray65")
band(quant75_summary$t, quant75_summary$f_50_L, quant75_summary$f_50_U, col="gray45")
lines(quant75_summary$t, quant75_summary$f, lwd=2)

plot(quant75_summary$t, quant75_summary$df, type="n", xaxt="n", xlab="Game time [minutes]",
     ylab="Trend", ylim=c(-10,10))
axis(1, seq(0, 48, length.out=9))
band(quant75_summary$t, quant75_summary$df_99_L, quant75_summary$df_99_U, col="gray80")
band(quant75_summary$t, quant75_summary$df_95_L, quant75_summary$df_95_U, col="gray65")
band(quant75_summary$t, quant75_summary$df_50_L, quant75_summary$df_50_U, col="gray45")
lines(quant75_summary$t, quant75_summary$df, lwd=2)
lines(c(0,48), c(0,0), lty=2)
mtext(paste(attributes(results[[quant75Index]])$home, 
            " vs ", 
            attributes(results[[quant75Index]])$away,
            ", ETI = ", 
            round(ETIs[quant75Index, "median"], 2),
            #" (", attributes(results[[quant75Index]])$date, ")",
            " (season 75% percentile)",
            sep=""), cex=1, font=2, padj=-0.9)

plot(quant75_summary$t, quant75_summary$TDI_med*100, type="n", xaxt="n",
    ylab="TDI [%]", ylim=c(0,100), xlab="Game time [minutes]")
axis(1, seq(0, 48, length.out=9))
band(quant75_summary$t, quant75_summary$TDI_99_L*100, quant75_summary$TDI_99_U*100, col="gray80")
band(quant75_summary$t, quant75_summary$TDI_95_L*100, quant75_summary$TDI_95_U*100, col="gray65")
band(quant75_summary$t, quant75_summary$TDI_50_L*100, quant75_summary$TDI_50_U*100, col="gray45")
lines(quant75_summary$t, quant75_summary$TDI_med*100, lwd=2)
lines(c(0,48), c(50,50), lty = 2)

########################################
# Max plot
########################################
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
            round(ETIs[maxIndex, "median"], 2),
            #" (", attributes(results[[maxIndex]])$date, ")",
            " (season maximum)",
            sep=""), cex=1, font=2, padj=-0.9)

plot(max_summary$t, max_summary$TDI_med, type="n", xaxt="n", xlab="Game time [minutes]",
     ylab="TDI [%]", ylim=c(0,100))
axis(1, seq(0, 48, length.out=9))
band(max_summary$t, max_summary$TDI_99_L*100, max_summary$TDI_99_U*100, col="gray80")
band(max_summary$t, max_summary$TDI_95_L*100, max_summary$TDI_95_U*100, col="gray65")
band(max_summary$t, max_summary$TDI_50_L*100, max_summary$TDI_50_U*100, col="gray45")
lines(max_summary$t, max_summary$TDI_med*100, lwd=2)
lines(c(0,48), c(50,50), lty = 2)

dev.off()
