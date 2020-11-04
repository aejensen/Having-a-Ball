rm(list=ls())

library(DEoptim)
library(mvtnorm)
library(rstan)
library(parallel)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

load("test.RData")

########################################################
# Fit models
########################################################
m <- stan_model("gptrendFixed.stan")

fitTrendR <- function(d) {
	tPred <- seq(min(d$time), max(d$time), length.out = 300)
	
	#Estimate Empirical Bayes parameters
  rqCov <- function(s, t, alpha, rho, nu) {
    alpha^2 * (1 + (s-t)^2 / (2 * nu * rho^2))^(-nu)
  }

  ctl <- DEoptim.control(itermax = 2000, trace = 100)
  set.seed(1234)
  opt.rq <- DEoptim(function(par) {
    mu <- rep(par[1], nrow(d))
    cMat <- outer(d$time, d$time, rqCov, par[2], par[3], 10000) + diag(par[4]^2 , nrow(d))
    -mvtnorm::dmvnorm(d$scorediff, mu, cMat, log=TRUE)
  }, lower = c(-50,0,0,0), upper = c(50,20,20,20), control = ctl)
  par.rq <- opt.rq$optim$bestmem
	
  #Fit model
  sDat <- list(n = nrow(d), t = d$time, y = d$scorediff, p = length(tPred), tPred = tPred)
  sDat$mu <- par.rq[1]
  sDat$alpha <- par.rq[2]
  sDat$rho <- par.rq[3]
  sDat$nu <- 10000
  sDat$sigma <- par.rq[4]

  iter <- 10000
  seed <- 12345
  fit <- sampling(m, data = sDat, iter = iter, seed = seed, algorithm = "Fixed_param")
  pred <- extract(fit, "pred")$pred
  
  list(data = sDat, posterior = pred)
}

fits <- mclapply(list(d1, d2, d3, d4, d5), function(d) {
	fitTrendR(d)
}, mc.cores=5)
save(fits, file="post.RData")

########################################################
# Test
########################################################
fit <- fits[[1]]

integrate(splinefun(fits[[1]]$data$tPred, fits[[1]]$posterior[1,,6]), min(fits[[1]]$data$tPred), max(fits[[1]]$data$tPred))

sapply(1:5, function(m) {
  integrate(splinefun(fits[[m]]$data$tPred, fits[[m]]$posterior[1,,6]), min(fits[[m]]$data$tPred), max(fits[[m]]$data$tPred))$value
})

########################################################
# Plot results
########################################################
band <- function(t, l, u, col) {
  polygon(c(t, rev(t)), c(l, rev(u)), col=col, border = NA)
}

pdf("../figures/gpFit.pdf", width = 8, height = 6)
par(mfrow=c(2,2), bty="n", mar = c(2.3, 2.3, 1, 0), mgp=c(1.3,0.4,0))
plot(fit$data$t, fit$data$y, pch = 19, xlab="Game time", ylab="f | Y", 
     type="n", ylim=c(-12, 12), xlim=c(0,50), yaxt="n")
axis(2, seq(-12, 12, length.out=7))
band(fit$data$tPred, 
     apply(fit$posterior[,,1], 2, quantile, prob = 0.005), 
     apply(fit$posterior[,,1], 2, quantile, prob = 0.995), col = "gray80")
band(fit$data$tPred, 
     apply(fit$posterior[,,1], 2, quantile, prob = 0.025), 
     apply(fit$posterior[,,1], 2, quantile, prob = 0.975), col = "gray65")
band(fit$data$tPred, 
     apply(fit$posterior[,,1], 2, quantile, prob = 0.25), 
     apply(fit$posterior[,,1], 2, quantile, prob = 0.75), col = "gray45")
lines(fit$data$tPred, 
      apply(fit$posterior[,,1], 2, mean), lwd = 2)
points(fit$data$t, fit$data$y, pch = 19, cex=0.8)
abline(h = 0, lty=2)
legend("topleft", c("Mean", "50%", "95%", "99%"), 
       col = c("black", "gray45", "gray65", "gray85"), 
       lwd = 2, bty="n", cex=0.7, lty = c(1, NA, NA, NA), 
       pch = c(NA, 15, 15, 15), pt.cex=1.5)

plot(fit$data$tPred, apply(fit$posterior[,,3], 2, mean), lwd = 2, type="n", 
     ylim = c(-10, 10), xlab="Game time", ylab="df | Y", xlim=c(0,50))
band(fit$data$tPred, 
     apply(fit$posterior[,,3], 2, quantile, prob = 0.005), 
     apply(fit$posterior[,,3], 2, quantile, prob = 0.995), col = "gray80")
band(fit$data$tPred, 
     apply(fit$posterior[,,3], 2, quantile, prob = 0.025), 
     apply(fit$posterior[,,3], 2, quantile, prob = 0.975), col = "gray65")
band(fit$data$tPred, 
     apply(fit$posterior[,,3], 2, quantile, prob = 0.25), 
     apply(fit$posterior[,,3], 2, quantile, prob = 0.75), col = "gray45")
lines(fit$data$tPred, apply(fit$posterior[,,3], 2, mean), lwd = 2)
abline(h = 0, lty = 2)
legend("topleft", c("Mean", "50%", "95%", "99%"), 
       col = c("black", "gray45", "gray65", "gray85"), 
       lwd = 2, bty="n", cex=0.7, lty = c(1, NA, NA, NA), 
       pch = c(NA, 15, 15, 15), pt.cex=1.5)

plot(fit$data$tPred, t(fit$posterior[1,,5])*100, type="l", lty = 1, lwd = 2, 
     xlab="Game time", ylab="Trend Direction Index [%]", ylim=c(0,100), xlim=c(0,50))
abline(h = 50, lty = 2)
title("Trend Direction Index", font.main=1)

plot(fit$data$tPred, t(fit$posterior[1,,6]), type="l", lty = 1, lwd = 2, 
     xlab="Game time", ylab="dETI", ylim=c(0,2.5), xlim=c(0,50))
title("Local Excitement Trend Index", font.main=1)
dev.off()

########################################################
# Excitement Trend Index
########################################################
integrate(splinefun(fit$data$tPred, fit$posterior[1,,6]), min(fit$data$tPred), max(fit$data$tPred))

