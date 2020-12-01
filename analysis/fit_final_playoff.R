library(DEoptim)
library(mvtnorm)
library(rstan)
library(parallel)
rstan_options(auto_write = TRUE)
options(mc.cores = 4)

m <- stan_model("gptrend.stan")
d <- read.csv("../data/finalPlayoff2020.csv")

fitTrendR <- function(d) {
  tPred <- seq(0, 48, length.out = 241)
	
  seCov <- function(s, t, alpha, rho) {
    alpha^2 * exp(-(s-t)^2 / (2 * rho^2))
  }
	
  ctl <- DEoptim.control(itermax = 1000, trace = 100)
  set.seed(1234)
  opt <- DEoptim(function(par) {
    mu <- rep(par[1], nrow(d))
    cMat <- outer(d$time, d$time, seCov, par[2], par[3]) + diag(par[4]^2 , nrow(d))
    -mvtnorm::dmvnorm(d$scorediff, mu, cMat, log=TRUE)
  }, lower = c(-50,0,0,0), upper = c(50,20,50,10), control = ctl)
  par <- opt$optim$bestmem
	
  #Fit model
  sDat <- list(n = nrow(d), t = d$time, y = d$scorediff, p = length(tPred), tPred = tPred)
  sDat$mu_mu <- par[1]
  sDat$alpha_mu <- par[2]
  sDat$rho_mu <- par[3]
  sDat$sigma_mu <- par[4]

  iter <- 50 * 10^3
  seed <- 12345
  fit <- sampling(m, data = sDat, iter = iter, seed = seed, chains = 4)

  list(data = d, sDat = sDat, posterior = fit)
}

fit <- fitTrendR(d)

save(fit, file = "output/fit_finalPlayoff.RData")
