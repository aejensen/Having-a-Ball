library(DEoptim)
library(mvtnorm)
library(rstan)
library(parallel)
rstan_options(auto_write = TRUE)
options(mc.cores = 4)

m <- stan_model("gptrend.stan")
d <- read.csv("../data/finalPlayoff2020.csv")

fitTrendR <- function(d) {
  tPred <- seq(0, 48, length.out = 200)
	
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

  iter <- 25 * 10^3
  seed <- 12345
  fit <- sampling(m, data = sDat, iter = iter, seed = seed, chains = 4)

  list(data = sDat, posterior = fit)
}

fit <- fitTrendR(d)
save.image(file = "../data/res_finalPlayoff.RData")

# Summarize
posterior <- extract(fit$posterior, "pred")$pred

post_summary <- data.frame(tPred = fit$data$tPred,
                           f_mean = apply(posterior[,,1], 2, mean),
                           f_50_L = apply(posterior[,,1], 2, quantile, prob=0.25),
                           f_50_U = apply(posterior[,,1], 2, quantile, prob=0.75),
                           f_95_L = apply(posterior[,,1], 2, quantile, prob=0.025),
                           f_95_U = apply(posterior[,,1], 2, quantile, prob=0.975),
                           f_99_L = apply(posterior[,,1], 2, quantile, prob=0.005),
                           f_99_U = apply(posterior[,,1], 2, quantile, prob=0.995),
                           df_mean = apply(posterior[,,3], 2, mean),
                           df_50_L = apply(posterior[,,3], 2, quantile, prob=0.25),
                           df_50_U = apply(posterior[,,3], 2, quantile, prob=0.75),
                           df_95_L = apply(posterior[,,3], 2, quantile, prob=0.025),
                           df_95_U = apply(posterior[,,3], 2, quantile, prob=0.975),
                           df_99_L = apply(posterior[,,3], 2, quantile, prob=0.005),
                           df_99_U = apply(posterior[,,3], 2, quantile, prob=0.995),
                           TDI_mean = apply(posterior[,,5], 2, mean),
													 TDI_median = apply(posterior[,,5], 2, median),
                           TDI_50_L = apply(posterior[,,5], 2, quantile, prob=0.25),
                           TDI_50_U = apply(posterior[,,5], 2, quantile, prob=0.75),
                           TDI_95_L = apply(posterior[,,5], 2, quantile, prob=0.025),
                           TDI_95_U = apply(posterior[,,5], 2, quantile, prob=0.975),
                           TDI_99_L = apply(posterior[,,5], 2, quantile, prob=0.005),
                           TDI_99_U = apply(posterior[,,5], 2, quantile, prob=0.995))

ETI <- apply(posterior[,,6], 1, function(q) pracma::trapz(fit$data$tPred, q)) 
		
save(d, post_summary, ETI, file = "../data/summary_finalPlayoff.RData")
