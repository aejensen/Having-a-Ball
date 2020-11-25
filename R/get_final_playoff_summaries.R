library(rstan)

getSummary <- function(fit) {
  posterior <- extract(fit$posterior, "pred")$pred	
  data.frame(t = fit$data$tPred,
             #Mean function
             f = apply(posterior[,,1], 2, mean),
  					 f_med = apply(posterior[,,1], 2, median),
             f_50_L = apply(posterior[,,1], 2, quantile, prob = 0.25),
             f_50_U = apply(posterior[,,1], 2, quantile, prob = 0.75),
             f_95_L = apply(posterior[,,1], 2, quantile, prob = 0.025),
             f_95_U = apply(posterior[,,1], 2, quantile, prob = 0.975),
             f_99_L = apply(posterior[,,1], 2, quantile, prob = 0.005),
             f_99_U = apply(posterior[,,1], 2, quantile, prob = 0.995),
             #Derivative
             df = apply(posterior[,,3], 2, mean),
  					 df_med = apply(posterior[,,3], 2, median),
             df_50_L = apply(posterior[,,3], 2, quantile, prob = 0.25),
             df_50_U = apply(posterior[,,3], 2, quantile, prob = 0.75),
             df_95_L = apply(posterior[,,3], 2, quantile, prob = 0.025),
             df_95_U = apply(posterior[,,3], 2, quantile, prob = 0.975),
             df_99_L = apply(posterior[,,3], 2, quantile, prob = 0.005),
             df_99_U = apply(posterior[,,3], 2, quantile, prob = 0.995),
             #Trend Direction Index
             TDI = apply(posterior[,,5], 2, mean),
  					 TDI_med = apply(posterior[,,5], 2, median),
             TDI_50_L = apply(posterior[,,5], 2, quantile, prob = 0.25),
             TDI_50_U = apply(posterior[,,5], 2, quantile, prob = 0.75),
             TDI_95_L = apply(posterior[,,5], 2, quantile, prob = 0.025),
             TDI_95_U = apply(posterior[,,5], 2, quantile, prob = 0.975),
             TDI_99_L = apply(posterior[,,5], 2, quantile, prob = 0.005),
             TDI_99_U = apply(posterior[,,5], 2, quantile, prob = 0.995))
}

