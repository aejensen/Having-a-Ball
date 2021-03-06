library(rstan)

load("../analysis/output/fit_finalPlayoff.RData")

posterior <- extract(fit$posterior, "pred")$pred

post_summary <- data.frame(tPred = fit$sDat$tPred,
                           #Mean function
                           f_mean = apply(posterior[,,1], 2, mean),
                           f_median = apply(posterior[,,1], 2, median),
                           f_50_L = apply(posterior[,,1], 2, quantile, prob=0.25),
                           f_50_U = apply(posterior[,,1], 2, quantile, prob=0.75),
                           f_95_L = apply(posterior[,,1], 2, quantile, prob=0.025),
                           f_95_U = apply(posterior[,,1], 2, quantile, prob=0.975),
                           #Posterior predictive distribution
                           y_95_L = apply(posterior[,,2], 2, quantile, prob=0.025),
                           y_95_U = apply(posterior[,,2], 2, quantile, prob=0.975),
                           #Trend
                           df_mean = apply(posterior[,,3], 2, mean),
                           df_median = apply(posterior[,,3], 2, median),
                           df_50_L = apply(posterior[,,3], 2, quantile, prob=0.25),
                           df_50_U = apply(posterior[,,3], 2, quantile, prob=0.75),
                           df_95_L = apply(posterior[,,3], 2, quantile, prob=0.025),
                           df_95_U = apply(posterior[,,3], 2, quantile, prob=0.975),
                           #Trend Direction Index
                           TDI_mean = apply(posterior[,,5], 2, mean),
                           TDI_median = apply(posterior[,,5], 2, median),
                           TDI_50_L = apply(posterior[,,5], 2, quantile, prob=0.25),
                           TDI_50_U = apply(posterior[,,5], 2, quantile, prob=0.75),
                           TDI_95_L = apply(posterior[,,5], 2, quantile, prob=0.025),
                           TDI_95_U = apply(posterior[,,5], 2, quantile, prob=0.975),
                           #Local Excitement Trend Index
                           dETI_mean = apply(posterior[,,6], 2, mean),
                           dETI_median = apply(posterior[,,6], 2, median),
                           dETI_50_L = apply(posterior[,,6], 2, quantile, prob=0.25),
                           dETI_50_U = apply(posterior[,,6], 2, quantile, prob=0.75),
                           dETI_95_L = apply(posterior[,,6], 2, quantile, prob=0.025),
                           dETI_95_U = apply(posterior[,,6], 2, quantile, prob=0.975))

ETI <- apply(posterior[,,6], 1, function(q) pracma::trapz(fit$sDat$tPred, q)) 
data <- fit$data

save(data, post_summary, ETI, file = "../results/summary_final_playoff.RData")
