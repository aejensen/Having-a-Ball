library(rstan)

load("analysis/output/fit_finalPlayoff.RData")

posterior <- extract(fit$posterior, "pred")$pred

post_summary <- data.frame(tPred = fit$sDat$tPred,
                           f_mean = apply(posterior[,,1], 2, mean),
                           f_median = apply(posterior[,,1], 2, median),
                           f_50_L = apply(posterior[,,1], 2, quantile, prob=0.25),
                           f_50_U = apply(posterior[,,1], 2, quantile, prob=0.75),
                           f_95_L = apply(posterior[,,1], 2, quantile, prob=0.025),
                           f_95_U = apply(posterior[,,1], 2, quantile, prob=0.975),
                           df_mean = apply(posterior[,,3], 2, mean),
                           df_median = apply(posterior[,,3], 2, median),
                           df_50_L = apply(posterior[,,3], 2, quantile, prob=0.25),
                           df_50_U = apply(posterior[,,3], 2, quantile, prob=0.75),
                           df_95_L = apply(posterior[,,3], 2, quantile, prob=0.025),
                           df_95_U = apply(posterior[,,3], 2, quantile, prob=0.975),
                           TDI_mean = apply(posterior[,,5], 2, mean),
                           TDI_median = apply(posterior[,,5], 2, median),
                           TDI_50_L = apply(posterior[,,5], 2, quantile, prob=0.25),
                           TDI_50_U = apply(posterior[,,5], 2, quantile, prob=0.75),
                           TDI_95_L = apply(posterior[,,5], 2, quantile, prob=0.025),
                           TDI_95_U = apply(posterior[,,5], 2, quantile, prob=0.975))

ETI <- apply(posterior[,,6], 1, function(q) pracma::trapz(fit$sDat$tPred, q)) 

data <- fit$data

save(data, post_summary, ETI, file = "results/summary_final_playoff.RData")
