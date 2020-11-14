library(HDInterval)
library(rstan)
library(parallel)

files <- list.files(pattern = "\\.RData$")

ETIs <- do.call("rbind", mclapply(files, function(f) {
  cat(f, "\n")
  load(f)
  posterior <- extract(fit$posterior, "pred")$pred
  eti <- apply(posterior[,,6], 1, function(q) pracma::trapz(fit$data$tPred, q))
  hp <- HDInterval::hdi(eti)
  
  out <- data.frame(file = f,
  	         mean = mean(eti), 
  	         var = var(eti), 
  	         median = median(eti), 
  	         hp["lower"], 
  	         hp["upper"])
  colnames(out) <- c("file", "mean", "var", "median", "lower", "upper")
  rownames(out) <- NULL
  out
}, mc.cores=64))

ETIs[order(ETIs$mean, decreasing=TRUE),][1:20,]
range(ETIs$mean)
txtplot::txtdensity(ETIs$mean)
range(ETIs$mean)

save(ETIs, file="../est_ETIs.RData")
