library(HDInterval)
library(rstan)
library(parallel)

files <- list.files(path = "../analysis/output/", pattern = "^fit-.*\\.RData$")
length(files) == 1143

ETIs <- do.call("rbind", mclapply(files, function(f) {
  cat(f, "\n")
  load(paste("../analysis/output/", f, sep=""))
  posterior <- extract(fit$posterior, "pred")$pred
  eti <- apply(posterior[,,6], 1, function(q) pracma::trapz(fit$sDat$tPred, q))
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

save(ETIs, file="../results/posterior_ETIs.RData")
