## ----setup, include=FALSE-------------------------------------------------------------------------
library(LM2GLMM)
library(spaMM)
library(lme4)
library(car)
options(width = 100)
knitr::opts_chunk$set(cache = TRUE, cache.path = "./cache_knitr/LMM_intro/", fig.path = "./fig_knitr/LMM_intro/", fig.width = 5, fig.height = 5, fig.align = "center")

## ---- cache = FALSE-------------------------------------------------------------------------------
c(mod$I2, mod$H2, mod$QE) ## from metafor

get_meta_metrics <- function(model, vi) {
  wi <- 1/vi  ## weights = inverse of sampling variance
  k <- length(model$ranef)  ## number of groups in random term (here number of studies)
  p <- length(model$fixef)  ## number of fixed effect parameters
  W <- diag(wi, nrow = k, ncol = k)  ## matrix of weights
  X <- as.matrix(as.data.frame(model$X.pv))  ## design matrix
  stXWX <- solve(t(X) %*% W %*% X)  ## weighted vcov of estimates
  P <- W - W %*% X %*% stXWX %*% crossprod(X, W)  ## weighted vcov of ??
  vi.avg <- (k - p) / sum(diag(P))
  I2 <- as.numeric(100 * model$lambda / (vi.avg + model$lambda) )
  H2 <- as.numeric((vi.avg + model$lambda) / vi.avg)
  QE <- max(0, c(crossprod(model$y, P) %*% model$y))
  return(c(I2 = I2, H2 = H2, QE = QE, vi.avg = vi.avg))
}

get_meta_metrics(mod_spaMM, dat.bcg$sampling.var) ## add vi.avg: weighted mean within study sampling variance

## ---- fig.width = 6, fig.height = 5.5, cache = FALSE----------------------------------------------
metafor::plot.rma.uni(mod)  ## or just plot(mod)

