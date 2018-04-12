## ----setup, include=FALSE---------------------------------------------------------------------------------------------
library(LM2GLMM)
options(width = 120)
knitr::opts_chunk$set(cache = TRUE, cache.path = "./cache_knitr/LM_point/", fig.path = "./fig_knitr/LM_point/")

## ----def comput_logLik_Alien------------------------------------------------------------------------------------------
compute_logLik <- function(vector.param, formula, data) {
  useful.data <- model.frame(formula = formula, data = data)
  X <- model.matrix(object = formula, data = useful.data)
  predicts <- X %*% matrix(vector.param)
  response <- model.response(useful.data)
  sigma2_resid <- sum((response - predicts)^2) / nrow(useful.data)
  logL <- sum(dnorm(response, mean = predicts, sd = sqrt(sigma2_resid), log = TRUE))
  return(logL)
}

(theta.lm <- c("intercept" = coef_lm[1][[1]], "slope" = coef_lm[2][[1]])) ## For testing
compute_logLik(vector.param = theta.lm, formula = size ~ humans_eaten, data = Alien)

## ----def compute_rss_Alien--------------------------------------------------------------------------------------------
compute_rss <- function(vector.param, formula, data) {
  useful.data <- model.frame(formula = formula, data = data)
  X <- model.matrix(object = formula, data = useful.data)
  predicts <- X %*% matrix(vector.param)
  response <- model.response(useful.data)
  rss <- sum((response - predicts)^2)
  return(rss)
}

compute_rss(vector.param = theta.lm, formula = size ~ humans_eaten, data = Alien)  ## For testing
optim(c("intercept" = 0, "slope" = 1), compute_rss, formula = size ~ humans_eaten, data = Alien)$par

