## ----setup, include=FALSE------------------------------------------------
library(LM2GLMM)
library(spaMM)
library(lattice)
library(mvtnorm)
library(rgl)
knitr::knit_hooks$set(webgl = hook_webgl) ## for rgl
knitr::opts_chunk$set(cache = TRUE, cache.path = "./cache_knitr/LM_uncertainty/", fig.path = "./fig_knitr/LM_uncertainty/", error = TRUE)

## ----coverage CI param boot proper 2-------------------------------------
CIboot2b <- function(rep = 100, i){
  data <- simulate_Aliens()
  mod <- update(mod_stats, data = data)
  newYs <- simulate(mod, nsim = rep)
  res_sim <- t(coef(lm(as.matrix(newYs) ~ humans_eaten, data = data)))
  boot.ci(boot.out = list(R = nrow(res_sim)),
          t0 = coef(mod)[[i]],
          t  = res_sim[, i, drop = FALSE],
          type = "basic")$basic[, 4:5]
}
set.seed(1L)
c(mean(replicate(5000, sum(findInterval(CIboot2b(i = 1), 50)) == 1)),
  mean(replicate(5000, sum(findInterval(CIboot2b(i = 2), 1.5)) == 1)))

## ----coverage CI param boot proper 3-------------------------------------
CIboot2c <- function(rep = 100, i){
  data <- simulate_Aliens()
  mod <- update(mod_stats, data = data)
  newYs <- simulate(mod, nsim = rep)
  res_sim <- t(coef(lm(as.matrix(newYs) ~ humans_eaten, data = data)))
  boot.ci(boot.out = list(R = nrow(res_sim)),
          t0 = coef(mod)[[i]],
          t  = res_sim[, i, drop = FALSE],
          var.t0 = vcov(mod)[i, i],
          var.t = rep(var(res_sim[, i]), nrow(res_sim)),
          type = "stud")$student[, 4:5]
}
set.seed(1L)
c(mean(replicate(5000, sum(findInterval(CIboot2c(i = 1), 50)) == 1)),
  mean(replicate(5000, sum(findInterval(CIboot2c(i = 2), 1.5)) == 1)))

## ----CI by profile intercept---------------------------------------------
where.is.limit.inter <- function(intercept, model_ref) {
  logLik_mod <- logLik(lm(size ~ 0 + humans_eaten + offset(rep(intercept, nrow(model_ref$model))),
                            data = model_ref$model))[[1]]
  logLik_goal <- logLik(model_ref)[[1]] - 0.5*qchisq(0.95, df = 1)
  return(abs(logLik_mod - logLik_goal))}
CI_intercept <- function(model_ref) {
  lwr <- optimise(where.is.limit.inter, interval = c(coef(model_ref)[1] - 10, coef(model_ref)[1]),
                  model_ref = model_ref)
  upr <- optimise(where.is.limit.inter, interval = c(coef(model_ref)[1], coef(model_ref)[1] + 10),
                  model_ref = model_ref)
  return(c(lwr = lwr$minimum, upr = upr$minimum))}
rbind("using Student" = confint(mod_stats)[1, ], "using bootstrap 1" = confint_boot[1, ],
      "using bootstrap 2" = confint_boot2[1, ], "using profile" = CI_intercept(mod_stats))

## ----CI by profile slope-------------------------------------------------
where.is.limit.slope <- function(slope, model_ref) {
  logLik_mod <- logLik(lm(size ~ 1 + offset(slope * humans_eaten),
                            data = model_ref$model))[[1]]
  logLik_goal <- logLik(model_ref)[[1]] - 0.5*qchisq(0.95, df = 1)
  return(abs(logLik_mod - logLik_goal))
}

CI_slope <- function(model_ref) {
  lwr <- optimise(where.is.limit.slope, interval = c(coef(model_ref)[2] - 1, coef(model_ref)[2]),
                  model_ref = model_ref)
  upr <- optimise(where.is.limit.slope, interval = c(coef(model_ref)[2], coef(model_ref)[2] + 1),
                  model_ref = model_ref)
  return(c(lwr = lwr$minimum, upr = upr$minimum))
}

rbind("using Student" = confint(mod_stats)[2, ], "using bootstrap 1" = confint_boot[2, ],
      "using bootstrap 2" = confint_boot2[2, ], "using profile" = CI_slope(mod_stats))

