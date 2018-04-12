## ----setup, include=FALSE------------------------------------------------
library(LM2GLMM)
library(spaMM)
library(lattice)
library(mvtnorm)
library(rgl)
knitr::knit_hooks$set(webgl = hook_webgl) ## for rgl
knitr::opts_chunk$set(cache = TRUE, cache.path = "./cache_knitr/LM_uncertainty/", fig.path = "./fig_knitr/LM_uncertainty/", error = TRUE)

## ----profile for intercept, fig.align = "center", fig.width = 4, fig.height = 4, fig.show = "hold"----
# candidate_intercept_u was a sequence of values defined back on slide 7 (could be nice for clarity to redefine it here)
logLik_profile <- sapply(candidate_intercept_u, function(intercept)
  logLik(lm(size ~ 0 + humans_eaten + offset(rep(intercept, nrow(Alien))), data = Alien)))
plot(logLik_profile ~ candidate_intercept_u, type = "l")
abline(v = 50, col = "green", lwd = 2)
abline(v = coef(mod_stats)[1], col = "blue", lwd = 2)
#abline(v = confint(mod_stats)[1, ], col = "purple", lty = 2, lwd = 2)
#abline(h = logLik(mod_stats) - 0.5*qchisq(0.95, 1), col = "red", lty = 2, lwd = 2)

