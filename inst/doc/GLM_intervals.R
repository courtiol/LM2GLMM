## ----setup, include=FALSE---------------------------------------------------------------------------------------------
library(LM2GLMM)
library(car)
library(MASS)
library(spaMM)
options(width = 120)
knitr::opts_chunk$set(cache = TRUE, cache.path = "./cache_knitr/GLM_uncertainty/", fig.path = "./fig_knitr/GLM_uncertainty/", fig.width = 4, fig.height = 4, fig.align = "center")

## ---- cache = FALSE---------------------------------------------------------------------------------------------------
summary(mod_binar2)$coef
z <- (mod_binar2$coef - 0) / sqrt(diag(vcov(mod_binar2)))
pvalues <- 2 * (1 - pnorm(abs(z)))
cbind(z, pvalues)

