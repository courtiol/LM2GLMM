## ----setup, include=FALSE------------------------------------------------
library(LM2GLMM)
library(spaMM)
library(lattice)
library(mvtnorm)
library(rgl)
library(car)
knitr::opts_chunk$set(cache = TRUE, cache.path = "./cache_knitr/LM_tests/", fig.path = "./fig_knitr/LM_tests/")

## ----non nested AIC, cache = FALSE---------------------------------------
AIC(mod_UK1)
AIC(mod_UK5)
exp((AIC(mod_UK5) - AIC(mod_UK1)) / 2) ## evidence ratio

