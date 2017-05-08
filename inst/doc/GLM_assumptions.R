## ----setup, include=FALSE---------------------------------------------------------------------------------------------
library(LM2GLMM)
library(spaMM)
library(car)
library(DHARMa)
library(pscl)
options(width = 120)
knitr::opts_chunk$set(cache = TRUE, cache.path = "./cache_knitr/GLM_resid/", fig.path = "./fig_knitr/GLM_resid/", fig.width = 5, fig.height = 5, fig.align = "center", error = TRUE)

## ---- cache = FALSE---------------------------------------------------------------------------------------------------
testZeroInflation(r, plot = TRUE)

