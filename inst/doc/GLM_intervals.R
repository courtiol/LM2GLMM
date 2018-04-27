## ----setup, include=FALSE---------------------------------------------------------------------------------------------
library(LM2GLMM)
library(car)
library(MASS)
library(spaMM)
spaMM.options(nb_cores = 4L)
options(width = 120)
knitr::opts_chunk$set(cache = TRUE, cache.path = "./cache_knitr/GLM_uncertainty/", fig.path = "./fig_knitr/GLM_uncertainty/", fig.width = 4, fig.height = 4, fig.align = "center", error = TRUE)

