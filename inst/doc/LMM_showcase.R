## ----setup, include=FALSE---------------------------------------------------------------------------------------------
library(LM2GLMM)
library(spaMM)
library(lme4)
library(car)
spaMM.options(nb_cores = 4L)
options(width = 120)
knitr::opts_chunk$set(cache = TRUE, cache.path = "./cache_knitr/LMM_showcase/", fig.path = "./fig_knitr/LMM_showcase/", fig.width = 5, fig.height = 5, fig.align = "center")

