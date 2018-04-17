## ----setup, include=FALSE---------------------------------------------------------------------------------------------
options(width = 120)
library(LM2GLMM)
library(spaMM)
library(lattice)
library(mvtnorm)
library(rgl)
library(car)
spaMM.options(nb_cores = 4L)
knitr::opts_chunk$set(cache = TRUE, cache.path = "./cache_knitr/LM_tests/", fig.path = "./fig_knitr/LM_tests/")

