## ----setup, include=FALSE------------------------------------------------
library(LM2GLMM)
library(spaMM)
library(lattice)
library(mvtnorm)
library(rgl)
knitr::knit_hooks$set(webgl = hook_webgl) ## for rgl
knitr::opts_chunk$set(cache = TRUE, cache.path = "./cache_knitr/LM_uncertainty/", fig.path = "./fig_knitr/LM_uncertainty/")

