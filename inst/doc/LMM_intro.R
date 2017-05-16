## ----setup, include=FALSE-------------------------------------------------------------------------
library(LM2GLMM)
library(spaMM)
library(lme4)
library(car)
options(width = 100)
knitr::opts_chunk$set(cache = TRUE, cache.path = "./cache_knitr/LMM_intro/", fig.path = "./fig_knitr/LMM_intro/", fig.width = 5, fig.height = 5, fig.align = "center")

## ---- cache = FALSE-------------------------------------------------------------------------------
Aliens

## ---- cache = FALSE-------------------------------------------------------------------------------
library(lme4)
(mod <- lmer(y ~ x + (1|group), data = Aliens, REML = FALSE))
c(var.group = as.numeric(attr(VarCorr(mod)$group, "stddev")^2), var.error = as.numeric(attr(VarCorr(mod), "sc")^2))

## ---- cache = FALSE-------------------------------------------------------------------------------
library(spaMM)
(mod <- fitme(y ~ x + (1|group), data = Aliens, method = "ML"))

## ---- cache = FALSE-------------------------------------------------------------------------------
plot(ecdf(test))
abline(0, 1, col = "red")

## ---- cache = FALSE-------------------------------------------------------------------------------
plot(ecdf(test2))
abline(0, 1, col = "red")

## ---- cache = FALSE-------------------------------------------------------------------------------
mod2

## ---- cache = FALSE-------------------------------------------------------------------------------
as.data.frame(ranef(mod2))

