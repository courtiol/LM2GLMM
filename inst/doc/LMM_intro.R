## ----setup, include=FALSE-------------------------------------------------------------------------
library(LM2GLMM)
options(width = 100)
knitr::opts_chunk$set(cache = FALSE, cache.path = "./cache_knitr/LMM_intro/", fig.path = "./fig_knitr/LMM_intro/", fig.width = 5, fig.height = 5, fig.align = "center")

## -------------------------------------------------------------------------------------------------
var.error <- 2
var.between.group <- 10

set.seed(1L)
group.effect <- rnorm(10, mean = 0, sd = sqrt(var.between.group))
Aliens <- data.frame(
  group.speed = rep(group.effect, each = 10),  ## 10 individuals for each 10 group
  group.id = factor(paste0("id_", rep(1:10, each = 10))),
  individual.effect = rnorm(100, mean = 0, sd = sqrt(var.error))
  )
Aliens$individual.speed <- Aliens$group.speed + Aliens$individual.effect

rbind(Aliens[1:2, ], Aliens[11:12, ], Aliens[21:22, ])

## ---- message = FALSE-----------------------------------------------------------------------------
library(lme4)
mod <- lmer(individual.speed ~ 0 + (1|group.id), data = Aliens)
as.numeric(VarCorr(mod)[[1]])  ## variance between groups
sigma(mod)^2  ## residual variance

## ---- message = FALSE-----------------------------------------------------------------------------
library(spaMM)
mod2 <- fitme(individual.speed ~ 0 + (1|group.id), data = Aliens, method = "REML")
as.numeric(mod2$lambda[[1]])
mod2$phi  ## residual variance

