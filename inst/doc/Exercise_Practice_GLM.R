## ----setup, include=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(LM2GLMM)
library(car)
knitr::opts_chunk$set(cache = TRUE, fig.align = "center", fig.width = 6, fig.height = 6,
                      cache.path = "./cache_knitr/Exo_GLM_solution/", fig.path = "./fig_knitr/Exo_GLM_solution/")
options(width = 200)

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
mod_insect <- lm(count ~ spray, data = InsectSprays)

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
InsectSprays$count_bis <- InsectSprays$count + 1
mod_insect_bis <- update(mod_insect, count_bis ~ .)
bc <- car::powerTransform(mod_insect_bis)
InsectSprays$count_bc <- car::bcPower(InsectSprays$count_bis, bc$lambda)
mod_insect_bc <- update(mod_insect_bis, count_bc ~ .)

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
mod_insect_glm <- glm(count ~ spray, data = InsectSprays, family = poisson())

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
table(mod_insect_glm$model$count)
testZeroInflation(insect_glm_resid)

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
mod_insect_glm_quasi <- glm(count ~ spray, data = InsectSprays, family = quasipoisson())
library(MASS)
mod_insect_glm_nb <- glm.nb(count ~ spray, data = InsectSprays)
rbind(deviance(mod_insect_glm),
      deviance(mod_insect_glm_quasi),
      deviance(mod_insect_glm_nb))

