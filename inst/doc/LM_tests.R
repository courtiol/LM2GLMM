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

## ----ghlt plot, fig.align = "center", fig.width = 8, fig.height = 4---------------------------------------------------
par(oma = c(0, 15, 0, 0))  ## increase left outer margin
plot(glht(mod_drink, linfct = mcp(drink = "Tukey")))

## ----drink------------------------------------------------------------------------------------------------------------
UK$drink2 <- relevel(UK$drink, ref = "Not at all")
summary(mod_drink2 <- lm(height ~ drink2, data = UK))$coef
## compare to summary(glht(mod_drink2, linfct = mcp(drink2 = "Dunnett")))

