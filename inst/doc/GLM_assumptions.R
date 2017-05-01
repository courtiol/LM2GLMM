## ----setup, include=FALSE---------------------------------------------------------------------------------------------
library(LM2GLMM)
options(width = 120)
knitr::opts_chunk$set(cache = FALSE, fig.width = 5, fig.height = 5, fig.align = "center")

## ---------------------------------------------------------------------------------------------------------------------
set.seed(1L)
Aliens <- data.frame(humans_eaten = round(runif(n = 100, min = 0, max = 15)))
Aliens$size  <- rnorm( n = 100, mean = 50 + 1.5 * Aliens$humans_eaten, sd = 5)
Aliens$eggs  <- rpois( n = 100, lambda = exp(-1 + 0.1 * Aliens$humans_eaten))
Aliens$happy <- rbinom(n = 100, size = 1, prob = plogis(-3 + 0.3 * Aliens$humans_eaten))
Aliens$all_eyes  <- round(runif(nrow(Aliens), min = 1, max = 12))
Aliens$blue_eyes <- rbinom(n = nrow(Aliens), size = Aliens$all_eyes, prob = plogis(-2 + 0.5 * Aliens$humans_eaten))
Aliens$pink_eyes <- Aliens$all_eyes - Aliens$blue_eyes

mod_gauss <- glm(size  ~ humans_eaten, family = gaussian(), data = Aliens)
mod_poiss <- glm(eggs  ~ humans_eaten, family = poisson(),  data = Aliens)
mod_binar <- glm(happy ~ humans_eaten, family = binomial(), data = Aliens)
mod_binom <- glm(cbind(blue_eyes, pink_eyes) ~ humans_eaten, family = binomial(), data = Aliens)

## ---------------------------------------------------------------------------------------------------------------------
rbind(residuals(mod_poiss, type = "deviance")[1:2],
      residuals(mod_poiss, type = "pearson")[1:2],
      residuals(mod_poiss, type = "working")[1:2],
      residuals(mod_poiss, type = "response")[1:2]
      )

## ---------------------------------------------------------------------------------------------------------------------
rbind(residuals(mod_binom, type = "deviance")[1:2],
      residuals(mod_binom, type = "pearson")[1:2],
      residuals(mod_binom, type = "working")[1:2],
      residuals(mod_binom, type = "response")[1:2]
      )

## ---- echo = FALSE----------------------------------------------------------------------------------------------------
par(las = 1)
plot(ecdf(residuals(mod_poiss, type = "working")), col = 3, main = "mod_poiss")
plot(ecdf(residuals(mod_poiss, type = "deviance")), col = 1, add = TRUE)
plot(ecdf(residuals(mod_poiss, type = "pearson")), col = 2, add = TRUE)
plot(ecdf(residuals(mod_poiss, type = "response")), col = 4, add = TRUE)
legend("bottomright", fill = 1:4, bty = "n", legend = c("deviance", "pearson", "working", "response"), title = "Type of residuals:")

## ---- echo = FALSE----------------------------------------------------------------------------------------------------
par(las = 1)
plot(ecdf(residuals(mod_binom, type = "working")), col = 3, main = "mod_binom")
plot(ecdf(residuals(mod_binom, type = "deviance")), col = 1, add = TRUE)
plot(ecdf(residuals(mod_binom, type = "pearson")), col = 2, add = TRUE)
plot(ecdf(residuals(mod_binom, type = "response")), col = 4, add = TRUE)
legend("topleft", fill = 1:4, bty = "n", legend = c("deviance", "pearson", "working", "response"), title = "Type of residuals:")

## ---------------------------------------------------------------------------------------------------------------------
rbind(residuals(mod_gauss, type = "response")[1:2],
      (mod_gauss$y - mod_gauss$fitted.values)[1:2])
rbind(residuals(mod_poiss, type = "response")[1:2],
      (mod_poiss$y - mod_poiss$fitted.values)[1:2])
rbind(residuals(mod_binom, type = "response")[1:2],
      (mod_binom$y - mod_binom$fitted.values)[1:2])

## ---------------------------------------------------------------------------------------------------------------------
variances <- poisson()$variance(mod_poiss$fitted.values)
rbind(residuals(mod_poiss, type = "pearson")[1:2],
      (residuals(mod_poiss, type = "response") * sqrt(mod_poiss$prior.weights / variances))[1:2])

variances <- binomial()$variance(mod_binom$fitted.values)
rbind(residuals(mod_binom, type = "pearson")[1:2],
      (residuals(mod_binom, type = "response") * sqrt(mod_binom$prior.weights / variances))[1:2])

## ---------------------------------------------------------------------------------------------------------------------
residuals(mod_poiss, type = "deviance")[1:2]
rbind((residuals(mod_poiss, type = "deviance")[1:2])^2,
      with(mod_poiss, poisson()$dev.resids(y = y, mu = fitted.values, wt = prior.weights))[1:2])

## ---------------------------------------------------------------------------------------------------------------------
c(sum(residuals(mod_gauss, type = "deviance")^2), deviance(mod_gauss))
c(sum(residuals(mod_poiss, type = "deviance")^2), deviance(mod_poiss))
c(sum(residuals(mod_binom, type = "deviance")^2), deviance(mod_binom))

## ---------------------------------------------------------------------------------------------------------------------
rbind(mod_poiss$residuals[1:2],
      ((mod_poiss$y - mod_poiss$fitted.values)/poisson()$mu.eta(mod_poiss$linear.predictors))[1:2])

rbind(mod_binom$residuals[1:2],
      ((mod_binom$y - mod_binom$fitted.values)/binomial()$mu.eta(mod_binom$linear.predictors))[1:2])

## ---------------------------------------------------------------------------------------------------------------------
mod_UK <- glm(milk ~ drink + sex + cigarettes, data = UK[1:10, ], family = poisson())
residuals(mod_UK, type = "partial")

