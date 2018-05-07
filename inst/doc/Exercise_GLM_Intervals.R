## ----setup, include=FALSE------------------------------------------------
library(LM2GLMM)
library(car)
knitr::opts_chunk$set(cache = TRUE, fig.align = "center", fig.width = 6, fig.height = 6,
cache.path = "./cache_knitr/Exo_GLM_solution/", fig.path = "./fig_knitr/Exo_GLM_solution/")

## ------------------------------------------------------------------------
set.seed(1L)
Aliens <- simulate_Aliens_GLM()
mod_binar <- glm(happy ~ humans_eaten, family = binomial(), data = Aliens)
mod_poiss <- glm(eggs  ~ humans_eaten, family = poisson(),  data = Aliens)

## ------------------------------------------------------------------------
data.for.pred <- data.frame(humans_eaten = 0:15)
p <- predict(mod_binar, newdata = data.for.pred, se.fit = TRUE)
p$upr <- p$fit + qnorm(0.975) * p$se.fit
p$lwr <- p$fit + qnorm(0.025) * p$se.fit
p.mu <- data.frame(fit = plogis(p$fit),
                   upr = binomial()$linkinv(p$upr), ## same as plogis
                   lwr = 1/(1 + exp(-p$lwr))) ## same as plogis

plot(p.mu$fit ~ data.for.pred$humans_eaten, type = "l", lwd = 2, las = 1, ylim = c(0, 1),
     ylab = "Probability of being happy", xlab = "Number of humans eaten")
points(p.mu$upr ~  data.for.pred$humans_eaten, type = "l", lwd = 2, lty = 2)
points(p.mu$lwr ~  data.for.pred$humans_eaten, type = "l", lwd = 2, lty = 2)

## ---- message = FALSE----------------------------------------------------
library(spaMM)
mod_binar_spaMM <- fitme(happy ~ humans_eaten, family = binomial(), data = Aliens)
p <- predict(mod_binar_spaMM, newdata = data.for.pred, intervals = "predVar")
plot(p ~ data.for.pred$humans_eaten, type = "l", lwd = 2, las = 1, ylim = c(0, 1),
     ylab = "Probability of being happy", xlab = "Number of humans eaten")
points(attr(p, "interval")[, 1] ~  data.for.pred$humans_eaten, type = "l", lwd = 2, lty = 2)
points(attr(p, "interval")[, 2] ~  data.for.pred$humans_eaten, type = "l", lwd = 2, lty = 2)

## ------------------------------------------------------------------------
p <- predict(mod_poiss, newdata = data.for.pred, se.fit = TRUE)
p$upr <- p$fit + qnorm(0.975) * p$se.fit
p$lwr <- p$fit + qnorm(0.025) * p$se.fit
p.mu <- data.frame(fit = exp(p$fit),
                   upr = poisson()$linkinv(p$upr), ## same as exp
                   lwr = mod_poiss$family$linkinv(p$lwr)) ## same as exp
plot(p.mu$fit ~ data.for.pred$humans_eaten, type = "l", lwd = 2, las = 1,
     ylim = range(Aliens$eggs),
     ylab = "Number of eggs", xlab = "Number of humans eaten")
points(p.mu$upr ~  data.for.pred$humans_eaten, type = "l", lwd = 2, lty = 2)
points(p.mu$lwr ~  data.for.pred$humans_eaten, type = "l", lwd = 2, lty = 2)
points(jitter(Aliens$eggs) ~ Aliens$humans_eaten, cex = 0.5)
rug(x = jitter(Aliens$humans_eaten), col = "red", side = 3)

## ---- message = FALSE--------------------------------------------------
library(effects)
plot(allEffects(mod_poiss))

library(visreg)
visreg(mod_poiss, scale = "response")

