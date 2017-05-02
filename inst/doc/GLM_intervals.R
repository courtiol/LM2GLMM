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
X <- model.matrix(mod_poiss)
W <- matrix(0, ncol = nrow(mod_poiss$model), nrow = nrow(mod_poiss$model))
diag(W) <- mod_poiss$weights
t(X) %*% W %*% X
(XTWX <- crossprod(mod_poiss$R))

## ---------------------------------------------------------------------------------------------------------------------
vcov(mod_poiss)
phi <- 1
phi*solve(XTWX)

## ---------------------------------------------------------------------------------------------------------------------
vcov(mod_gauss)
XTWX <- crossprod(mod_gauss$R)
phi <- mod_gauss$deviance / mod_gauss$df.residual
phi*solve(XTWX)

## ---------------------------------------------------------------------------------------------------------------------
vcov(mod_binar)
XTWX <- crossprod(mod_binar$R)
phi <- 1
phi*solve(XTWX)

## ---------------------------------------------------------------------------------------------------------------------
vcov(mod_binom)
XTWX <- crossprod(mod_binom$R)
phi <- 1
phi*solve(XTWX)

## ---------------------------------------------------------------------------------------------------------------------
set.seed(1L)
x <- seq(1, 2, length = 100)
y <- exp(rnorm(n = length(x), mean = 2 + 1 * x, sd = 0.5))

mod_lm   <- lm(log(y) ~ x)
mod_glm  <- glm(y ~ x, family = gaussian(link = "log"))
logLikH0 <- replicate(1000, {
  new.y <- exp(as.matrix(simulate(mod_lm)))
  logLik(lm(log(new.y) ~ x)) - logLik(glm(new.y ~ x, family = gaussian(link = "log")))
  })
mean(logLikH0 < (logLik(mod_lm)-logLik(mod_glm)))

## ---------------------------------------------------------------------------------------------------------------------
set.seed(1L)
x <- seq(1, 2, length = 100)
y <- exp(2 + 1 * x) + rnorm(n = length(x), mean = 0, sd = 5)

mod_lm   <- lm(log(y) ~ x)
mod_glm  <- glm(y ~ x, family = gaussian(link = "log"))
difflogLikH0 <- replicate(1000, {
  new.y <- as.matrix(simulate(mod_glm))
  logLik(glm(new.y ~ x, family = gaussian(link = "log"))) - logLik(lm(log(new.y) ~ x))
  })
hist(difflogLikH0)
abline(v = logLik(mod_glm)-logLik(mod_lm), col = 2, lwd = 2)
mean((logLik(mod_glm)-logLik(mod_lm)) > logLikH0)


c(mean(logLikH0), mean(logLik(mod_glm)-logLik(mod_lm)))

## ---------------------------------------------------------------------------------------------------------------------
set.seed(1L)
x <- seq(1, 2, length = 100)
y <- exp(2 + 1 * x) + rnorm(n = length(x), mean = 0, sd = 5)
mod_glm  <- glm(y ~ x, family = gaussian(link = "log"))
logLikBoot <- replicate(1000, {
  new.y <- as.matrix(simulate(mod_glm))
  logLik(glm(new.y ~ x, family = gaussian(link = "log")))
  })
hist(logLikBoot, xlim = range(logLikBoot, logLik(mod_glm)))
abline(v = logLik(mod_glm), col = 2, lwd = 2)
mean(logLik(mod_glm) > logLikBoot)

