## ----setup, include=FALSE---------------------------------------------------------------------------------------------
library(LM2GLMM)
options(width = 120)
knitr::opts_chunk$set(cache = FALSE, fig.width = 5, fig.height = 5, fig.align = "center")

## ----family, eval = FALSE---------------------------------------------------------------------------------------------
#  gaussian(link = "identity")
#  binomial(link = "logit")
#  poisson(link = "log")
#  Gamma(link = "inverse")
#  inverse.gaussian(link = "1/mu^2")

## ---------------------------------------------------------------------------------------------------------------------
set.seed(1L)
Aliens <- data.frame(humans_eaten = round(runif(n = 100, min = 0, max = 15)))
Aliens$size  <- rnorm( n = 100, mean = 50 + 1.5 * Aliens$humans_eaten, sd = 5)
Aliens$eggs  <- rpois( n = 100, lambda = exp(-1 + 0.1 * Aliens$humans_eaten))
Aliens$happy <- rbinom(n = 100, size = 1, prob = plogis(-3 + 0.3 * Aliens$humans_eaten))
Aliens$all_eyes  <- round(runif(nrow(Aliens), min = 1, max = 12))
Aliens$blue_eyes <- rbinom(n = nrow(Aliens), size = Aliens$all_eyes, prob = plogis(-2 + 0.5 * Aliens$humans_eaten))
Aliens$pink_eyes <- Aliens$all_eyes - Aliens$blue_eyes
head(Aliens)

## ---------------------------------------------------------------------------------------------------------------------
mod_gauss <- glm(size  ~ humans_eaten, family = gaussian(), data = Aliens)
mod_poiss <- glm(eggs  ~ humans_eaten, family = poisson(),  data = Aliens)
mod_binar <- glm(happy ~ humans_eaten, family = binomial(), data = Aliens)
mod_binom <- glm(cbind(blue_eyes, pink_eyes) ~ humans_eaten, family = binomial(), data = Aliens)

## Example of output:
mod_binom

## ---------------------------------------------------------------------------------------------------------------------
names(mod_gauss)
names(mod_binom)

## ---------------------------------------------------------------------------------------------------------------------
mod_gauss$coefficients
mod_poiss$coefficients
mod_binom$coefficients

## ---------------------------------------------------------------------------------------------------------------------
rbind(mod_poiss$residuals[1:2], residuals(mod_poiss, type = "working")[1:2])
rbind(mod_binom$residuals[1:2], residuals(mod_binom, type = "working")[1:2])

## ---------------------------------------------------------------------------------------------------------------------
rbind(summary(mod_gauss$linear.predictors),
      summary(mod_poiss$linear.predictors),
      summary(mod_binar$linear.predictors),
      summary(mod_binom$linear.predictors)
      )

## ---------------------------------------------------------------------------------------------------------------------
rbind(mod_gauss$linear.predictors[1:2], (model.matrix(mod_gauss) %*% mod_gauss$coefficients)[1:2])
rbind(mod_poiss$linear.predictors[1:2], (model.matrix(mod_poiss) %*% mod_poiss$coefficients)[1:2])
rbind(mod_binom$linear.predictors[1:2], (model.matrix(mod_binom) %*% mod_binom$coefficients)[1:2])

## ---------------------------------------------------------------------------------------------------------------------
rbind(summary(mod_gauss$fitted.values),
      summary(mod_poiss$fitted.values),
      summary(mod_binom$fitted.values)
      )

## ---------------------------------------------------------------------------------------------------------------------
summary(mod_binar$fitted.values)

## ---------------------------------------------------------------------------------------------------------------------
rbind(mod_gauss$fitted.values[1:2], gaussian()$linkinv(mod_gauss$linear.predictors[1:2]))
rbind(mod_poiss$fitted.values[1:2], poisson()$linkinv(mod_poiss$linear.predictors[1:2]))
rbind(mod_binom$fitted.values[1:2], binomial()$linkinv(mod_binom$linear.predictors[1:2]))

## ---------------------------------------------------------------------------------------------------------------------
mod_poiss_sat <- update(mod_poiss, . ~ as.factor(mod_poiss$y))
c(mod_poiss$deviance, -2*(logLik(mod_poiss) - logLik(mod_poiss_sat)))
mod_poiss_null <- update(mod_poiss, . ~ 1)
c(mod_poiss$null.deviance, mod_poiss_null$deviance)

## ---------------------------------------------------------------------------------------------------------------------
mod_binom_sat <- update(mod_binom, . ~ as.factor(mod_binom$y))
c(mod_binom$deviance, -2*(logLik(mod_binom) - logLik(mod_binom_sat)))
mod_binom_null <- update(mod_binom, . ~ 1)
c(mod_binom$null.deviance, mod_binom_null$deviance)

## ---------------------------------------------------------------------------------------------------------------------
c(mod_gauss$aic, -2*logLik(mod_gauss) + 2*(mod_gauss$rank + 1))  ## + 1 for the estimate of the variance
c(mod_poiss$aic, -2*logLik(mod_poiss) + 2*(mod_poiss$rank))
c(mod_binom$aic, -2*logLik(mod_binom) + 2*(mod_binom$rank))

## ---------------------------------------------------------------------------------------------------------------------
logLik(mod_poiss)
mod_poiss_double <- glm(eggs  ~ humans_eaten, family = poisson(), weights = rep(2, nrow(Aliens)), data = Aliens)

Aliens_double <- Aliens[rep(1:nrow(Aliens), each = 2), ]
mod_poiss_double_bis <- glm(eggs  ~ humans_eaten, family = poisson(),  data = Aliens_double)
c(logLik(mod_poiss_double), logLik(mod_poiss_double_bis))

mod_poiss_double$prior.weights[1:4]

## ---------------------------------------------------------------------------------------------------------------------
mod_binom$prior.weights[1:6]

## ---------------------------------------------------------------------------------------------------------------------
Aliens$prop_blue <- Aliens$blue_eyes / Aliens$all_eyes
mod_binom_bis  <- glm(prop_blue ~ humans_eaten, weights = all_eyes, data = Aliens, family = binomial())
c(logLik(mod_binom), logLik(mod_binom_bis))

## ---------------------------------------------------------------------------------------------------------------------
as.data.frame(
  cbind("gaussian" = c(names(gaussian()), NA), "poisson" = names(poisson()), "binomial" = names(binomial()))
  )

## ---- eval = FALSE----------------------------------------------------------------------------------------------------
#  print.AsIs(gaussian())  ## Tip: print.AsIS display the 'true' content of an object
#  print.AsIs(poisson())

## ---------------------------------------------------------------------------------------------------------------------
c(gaussian()$link, poisson()$link, binomial()$link)
c(poisson()$linkfun(12), log(12))
c(binomial()$linkfun(0.9), log(0.9/(1 - 0.9)))

## ---- eval = FALSE----------------------------------------------------------------------------------------------------
#  c(gaussian(link = "identity")$linkfun(12), 12)
#  c(gaussian(link = "log")$linkfun(12), log(12))
#  c(gaussian(link = "inverse")$linkfun(12), 1/12)
#  
#  c(poisson (link = "log")$linkfun(12), log(12))
#  c(poisson (link = "identity")$linkfun(12), 12)
#  c(poisson (link = "sqrt")$linkfun(12), sqrt(12))
#  
#  c(binomial(link = "logit")$linkfun(0.9), log(0.9/(1 - 0.9)))
#  c(binomial(link = "probit")$linkfun(0.9), qnorm(0.9))
#  c(binomial(link = "cauchit")$linkfun(0.9), qcauchy(0.9))
#  c(binomial(link = "log")$linkfun(0.9), log(0.9))
#  c(binomial(link = "cloglog")$linkfun(0.9), log(-log(1 - 0.9)))

## ---------------------------------------------------------------------------------------------------------------------
gaussian()$linkinv(12)
c(poisson()$linkinv(2.484907), exp(2.484907))
c(binomial()$linkinv(2.197225), plogis(2.197225), 1/(1 + exp(-2.197225)))

## ---------------------------------------------------------------------------------------------------------------------
gaussian()$variance(1:10)  ## = constant
poisson()$variance(1:10) ## = lambda
binomial()$variance(seq(0, 1, 0.1))  ## = p x q

## ---------------------------------------------------------------------------------------------------------------------
with(mod_gauss, gaussian()$dev.resids(y = y, mu = fitted.values, wt = prior.weights))[1:2]

sigma2_error <- sum(summary(mod_gauss)$deviance.resid^2) / mod_gauss$df.residual
sigma2_resid <- sigma2_error *  mod_gauss$df.residual / nrow(mod_gauss$model)
deviance.pt <- -2 * sigma2_resid * (dnorm(x = mod_gauss$y, mean = mod_gauss$fitted.values,
                                          sd = rep(sqrt(sigma2_resid), length(mod_gauss$y)), log = TRUE) - 
                                    dnorm(x = mod_gauss$y, mean = mod_gauss$y,
                                          sd = rep(sqrt(sigma2_resid), length(mod_gauss$y)), log = TRUE))
(mod_gauss$prior.weight * deviance.pt)[1:2] 

## ---------------------------------------------------------------------------------------------------------------------
with(mod_poiss, poisson()$dev.resids(y = y, mu = fitted.values, wt = prior.weights))[1:2]

deviance.pt  <- -2 * (dpois(x = mod_poiss$y, lambda = mod_poiss$fitted.values, log = TRUE) - 
                      dpois(x = mod_poiss$y, lambda = mod_poiss$y, log = TRUE))

(mod_poiss$prior.weight * deviance.pt)[1:2]

## ---------------------------------------------------------------------------------------------------------------------
with(mod_binar, binomial()$dev.resids(y = y, mu = fitted.values, wt = prior.weights))[1:2]

deviance.pt  <- -2 * (dbinom(x    = mod_binar$y,
                             size = 1,
                             prob = mod_binar$fitted.values, log = TRUE) - 
                      dbinom(x    = mod_binar$y,
                             size = 1,
                             prob = mod_binar$y, log = TRUE))

(mod_poiss$prior.weight * deviance.pt)[1:2]

## ---------------------------------------------------------------------------------------------------------------------
with(mod_binom, binomial()$dev.resids(y = y, mu = fitted.values, wt = prior.weights))[1:2]

deviance.pt  <- -2 * (dbinom(x    = mod_binom$y*mod_binom$prior.weights,
                             size = mod_binom$prior.weights,
                             prob = mod_binom$fitted.values, log = TRUE) - 
                      dbinom(x    = mod_binom$y*mod_binom$prior.weights,
                             size = mod_binom$prior.weights,
                             prob = mod_binom$y, log = TRUE))

deviance.pt[1:2]  ## here the prior.weights directly considered

## ---------------------------------------------------------------------------------------------------------------------
my_glm1 <- function(formula, data, family, weights = NULL) {

  useful.data <- model.frame(formula = formula, data = data)  ## keep only useful rows and columns
  X <- model.matrix(object = formula, data = useful.data)     ## build design matrix
  y <- model.response(useful.data)                            ## extract response
  mu.eta <- family$mu.eta  ## extract function to compute d mu/d eta
  nobs <- NROW(y) ## count the rows of observations (not nrow() as y can be a vector or a matrix)
  if (is.null(weights)) weights <- rep(1, nobs)  ## set the prior.weights if NULL

  etastart <- start <- mustart <- NULL  ## required for family$initialize
  eval(family$initialize)               ## recomputes y and prior weights if binomial + cbind
  
  get_resid_deviance <- function(coef) {  ## define function computing the scaled residual deviance
    eta <- as.numeric(X %*% coef)   ## compute the linear predictors
    mu  <- family$linkinv(eta)      ## express the linear predictor in the scale of the response
    dev <- sum(family$dev.resids(y, mu, weights))  ## compute the deviance, mind that here it is still the prior weight
    return(dev)  ## return the scaled residual deviance
  }
  
  fit <- nlminb(rep(0, ncol(X)), get_resid_deviance) ## optimisation; same as optim() but seems to work better
  if (fit$convergence != 0) warning("the algorithm did not converge")  ## test if convergence has been reached
  return(fit$par)  ## returns estimates
}

## ---------------------------------------------------------------------------------------------------------------------
my_glm2 <- function(formula, data, family, weights = NULL) {
  
  useful.data <- model.frame(formula = formula, data = data)  ## keep only useful rows and columns
  X <- model.matrix(object = formula, data = useful.data)     ## build design matrix
  y <- model.response(useful.data)                            ## extract response
  nobs <- NROW(y) ## count the rows of observations (not nrow() as y can be a vector or a matrix)
  if (is.null(weights)) weights <- rep(1, nobs)  ## set the prior.weights if NULL

  etastart <- start <- mustart <- NULL  ## required for family$initialize
  eval(family$initialize)         ## creates mustart -- the initial values for mu -- and 
                                  ## recomputes y and weights if binomial + cbind
  eta <- family$linkfun(mustart)  ## compute initial values for eta from mustart
  
  for (iter in 1:25L) {
    mu <- family$linkinv(eta)    ## compute the fitted values
    deriv <- family$mu.eta(eta)  ## compute the derivative d mu/d eta
    Z <- eta + (y - mu)/deriv    ## compute the adjusted response
    W <- weights*deriv^2/family$variance(mu)  ## compute the working weights
    lm.fit <- lm(Z ~ X - 1, weights = W)  ## lm with weights
    beta <- lm.fit$coef  ## extract the estimates
    eta <- X %*% beta    ## compute the linear predictor
  }
  return(beta)  ## returns estimates
}

## ---------------------------------------------------------------------------------------------------------------------
my_glm3 <- function(formula, data, family, weights = NULL) {
  useful.data <- model.frame(formula = formula, data = data)  ## keep only useful rows and columns
  X <- model.matrix(object = formula, data = useful.data)     ## build design matrix
  y <- model.response(useful.data)                            ## extract response
  nobs <- NROW(y) ## count the rows of observations (not nrow() as y can be a vector or a matrix)
  if (is.null(weights)) weights <- rep(1, nobs)  ## set the prior.weights if NULL
  etastart <- start <- mustart <- NULL  ## required for family$initialize
  eval(family$initialize)         ## creates mustart -- the initial values for mu -- and 
                                  ## recomputes y and weights if binomial + cbind
  eta <- family$linkfun(mustart)  ## compute initial values for eta from mustart
  devold <- Inf  ## set old residual deviance | glm.fit does something more clever here
  for (iter in 1:25L) {
    mu <- family$linkinv(eta)    ## compute the fitted values
    deriv <- family$mu.eta(eta)  ## compute the derivative d mu/d eta
    Z <- eta + (y - mu)/deriv    ## compute the adjusted response
    W <- weights*deriv^2/family$variance(mu)  ## compute the working weights

    dev <- sum(family$dev.resids(y, mu, weights))        ## compute residual deviance
    if (abs(dev - devold)/(0.1 + abs(dev)) < 1e-7) break ## leave loop if it has converged
    devold <- dev  ## update residual deviance
    QR <- qr(X*sqrt(W))  ## perform the QR decomposition | glm.fit uses C_Cdqrls directly.
    beta <- qr.coef(QR, Z*sqrt(W))  ## compute coefficients | glm.fit uses C_Cdqrls directly.
    eta <- as.numeric(X %*% beta)  ## compute the predicted values on the scale of the linear predictor
  }
  if (iter == 25L) warning("the algorithm did not converge after 25 iterations...")
  return(beta)  ## returns estimates
}

## ---------------------------------------------------------------------------------------------------------------------
rbind(mod_gauss$coefficients,
      my_glm1(size ~ humans_eaten, data = Aliens, family = gaussian()),
      my_glm2(size ~ humans_eaten, data = Aliens, family = gaussian()),
      my_glm3(size ~ humans_eaten, data = Aliens, family = gaussian())
      )

## ---------------------------------------------------------------------------------------------------------------------
rbind(mod_poiss$coefficients,
      my_glm1(eggs ~ humans_eaten, data = Aliens, family = poisson()),
      my_glm2(eggs ~ humans_eaten, data = Aliens, family = poisson()),
      my_glm3(eggs ~ humans_eaten, data = Aliens, family = poisson())
      )

## ---------------------------------------------------------------------------------------------------------------------
rbind(mod_binar$coefficients,
      my_glm1(happy ~ humans_eaten, data = Aliens, family = binomial()),
      my_glm2(happy ~ humans_eaten, data = Aliens, family = binomial()),
      my_glm3(happy ~ humans_eaten, data = Aliens, family = binomial())
      )

## ---------------------------------------------------------------------------------------------------------------------
rbind(mod_binom$coefficients,
      my_glm1(cbind(blue_eyes, pink_eyes) ~ humans_eaten, data = Aliens, family = binomial()),
      my_glm2(cbind(blue_eyes, pink_eyes) ~ humans_eaten, data = Aliens, family = binomial()),
      my_glm3(cbind(blue_eyes, pink_eyes) ~ humans_eaten, data = Aliens, family = binomial())
      )

## ---------------------------------------------------------------------------------------------------------------------
rbind(mod_binom$coefficients,
      my_glm1(prop_blue ~ humans_eaten, data = Aliens, family = binomial(), weights = Aliens$all_eyes),
      my_glm2(prop_blue ~ humans_eaten, data = Aliens, family = binomial(), weights = Aliens$all_eyes),
      my_glm3(prop_blue ~ humans_eaten, data = Aliens, family = binomial(), weights = Aliens$all_eyes)
      )

