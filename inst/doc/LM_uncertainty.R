## ----setup, include=FALSE------------------------------------------------
library(LM2GLMM)
library(spaMM)
library(lattice)
library(mvtnorm)
library(rgl)
spaMM.options(nb_cores = 4L)
knitr::knit_hooks$set(webgl = hook_webgl) ## for rgl
knitr::opts_chunk$set(cache = TRUE, cache.path = "./cache_knitr/LM_uncertainty/", fig.path = "./fig_knitr/LM_uncertainty/", error = TRUE)

## ----point estimates-----------------------------------------------------
set.seed(123L)
Alien <- data.frame(humans_eaten = sample(1:12))
Alien$size <- rnorm(n = 12, mean = 50 + 1.5*Alien$humans_eaten, sd = sqrt(25))

mod_stats <- lm(size ~ humans_eaten, data = Alien)
c(coef(mod_stats), sigma2_error <- summary(mod_stats)$sigma^2)

mod_spaMM_ML   <- fitme(size ~ humans_eaten, data = Alien, method = "ML")
mod_spaMM_REML <- fitme(size ~ humans_eaten, data = Alien, method = "REML")
c(mod_spaMM_ML$fixef, sigma2 = mod_spaMM_REML$phi)  ## in LM fixef are same for ML and REML but not in LMM

## ----sigma2 resid--------------------------------------------------------
sigma2_resid <- mod_spaMM_ML$phi ## stored for later (but it is not an estimate!)

## ----vcov----------------------------------------------------------------
XTX <- crossprod(model.matrix(mod_stats))
## XTX/sigma2_error is the Hessian (matrix of second derivative) of Lik(X)
## the inverse of the Hessian gives the covariance matrix
sigma2_error * solve(XTX)  
vcov(mod_stats)
vcov(mod_spaMM_REML)  ## don't use the ML fit here!

## ----compute density betas-----------------------------------------------
candidate_intercept_u <- seq(from = 30, to = 70,  by = 0.1)
candidate_slope_u     <- seq(from =  0, to = 2.5, by = 0.1)

candidates <- expand.grid(candidate_intercept = candidate_intercept_u, candidate_slope = candidate_slope_u)

candidates$d <- dmvnorm(x = candidates[, 1:2], mean = coef(mod_stats), sigma = vcov(mod_stats), log = FALSE)

## ----plot rgl, webgl=TRUE, fig.align="center"----------------------------
library(rgl)  ## to make interactive plots!
with(data = candidates, plot3d(candidate_intercept, candidate_slope, d))

## ----plot graphics, fig.align = "center", fig.height = 4, fig.width = 4----
contour(x = candidate_intercept_u, y = candidate_slope_u,
        z = matrix(candidates$d, nrow = length(candidate_intercept_u)),
        nlevels = 5, xlab = "intercept", ylab = "slope")

## ----gamma, fig.align = "center", fig.height = 4, fig.width = 4----------
curve(dgamma(x, shape = mod_stats$df.residual/2, scale = 2*sigma2_error/mod_stats$df.residual),
    from = 0, to = 80, xlab = "sigma2_error", ylab = "probability density", lwd = 3)

## ----simulate alien fn---------------------------------------------------
simulate_Aliens

## ----alien dataset-------------------------------------------------------
head(Alien)

## ----simulated dataset---------------------------------------------------
set.seed(123L)
head(simulate_Aliens())

## ----redraw samples------------------------------------------------------
new_beta_estimates <- t(replicate(1000, coef(update(mod_stats, data = simulate_Aliens()))))

## ----plot ecdf intercept, fig.align = "center", fig.height = 4, fig.width = 4----
curve(pnorm(x, mean = coef(mod_stats)[1],
    sd = sqrt(vcov(mod_stats)[[1]])),
    from = 30, to = 60, ylab = "cdf", lwd = 3)
plot(ecdf(new_beta_estimates[, 1]), col = "red",
     add = TRUE)

## ----plot ecdf slope, fig.align = "center", fig.height = 4, fig.width = 4----
curve(pnorm(x, mean = coef(mod_stats)[2],
    sd = sqrt(vcov(mod_stats)[[4]])),
    from = 0, to = 3, ylab = "cdf", lwd = 3)
plot(ecdf(new_beta_estimates[, 2]), col = "red",
     add = TRUE)

## ----plot ecdf sigma2, fig.align = "center", fig.height = 4, fig.width = 4----
new_sigma2_error <- t(replicate(1000, summary(update(mod_stats, data = simulate_Aliens()))$sigma^2))
curve(pgamma(x, shape = mod_stats$df.residual/2, scale = 2*summary(mod_stats)$sigma^2/mod_stats$df.residual),
    from = 0, to = 80, ylab = "cdf", lwd = 3)
plot(ecdf(new_sigma2_error), col = "red", add = TRUE)

## ----betas param boot----------------------------------------------------
newYs <- simulate(mod_stats, nsim = 5000)  ## new response variable using parametric bootstrap
res_sim <- t(coef(lm(as.matrix(newYs) ~ humans_eaten, data = Alien)))

## ----plot intercept param boot, fig.align = "center", fig.height = 4, fig.width = 4----
curve(pnorm(x, mean = coef(mod_stats)[1],
    sd = sqrt(vcov(mod_stats)[[1]])),
    from = 30, to = 60, ylab = "cdf", lwd = 3)
plot(ecdf(new_beta_estimates[, 1]), col = "red", add = TRUE)
plot(ecdf(res_sim[, 1]), col = "orange", add = TRUE)

## ----plot slope param boot, fig.align = "center", fig.height = 4, fig.width = 4----
curve(pnorm(x, mean = coef(mod_stats)[2],
    sd = sqrt(vcov(mod_stats)[[4]])),
    from = 0, to = 3, ylab = "cdf", lwd = 3)
plot(ecdf(new_beta_estimates[, 2]), col = "red", add = TRUE)
plot(ecdf(res_sim[, 2]), col = "orange", add = TRUE)

## ----plot 2D betas param boot, fig.align = "center", fig.height = 4, fig.width = 4, results = "hide"----
with(candidates, contour(candidate_intercept_u, candidate_slope_u,
  matrix(d, nrow = length(candidate_intercept_u)), col = "transparent", xlab = "intercept", ylab = "slope"))
points(res_sim[, "(Intercept)"], res_sim[, "humans_eaten"], col = "orange", pch = 1, cex = 0.5, lwd = 0.2)
with(candidates, contour(candidate_intercept_u, candidate_slope_u,
  matrix(d, nrow = length(candidate_intercept_u)), nlevels = 4, add = TRUE))

## ----quantile norm-------------------------------------------------------
c(Q0.025_N = qnorm(0.025), Q0.975_N = qnorm(0.975))

## ----quantile t----------------------------------------------------------
c(Qt_0.025 = qt(0.025, df = 10), Qt_0.975 = qt(0.975, df = 10))  ## with N-K = 10 (also try with 1000)

## ----CI Wald-------------------------------------------------------------
confint(mod_stats)
quantile_min <- qt(0.025, mod_stats$df.residual)
quantile_max <- qt(0.975, mod_stats$df.residual)
cbind(coef(mod_stats) + quantile_min * sqrt(diag(vcov(mod_stats))),
      coef(mod_stats) + quantile_max * sqrt(diag(vcov(mod_stats))))

## ----coverage CI Wald----------------------------------------------------
set.seed(1L)
mean(replicate(5000, sum(findInterval(confint(update(mod_stats, data = simulate_Aliens()))[1, ], 50)) == 1))
mean(replicate(5000, sum(findInterval(confint(update(mod_stats, data = simulate_Aliens()))[2, ], 1.5)) == 1))

## ----CI nonparam boot----------------------------------------------------
  res_sim <- do.call("rbind", sapply(1:rep, function(i) {
    newdata <- Alien[sample(1:nrow(Alien), replace = TRUE), ]
    t(coef(update(mod_stats, data = newdata)))
    }, simplify = FALSE))
  (confint_npboot <- rbind(quantile(res_sim[, 1], probs = c(0.025, 0.975)),
                           quantile(res_sim[, 2], probs = c(0.025, 0.975))))

## ----coverage CI nonparam boot-------------------------------------------
CInpboot <- function(rep = 100){
  data <- simulate_Aliens()
  mod <- update(mod_stats, data = data)
  res_sim <- do.call("rbind", sapply(1:rep, function(i) {
    newdata <- data[sample(1:nrow(data), replace = TRUE), ]
    t(coef(update(mod, data = newdata)))
  }, simplify = FALSE))
  rbind(quantile(res_sim[, 1], probs = c(0.025, 0.975)),
        quantile(res_sim[, 2], probs = c(0.025, 0.975)))
}
set.seed(1L)
mean(replicate(5000, sum(findInterval(CInpboot()[1, ], 50)) == 1))
mean(replicate(5000, sum(findInterval(CInpboot()[2, ], 1.5)) == 1))

## ----CI confint for comp-------------------------------------------------
confint(mod_stats)  ## asymptotic (for comparison)

## ----CI param boot-------------------------------------------------------
(confint_boot <- rbind(quantile(2*coef(mod_stats)[1] - res_sim[, 1], probs = c(0.025, 0.975)),
                       quantile(2*coef(mod_stats)[2] - res_sim[, 2], probs = c(0.025, 0.975))))

## ----coverage CI param boot----------------------------------------------
CIboot <- function(rep = 100){
  data <- simulate_Aliens()
  mod <- update(mod_stats, data = data)
  newYs <- simulate(mod, nsim = rep)
  res_sim <- t(coef(lm(as.matrix(newYs) ~ humans_eaten, data = data)))
  rbind(quantile(2*coef(mod)[1] - res_sim[, 1], probs = c(0.025, 0.975)),
        quantile(2*coef(mod)[2] - res_sim[, 2], probs = c(0.025, 0.975)))
}
set.seed(1L)
mean(replicate(5000, sum(findInterval(CIboot()[1, ], 50)) == 1))
mean(replicate(5000, sum(findInterval(CIboot()[2, ], 1.5)) == 1))

## ----param boot proper 1-------------------------------------------------
refit <- function(data, i = c(1, 2)) coef(lm(size ~ humans_eaten, data = data))[i]
newY  <- function(data, ...) { ## the second argument is necessary even if we don't use it
  data[, "size"] <- simulate(mod_stats, nsim = 1)
  return(data)
}
refit(Alien) ## the original fit
refit(data = newY(Alien)) ## fit on data simulated by parametric bootstrap

## ----param boot proper 2, message = FALSE--------------------------------
library(boot)
(sim.boot1 <- boot(data = Alien, statistic = refit, ran.gen = newY,
                  i = 1, R = 100, sim = "parametric")) ## change i to 2 for the slope!
sim.boot2 <- boot(data = Alien, statistic = refit, ran.gen = newY,
                  i = 2, R = 100, sim = "parametric")

## ----param boot proper 3-------------------------------------------------
boot.ci(sim.boot1, type = "basic")  ## alternative: "perc", "norm", "stud"

(confint_boot2 <- rbind(boot.ci(sim.boot1, type = "basic")$basic[, 4:5],
                        boot.ci(sim.boot2, type = "basic")$basic[, 4:5]))

## ----coverage CI param boot proper---------------------------------------
CIboot2a <- function(rep = 100, i){
  data <- simulate_Aliens()
  mod <- update(mod_stats, data = data)
  newY  <- function(data, ...) {
    data[, "size"] <- simulate(mod, nsim = 1)
    return(data)
  }
  sim.boot <- boot(data = data, statistic = refit, ran.gen = newY, i = i, R = rep, sim = "parametric")
  boot.ci(sim.boot, type = "basic")$basic[, 4:5]
}
set.seed(1L)
c(mean(replicate(500, sum(findInterval(CIboot2a(i = 1), 50)) == 1)),
  mean(replicate(500, sum(findInterval(CIboot2a(i = 2), 1.5)) == 1)))

## ----coverage CI param boot proper 2-------------------------------------
CIboot2b <- function(rep = 100, i){
  data <- simulate_Aliens()
  mod <- update(mod_stats, data = data)
  newYs <- simulate(mod, nsim = rep)
  res_sim <- t(coef(lm(as.matrix(newYs) ~ humans_eaten, data = data)))
  boot.ci(boot.out = list(R = nrow(res_sim)),
          t0 = coef(mod)[[i]],
          t  = res_sim[, i, drop = FALSE],
          type = "basic")$basic[, 4:5]
}
set.seed(1L)
c(mean(replicate(5000, sum(findInterval(CIboot2b(i = 1), 50)) == 1)),
  mean(replicate(5000, sum(findInterval(CIboot2b(i = 2), 1.5)) == 1)))

## ----coverage CI param boot proper 3-------------------------------------
CIboot2c <- function(rep = 100, i){
  data <- simulate_Aliens()
  mod <- update(mod_stats, data = data)
  newYs <- simulate(mod, nsim = rep)
  res_sim <- t(coef(lm(as.matrix(newYs) ~ humans_eaten, data = data)))
  boot.ci(boot.out = list(R = nrow(res_sim)),
          t0 = coef(mod)[[i]],
          t  = res_sim[, i, drop = FALSE],
          var.t0 = vcov(mod)[i, i],
          var.t = rep(var(res_sim[, i]), nrow(res_sim)),
          type = "stud")$student[, 4:5]
}
set.seed(1L)
c(mean(replicate(5000, sum(findInterval(CIboot2c(i = 1), 50)) == 1)),
  mean(replicate(5000, sum(findInterval(CIboot2c(i = 2), 1.5)) == 1)))

## ----logLik fixed--------------------------------------------------------
logLik(lm(size ~ 0 + offset(50 + 1.5 * humans_eaten), data = Alien))

logLik(fitme(size ~ 0 + offset(50 + 1.5 * humans_eaten), fixed = list(phi = 25), data = Alien))

## ----profile sigma2, fig.align = "center", fig.width = 4, fig.height = 4----
mod_spaMM_allfix <- fitme(size ~ humans_eaten, fixed = list(phi = 25), data = Alien)
candidate_sigma2 <- seq(10, 70, 0.5)
logLik_profile <- sapply(candidate_sigma2, function(sigma2) ## for each sigma2 we re-estimate other parameters
  logLik(update(mod_spaMM_allfix, fixed = list(phi = sigma2))))
plot(logLik_profile ~ candidate_sigma2, type = "l")
abline(v = 25, col = "green", lwd = 2)
abline(v = mod_spaMM_ML$phi, col = "blue", lwd = 2)

## ----profile for intercept, fig.align = "center", fig.width = 4, fig.height = 4, fig.show = "hold"----
logLik_profile <- sapply(candidate_intercept_u, function(intercept)
  logLik(lm(size ~ 0 + humans_eaten + offset(rep(intercept, nrow(Alien))), data = Alien)))
plot(logLik_profile ~ candidate_intercept_u, type = "l")
abline(v = 50, col = "green", lwd = 2)
abline(v = coef(mod_stats)[1], col = "blue", lwd = 2)
#abline(v = confint(mod_stats)[1, ], col = "purple", lty = 2, lwd = 2)
#abline(h = logLik(mod_stats) - 0.5*qchisq(0.95, 1), col = "red", lty = 2, lwd = 2)

## ----profile betas, fig.align = "center", echo = FALSE, results = "hide", fig.height = 5.5, fig.width = 5.5----
candidates$logLik <- NA

for (i in 1:nrow(candidates)) {
  mod_temp <- lm(size ~  0 + offset(candidates[i, 1] + candidates[i, 2] * humans_eaten), data = Alien)
  candidates[i, "logLik"] <- logLik(mod_temp)
}

par(las = 1)
with(candidates, contour(candidate_intercept_u, candidate_slope_u,
  matrix(candidates$logLik, nrow = length(candidate_intercept_u)), nlevels = 15,
  xlab = "slope", ylab = "intercept"))
points(mod_spaMM_ML$fixef["(Intercept)"], mod_spaMM_ML$fixef["humans_eaten"],
        col = "blue", pch = 4, lwd = 2, cex = 3)
points(50, 1.5, col = "green", pch = 4, lwd = 2, cex = 3)

#limit <- c(logLik(mod_stats) - 0.5*qchisq(0.95, df = 2))
#with(candidates, contour(candidate_intercept_u, candidate_slope_u,
#  matrix(candidates$logLik, nrow = length(candidate_intercept_u)),
#  levels = limit, add = TRUE, col = "red", lwd = 2))

## ----profile betas bis, eval = FALSE-------------------------------------
#  candidates$logLik <- NA
#  
#  for (i in 1:nrow(candidates)) {
#    mod_temp <- lm(size ~  0 + offset(candidates[i, 1] + candidates[i, 2] * humans_eaten), data = Alien)
#    candidates[i, "logLik"] <- logLik(mod_temp)
#  }
#  
#  par(las = 1)
#  with(candidates, contour(candidate_intercept_u, candidate_slope_u,
#                           matrix(candidates$logLik, nrow = length(candidate_intercept_u)),
#                           nlevels = 15, xlab = "slope", ylab = "intercept"))
#  points(mod_spaMM_ML$fixef["(Intercept)"], mod_spaMM_ML$fixef["humans_eaten"],
#         col = "blue", pch = 4, lwd = 2, cex = 3)
#  points(50, 1.5, col = "green", pch = 4, lwd = 2, cex = 3)
#  
#  #limit <- c(logLik(mod_stats) - 0.5*qchisq(0.95, df = 2))
#  #with(candidates, contour(candidate_intercept_u, candidate_slope_u,
#  #  matrix(candidates$logLik, nrow = length(candidate_intercept_u)),
#  #  levels = limit, add = TRUE, col = "red", lwd = 2))

## ----CI by profile intercept---------------------------------------------
where.is.limit.inter <- function(intercept, model_ref) {
  logLik_mod <- logLik(lm(size ~ 0 + humans_eaten + offset(rep(intercept, nrow(model_ref$model))),
                            data = model_ref$model))[[1]]
  logLik_goal <- logLik(model_ref)[[1]] - 0.5*qchisq(0.95, df = 1)
  return(abs(logLik_mod - logLik_goal))}
CI_intercept <- function(model_ref) {
  lwr <- optimise(where.is.limit.inter, interval = c(coef(model_ref)[1] - 10, coef(model_ref)[1]),
                  model_ref = model_ref)
  upr <- optimise(where.is.limit.inter, interval = c(coef(model_ref)[1], coef(model_ref)[1] + 10),
                  model_ref = model_ref)
  return(c(lwr = lwr$minimum, upr = upr$minimum))}
rbind("using Student" = confint(mod_stats)[1, ], "using np. boot." = confint_npboot[1, ],
      "using param. boot. 1" = confint_boot[1, ], "using param. boot. 2" = confint_boot2[1, ],
      "using profile" = CI_intercept(mod_stats))

## ----CI by profile slope-------------------------------------------------
where.is.limit.slope <- function(slope, model_ref) {
  logLik_mod <- logLik(lm(size ~ 1 + offset(slope * humans_eaten),
                            data = model_ref$model))[[1]]
  logLik_goal <- logLik(model_ref)[[1]] - 0.5*qchisq(0.95, df = 1)
  return(abs(logLik_mod - logLik_goal))
}

CI_slope <- function(model_ref) {
  lwr <- optimise(where.is.limit.slope, interval = c(coef(model_ref)[2] - 1, coef(model_ref)[2]),
                  model_ref = model_ref)
  upr <- optimise(where.is.limit.slope, interval = c(coef(model_ref)[2], coef(model_ref)[2] + 1),
                  model_ref = model_ref)
  return(c(lwr = lwr$minimum, upr = upr$minimum))
}

rbind("using Student" = confint(mod_stats)[2, ], "using np. boot." = confint_npboot[2, ],
      "using param. boot. 1" = confint_boot[2, ], "using param. boot. 2" = confint_boot2[2, ],
      "using profile" = CI_slope(mod_stats))

## ----coverage CI profile-------------------------------------------------
set.seed(1L); n.replicates <- 1000
mean(replicate(n.replicates, sum(findInterval(CI_intercept(lm(size ~ humans_eaten,
    data = simulate_Aliens())), 50)) == 1))
mean(replicate(n.replicates, sum(findInterval(CI_slope(lm(size ~ humans_eaten,
    data = simulate_Aliens())), 1.5)) == 1))

mean(replicate(n.replicates, sum(findInterval(CI_intercept(lm(size ~ humans_eaten,
    data = simulate_Aliens(N = 500))), 50)) == 1))
mean(replicate(n.replicates, sum(findInterval(CI_slope(lm(size ~ humans_eaten,
    data = simulate_Aliens(N = 500))), 1.5)) == 1))

## ----CI predVar, fig.align="center", fig.width=4, fig.height=4-----------
pred <- data.frame(humans_eaten = seq(1, 12, 0.1))
pred <- cbind(pred, predict(mod_stats, newdata = pred, interval = "confidence"))
plot(size ~ humans_eaten, data = Alien)
points(fit ~ humans_eaten, data = pred, lty = 1, lwd = 2, col = "blue", type = "l")
points(upr ~ humans_eaten, data = pred, lty = 2, lwd = 2, col = "blue", type = "l")
points(lwr ~ humans_eaten, data = pred, lty = 2, lwd = 2, col = "blue", type = "l")

## ----compute predVar-----------------------------------------------------
predict(mod_stats, newdata = data.frame(humans_eaten = 4:5), interval = "confidence")
pred <- predict(mod_stats, newdata = data.frame(humans_eaten = 4:5), interval = "confidence", se.fit = TRUE)
rbind(c(pred$fit[1, "fit"] + quantile_min * pred$se.fit[1], 
        pred$fit[1, "fit"] + quantile_max * pred$se.fit[1]),
      c(pred$fit[2, "fit"] + quantile_min * pred$se.fit[2], 
        pred$fit[2, "fit"] + quantile_max * pred$se.fit[2]))

## ----compute predVar2----------------------------------------------------
pred$se.fit^2 ## prediction variance
(X <- cbind(c(1, 1), 4:5))  ## design matrix
(vcov.means <- X %*% vcov(mod_stats) %*% t(X))  ## covariances for predicted values
diag(vcov.means)

## ----respVar, fig.align="center", fig.width=4, fig.height=4--------------
pred <- data.frame(humans_eaten = seq(1, 12, 0.1))
pred <- cbind(pred, predict(mod_stats, newdata = pred, interval = "prediction"))
plot(size ~ humans_eaten, data = Alien)
points(fit ~ humans_eaten, data = pred, lty = 1, lwd = 2, col = "red", type = "l")
points(upr ~ humans_eaten, data = pred, lty = 2, lwd = 2, col = "red", type = "l")
points(lwr ~ humans_eaten, data = pred, lty = 2, lwd = 2, col = "red", type = "l")

## ----compute respVar-----------------------------------------------------
pred <- predict(mod_stats, newdata = data.frame(humans_eaten = 4), interval = "prediction", se.fit = TRUE)
pred$fit
se.pred <- sqrt(pred$se.fit^2 + pred$residual.scale^2)
pred$fit[, "fit"] + quantile_min * se.pred
pred$fit[, "fit"] + quantile_max * se.pred
pred$residual.scale^2 == summary(mod_stats)$sigma^2  ## estimate of the error variance

