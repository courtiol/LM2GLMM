## ----setup, include=FALSE---------------------------------------------------------------------------------------------
library(LM2GLMM)
library(spaMM)
library(car)
library(DHARMa)
library(pscl)
spaMM.options(nb_cores = 4L)
options(width = 120)
knitr::opts_chunk$set(cache = TRUE, cache.path = "./cache_knitr/GLM_resid/", fig.path = "./fig_knitr/GLM_resid/", fig.width = 5, fig.height = 5, fig.align = "center", error = TRUE)

## ---------------------------------------------------------------------------------------------------------------------
set.seed(1L)
Aliens <- simulate_Aliens_GLM()
mod_gauss <- glm(size  ~ humans_eaten, family = gaussian(), data = Aliens)
mod_poiss <- glm(eggs  ~ humans_eaten, family = poisson(),  data = Aliens)
mod_binar <- glm(happy ~ humans_eaten, family = binomial(), data = Aliens)
mod_binom <- glm(cbind(blue_eyes, pink_eyes) ~ humans_eaten, family = binomial(), data = Aliens)

## ---------------------------------------------------------------------------------------------------------------------
par(mfrow = c(2, 2))
plot(mod_poiss)

## ---------------------------------------------------------------------------------------------------------------------
par(mfrow = c(2, 2))
plot(mod_binar)

## ---------------------------------------------------------------------------------------------------------------------
par(mfrow = c(2, 2))
plot(mod_binom)

## ---------------------------------------------------------------------------------------------------------------------
rbind(
  residuals(mod_poiss, type = "deviance")[1:2],
  residuals(mod_poiss, type = "pearson")[1:2],
  residuals(mod_poiss, type = "working")[1:2],
  residuals(mod_poiss, type = "response")[1:2])

## ---------------------------------------------------------------------------------------------------------------------
rbind(
  residuals(mod_binom, type = "deviance")[1:2],
  residuals(mod_binom, type = "pearson")[1:2],
  residuals(mod_binom, type = "working")[1:2],
  residuals(mod_binom, type = "response")[1:2])

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
(X <- sum(residuals(mod_binom, type = "pearson")^2))

## ---------------------------------------------------------------------------------------------------------------------
1 - pchisq(X, mod_poiss$df.residual)  ## Pearson goodness of fit test
X / mod_poiss$df.residual  ## measure of overdispersion

## ---------------------------------------------------------------------------------------------------------------------
residuals(mod_poiss, type = "deviance")[1:2]
rbind((residuals(mod_poiss, type = "deviance")[1:2])^2,
      with(mod_poiss, poisson()$dev.resids(y = y, mu = fitted.values, wt = prior.weights))[1:2])

## ---------------------------------------------------------------------------------------------------------------------
c(sum(residuals(mod_gauss, type = "deviance")^2), deviance(mod_gauss))
c(sum(residuals(mod_poiss, type = "deviance")^2), deviance(mod_poiss))
c(sum(residuals(mod_binom, type = "deviance")^2), deviance(mod_binom))

## ----gof 1, fig.width = 3, fig.height = 3-----------------------------------------------------------------------------
p <- replicate(1000, {
  d <- simulate_Aliens_GLM()
  m <- update(mod_poiss, data = d)
  X <- sum(residuals(m, type = "pearson")^2)
  1 - pchisq(X, m$df.residual)
})
plot(ecdf(p))
abline(0, 1, lty = 2, col = "green")

## ----gof 2, fig.width = 3, fig.height = 3-----------------------------------------------------------------------------
p <- replicate(1000, {
  d <- simulate_Aliens_GLM()
  m <- update(mod_poiss, data = d)
  dev <- deviance(m)
  1 - pchisq(dev, m$df.residual)
})
plot(ecdf(p))
abline(0, 1, lty = 2, col = "green")

## ----gof 3, fig.width = 3, fig.height = 3-----------------------------------------------------------------------------
p <- replicate(1000, {
  d <- simulate_Aliens_GLM()
  m <- update(mod_binar, data = d)
  X <- sum(residuals(m, type = "pearson")^2)
  1 - pchisq(X, m$df.residual)
})
plot(ecdf(p))
abline(0, 1, lty = 2, col = "green")

## ----gof 4, fig.width = 3, fig.height = 3-----------------------------------------------------------------------------
p <- replicate(1000, {
  d <- simulate_Aliens_GLM()
  m <- update(mod_binar, data = d)
  dev <- deviance(m)
  1 - pchisq(dev, m$df.residual)
})
plot(ecdf(p))
abline(0, 1, lty = 2, col = "green")

## ----gof 5, fig.width = 3, fig.height = 3-----------------------------------------------------------------------------
p <- replicate(1000, {
  d <- simulate_Aliens_GLM()
  m <- update(mod_binom, data = d)
  X <- sum(residuals(m, type = "pearson")^2)
  1 - pchisq(X, m$df.residual)
})
plot(ecdf(p))
abline(0, 1, lty = 2, col = "green")

## ----gof 6, fig.width = 3, fig.height = 3-----------------------------------------------------------------------------
p <- replicate(1000, {
  d <- simulate_Aliens_GLM()
  m <- update(mod_binom, data = d)
  dev <- deviance(m)
  1 - pchisq(dev, m$df.residual)
})
plot(ecdf(p))
abline(0, 1, lty = 2, col = "green")

## ---------------------------------------------------------------------------------------------------------------------
rbind(mod_poiss$residuals[1:2],
      ((mod_poiss$y - mod_poiss$fitted.values)/poisson()$mu.eta(mod_poiss$linear.predictors))[1:2])

rbind(mod_binom$residuals[1:2],
      ((mod_binom$y - mod_binom$fitted.values)/binomial()$mu.eta(mod_binom$linear.predictors))[1:2])

## ---------------------------------------------------------------------------------------------------------------------
mod_UK_small <- glm(milk ~ drink + sex + cigarettes, data = UK[1:10, ], family = poisson())
residuals(mod_UK_small, type = "partial")

## ---------------------------------------------------------------------------------------------------------------------
(p <- predict(mod_UK_small, type = "terms")) ## prediction expressed per predictor
c(sum(p[1, ]) + attr(p, "constant"), predict(mod_UK_small, type = "link")[1])

## ---------------------------------------------------------------------------------------------------------------------
rbind((p + residuals(mod_UK_small, type = "working"))[1, ],
      residuals(mod_UK_small, type = "partial")[1, ])

## ---- fig.width = 10, fig.height = 4----------------------------------------------------------------------------------
library(car)
par(mfrow = c(1, 3))
crPlots(mod_UK_small, terms = ~ drink)
crPlots(mod_UK_small, terms = ~ sex)
crPlots(mod_UK_small, terms = ~ cigarettes)

## ---- fig.width = 10, fig.height = 4----------------------------------------------------------------------------------
mod_UK <- glm(milk ~ drink + sex + cigarettes, data = UK, family = poisson())
par(mfrow = c(1, 3))
crPlots(mod_UK, terms = ~ drink)
crPlots(mod_UK, terms = ~ sex)
crPlots(mod_UK, terms = ~ cigarettes)

## ---- fig.height = 4, fig.width = 4-----------------------------------------------------------------------------------
set.seed(1L)
s <- simulate(mod_poiss, 1000)
r <- sapply(s, function(i) i + runif(nrow(mod_poiss$model), min = -0.5, max = 0.5))
hist(r[1, ], main = "distrib of first fitted value", nclass = 30)
abline(v = mod_poiss$y[1] + runif(1, min = -0.5, max = 0.5), col = "red", lwd = 2, lty = 2)

## ---- fig.height = 4, fig.width = 4-----------------------------------------------------------------------------------
plot(ecdf1 <- ecdf(r[1, ]), main = "cdf of first fitted value")
noise <- runif(1, min = -0.5, max = 0.5)
simulated_residual_1 <-  ecdf1(mod_poiss$y[1] + noise)
segments(x0 = mod_poiss$y[1] + noise, y0 = 0, y1 =  simulated_residual_1, col = "red", lwd = 2)
arrows(x0 = mod_poiss$y[1] + noise, x1 = -1, y0 = simulated_residual_1, col = "red", lwd = 2)

## ---- fig.height = 4, fig.width = 4-----------------------------------------------------------------------------------
plot(ecdf2 <- ecdf(r[2, ]), main = "cdf of second fitted value")
noise <- runif(1, min = -0.5, max = 0.5)
simulated_residual_2 <-  ecdf2(mod_poiss$y[2] + noise)
segments(x0 = mod_poiss$y[2] + noise, y0 = 0, y1 =  simulated_residual_2, col = "red", lwd = 2)
arrows(x0 = mod_poiss$y[2] + noise, x1 = -1, y0 = simulated_residual_2, col = "red", lwd = 2)

## ----simu resid, fig.height = 4, fig.width = 4------------------------------------------------------------------------
simulated_residuals <- rep(NA, nrow(mod_poiss$model))
for (i in 1:nrow(mod_poiss$model)) {
  ecdf_fn <- ecdf(r[i, ])
  simulated_residuals[i] <- ecdf_fn(mod_poiss$y[i] + runif(1, min = -0.5, max = 0.5))
}

## ---- fig.height = 4, fig.width = 4-----------------------------------------------------------------------------------
plot(simulated_residuals)

## ---- fig.height = 4, fig.width = 4-----------------------------------------------------------------------------------
plot(ecdf(simulated_residuals))

## ----simres poiss, fig.width = 7, fig.height = 4----------------------------------------------------------------------
library(DHARMa)
mod_poiss_simres <- simulateResiduals(mod_poiss)
plot(mod_poiss_simres)

## ----simres binar, fig.width = 7, fig.height = 4----------------------------------------------------------------------
mod_binar_simres <- simulateResiduals(mod_binar)
plot(mod_binar_simres)

## ----simres binom, fig.width = 7, fig.height = 4----------------------------------------------------------------------
mod_binom_simres <- simulateResiduals(mod_binom)
plot(mod_binom_simres)

## ----simres UK, fig.width = 7, fig.height = 4-------------------------------------------------------------------------
mod_UK_simres <- simulateResiduals(mod_UK)
plot(mod_UK_simres)

## ---------------------------------------------------------------------------------------------------------------------
set.seed(1L)
Aliens2 <- data.frame(humans_eaten = runif(100, min = 0, max = 15))
Aliens2$eggs <- rpois( n = 100, lambda = exp(1 + 0.2 * Aliens2$humans_eaten - 0.02 *  Aliens2$humans_eaten^2))
mod_poiss2bad <- glm(eggs ~ humans_eaten, data = Aliens2, family = poisson()) ## mispecified model
(mod_poiss2good <- glm(eggs ~ poly(humans_eaten, 2, raw = TRUE), data = Aliens2, family = poisson())) ## good model

## ----  fig.width = 4, fig.height = 4----------------------------------------------------------------------------------
plot(I(1 + 0.2 * Aliens2$humans_eaten - 0.02 *  Aliens2$humans_eaten^2) ~ Aliens2$humans_eaten,
     ylab = "eta", ylim = c(-1, 2))
data.for.pred <- data.frame(humans_eaten = 0:15)
points(predict(mod_poiss2good, newdata = data.for.pred) ~ I(0:15), col = "blue")
points(predict(mod_poiss2bad, newdata = data.for.pred) ~ I(0:15), col = "red")

## ----  fig.width = 4, fig.height = 4----------------------------------------------------------------------------------
plot(exp(1 + 0.2 * Aliens2$humans_eaten - 0.02 *  Aliens2$humans_eaten^2) ~ Aliens2$humans_eaten,
     ylab = "predicted number of eggs", ylim = c(0, 6))
points(predict(mod_poiss2good, newdata = data.for.pred, type = "response") ~ I(0:15), col = "blue")
points(predict(mod_poiss2bad, newdata = data.for.pred, type = "response") ~ I(0:15), col = "red")

## ---------------------------------------------------------------------------------------------------------------------
crPlots(mod_poiss2bad, terms = ~ humans_eaten)

## ---------------------------------------------------------------------------------------------------------------------
crPlots(mod_poiss, terms = ~ humans_eaten)  ## cannot do that for mod_poiss2good

## ---- fig.width = 7, fig.height = 4-----------------------------------------------------------------------------------
plot(s_bad <- simulateResiduals(mod_poiss2bad))

## ---- fig.width = 7, fig.height = 4-----------------------------------------------------------------------------------
plot(s_good <- simulateResiduals(mod_poiss2good))

## ---------------------------------------------------------------------------------------------------------------------
testUniformity(s_bad)
testUniformity(s_good)

## ---- fig.width = 4, fig.height = 4, message = FALSE------------------------------------------------------------------
library(mgcv)
mod_poiss2GAM <- gam(eggs ~ s(humans_eaten), data = Aliens2, family = poisson())
plot(mod_poiss2GAM)

## ---------------------------------------------------------------------------------------------------------------------
testTemporalAutocorrelation(s_good, time = mod_poiss2good$fitted.values, plot = FALSE)

## ---------------------------------------------------------------------------------------------------------------------
testTemporalAutocorrelation(s_good, time = Aliens2$humans_eaten, plot = FALSE)

## ---------------------------------------------------------------------------------------------------------------------
data.frame(age = c(1:5, 1:5),
      id = c(rep("A", 5), rep("B", 5)),
      death = c(0, 0, 0, 0, 1, 0, 0, 1, NA, NA),
      annual_rain = c(100, 120, 310, 50, 200, 45, 100, 320, 100, 120))

## ---- fig.width = 10, fig.height = 3----------------------------------------------------------------------------------
p <- seq(0, 1, 0.1)
lambda <- 0:10
theta <- 0:10
v_b <- binomial()$variance(p)
v_p <- poisson()$variance(lambda)
v_G <- Gamma()$variance(theta)
par(mfrow = c(1, 3), las = 2)
plot(v_b ~ p); plot(v_p ~ lambda); plot(v_G ~ theta)

## ---------------------------------------------------------------------------------------------------------------------
set.seed(1L)
popA <- data.frame(humans_eaten = runif(50, min = 0, max = 15))
popA$eggs <- rpois(n = 50, lambda = exp(-1 + 0.05 * popA$humans_eaten))
popA$pop <- "A"
popB <- data.frame(humans_eaten = runif(50, min = 0, max = 15))
popB$eggs <- rpois(n = 50, lambda = exp(-3 + 0.4 * popB$humans_eaten))
popB$pop <- "B"
AliensMix <- rbind(popA, popB)
(mod_poissMix <- glm(eggs ~ humans_eaten, family = poisson(), data = AliensMix))

## ---------------------------------------------------------------------------------------------------------------------
cbind(disp = mod_poissMix$deviance / mod_poissMix$df.residual,
      pv = 1 - pchisq(mod_poissMix$deviance, mod_poissMix$df.residual))

## ---------------------------------------------------------------------------------------------------------------------
cbind(disp = sum(residuals(mod_poissMix, type = "pearson")^2) / mod_poissMix$df.residual,
      pv = 1 - pchisq(sum(residuals(mod_poissMix, type = "pearson")^2), mod_poissMix$df.residual))

## ---------------------------------------------------------------------------------------------------------------------
r <- simulateResiduals(mod_poissMix, refit = TRUE)
testOverdispersion(r)

## ---------------------------------------------------------------------------------------------------------------------
mod_poissMix2 <- glm(eggs ~ pop*humans_eaten, family = poisson(), data = AliensMix)
r2 <- simulateResiduals(mod_poissMix2, refit = TRUE)
testOverdispersion(r2)  ## you can change options to test for underdispersion

## ---------------------------------------------------------------------------------------------------------------------
mod_poissMixQ <- glm(eggs ~ humans_eaten, family = quasipoisson(), data = AliensMix)
summary(mod_poissMixQ)$coef
Anova(mod_poissMixQ, test = "F")

## ---------------------------------------------------------------------------------------------------------------------
logLik(mod_poissMixQ)

## ---------------------------------------------------------------------------------------------------------------------
mod_poissMix <- glm(eggs ~ humans_eaten, family = poisson(), data = AliensMix)
sum(residuals(mod_poissMix, type = "pearson")^2) / mod_poissMix$df.residual
summary(mod_poissMix)$coefficients

## ---------------------------------------------------------------------------------------------------------------------
library(MASS)
mod_poissMixNB <- glm.nb(eggs ~ humans_eaten, data = AliensMix)
sum(residuals(mod_poissMixNB, type = "pearson")^2) / mod_poissMix$df.residual
summary(mod_poissMixNB)$coefficients
c(AIC(mod_poissMix), AIC(mod_poissMixNB), logLik(mod_poissMixNB))

## ---------------------------------------------------------------------------------------------------------------------
negbin <- spaMM::negbin ## otherwise spaMM tries to use negbin from mgcv, which won't work
mod_poissMixSpaMM   <- fitme(eggs ~ humans_eaten, family = poisson(), data = AliensMix)
mod_poissMixNBSpaMM <- fitme(eggs ~ humans_eaten, family = negbin(), data = AliensMix)
summary(mod_poissMixNBSpaMM)
c(AIC(mod_poissMixSpaMM), AIC(mod_poissMixNBSpaMM), logLik(mod_poissMixNBSpaMM))

## ----COMPoisson, warning = FALSE--------------------------------------------------------------------------------------
mod_poissMixCP <- glm(eggs ~ humans_eaten, family = COMPoisson(nu = 1), data = AliensMix)
mod_poissMixCP$coef
c(AIC(mod_poissMix), AIC(mod_poissMixNB), AIC(mod_poissMixCP))

## ----COMPoisson2, warning = FALSE-------------------------------------------------------------------------------------
mod_poissMixCPSpaMM <- fitme(eggs ~ humans_eaten, family = COMPoisson(), data = AliensMix)
summary(mod_poissMixCPSpaMM)
c(AIC(mod_poissMixSpaMM), AIC(mod_poissMixNBSpaMM), AIC(mod_poissMixCP), AIC(mod_poissMixCPSpaMM))

## ---- echo = FALSE, warning = FALSE, fig.height = 6, fig.width = 6----------------------------------------------------
d <- data.frame(humans_eaten = seq(0, 15, length = 1000))
p <- predict(mod_poissMixSpaMM, newdata = d, intervals = "predVar")
plot(p ~ d$humans_eaten, ylim = range(mod_poissMixSpaMM$data$eggs), type = "l", lwd = 3)
points(attr(p, "interval")[, 1] ~ d$humans_eaten, lty = 2, type = "l")
points(attr(p, "interval")[, 2] ~ d$humans_eaten, lty = 2, type = "l")
p <- predict(mod_poissMixNBSpaMM, newdata = d, intervals = "predVar")
points(p ~ d$humans_eaten, type = "l", col = "red", lwd = 3)
points(attr(p, "interval")[, 1] ~ d$humans_eaten, lty = 2, type = "l", col = "red")
points(attr(p, "interval")[, 2] ~ d$humans_eaten, lty = 2, type = "l", col = "red")
p <- predict(mod_poissMixCPSpaMM, newdata = d, intervals = "predVar")
points(p ~ d$humans_eaten, type = "l", col = "blue", lwd = 3)
points(attr(p, "interval")[, 1] ~ d$humans_eaten, lty = 2, type = "l", col = "blue")
points(attr(p, "interval")[, 2] ~ d$humans_eaten, lty = 2, type = "l", col = "blue")
legend("topleft", fill = c("black", "red", "blue"), legend = c("Poisson", "Negative Binomial", "Conway-Maxwell-Poisson "), bty = "n")

## ---- eval = FALSE----------------------------------------------------------------------------------------------------
#  d <- data.frame(humans_eaten = seq(0, 15, length = 1000))
#  p <- predict(mod_poissMixSpaMM, newdata = d, intervals = "predVar")
#  plot(p ~ d$humans_eaten, ylim = range(mod_poissMixSpaMM$data$eggs), type = "l", lwd = 3)
#  points(attr(p, "interval")[, 1] ~ d$humans_eaten, lty = 2, type = "l")
#  points(attr(p, "interval")[, 2] ~ d$humans_eaten, lty = 2, type = "l")
#  p <- predict(mod_poissMixNBSpaMM, newdata = d, intervals = "predVar")
#  points(p ~ d$humans_eaten, type = "l", col = "red", lwd = 3)
#  points(attr(p, "interval")[, 1] ~ d$humans_eaten, lty = 2, type = "l", col = "red")
#  points(attr(p, "interval")[, 2] ~ d$humans_eaten, lty = 2, type = "l", col = "red")
#  p <- predict(mod_poissMixCPSpaMM, newdata = d, intervals = "predVar")
#  points(p ~ d$humans_eaten, type = "l", col = "blue", lwd = 3)
#  points(attr(p, "interval")[, 1] ~ d$humans_eaten, lty = 2, type = "l", col = "blue")
#  points(attr(p, "interval")[, 2] ~ d$humans_eaten, lty = 2, type = "l", col = "blue")
#  legend("topleft", fill = c("black", "red", "blue"),
#         legend = c("Poisson", "Negative Binomial", "Conway-Maxwell-Poisson "), bty = "n")

## ---------------------------------------------------------------------------------------------------------------------
set.seed(1L)
AliensZ <- simulate_Aliens_GLM(N = 1000)
AliensZ$eggs <- AliensZ$eggs * AliensZ$happy ## unhappy Aliens loose their eggs :-(
barplot(table(AliensZ$eggs))

## ---------------------------------------------------------------------------------------------------------------------
mod_Zpoiss <- glm(eggs ~ humans_eaten, data = AliensZ, family = poisson())
r <- simulateResiduals(mod_Zpoiss)
testZeroInflation(r, plot = FALSE)
mean(sum(AliensZ$eggs == 0) / sum(dpois(0, fitted(mod_Zpoiss))))

## ---- cache = FALSE---------------------------------------------------------------------------------------------------
testZeroInflation(r, plot = TRUE)

## ---------------------------------------------------------------------------------------------------------------------
mod_Znb <- glm.nb(eggs ~ humans_eaten, data = AliensZ)
r <- simulateResiduals(mod_Znb)
testZeroInflation(r, plot = FALSE)

## ---------------------------------------------------------------------------------------------------------------------
testZeroInflation(r, plot = TRUE)

## ---- message = FALSE-------------------------------------------------------------------------------------------------
library(pscl)
mod_Zhurd1 <- hurdle(eggs ~ humans_eaten | humans_eaten, dist = "poisson", zero.dist = "binomial",
                     data = AliensZ)
mod_Zhurd2 <- hurdle(eggs ~ humans_eaten | 1, dist = "poisson", zero.dist = "binomial", data = AliensZ)
lmtest::lrtest(mod_Zhurd1, mod_Zhurd2)
mean(sum(AliensZ$eggs == 0) / sum(predict(mod_Zhurd1, type = "prob")[, 1]))

## ---- message = FALSE-------------------------------------------------------------------------------------------------
mod_Zzi1 <- zeroinfl(eggs ~ humans_eaten | humans_eaten, dist = "poisson", data = AliensZ)
mod_Zzi2 <- zeroinfl(eggs ~ humans_eaten | 1, dist = "poisson", data = AliensZ)
lmtest::lrtest(mod_Zzi1, mod_Zzi2)
mean(sum(AliensZ$eggs == 0) / sum(predict(mod_Zzi1, type = "prob")[, 1]))

## ---------------------------------------------------------------------------------------------------------------------
cbind(Poisson = AIC(mod_Zpoiss),
      NegBin  = AIC(mod_Znb),
      Hurdle  = AIC(mod_Zhurd1),
      ZeroInf = AIC(mod_Zzi1))
tab <- rbind(Poisson = c(mod_Zpoiss$coefficients, NA, NA),
             NegBin = c(mod_Znb$coefficients, NA, NA),
             Hurdle = unlist(mod_Zhurd1$coefficients),  ## NOTE: the hurdle part predicts positive counts, not zeros!!
             ZeroInfl = unlist(mod_Zzi1$coefficients),  ## NOTE: the binary part predicts zeros, not counts!!
             Truth = c(attr(AliensZ, "param.eta")$eggs, attr(AliensZ, "param.eta")$happy))
colnames(tab) <- c("Int.count", "Slope.count", "Int.bin", "Slope.bin")
tab

## ---------------------------------------------------------------------------------------------------------------------
set.seed(1L)
n <- 50
test <- data.frame(happy = rbinom(2*n, prob = c(rep(0, n), rep(0.75, n)), size = 1), 
                   sp = factor(c(rep("sp1", n), rep("sp2", n))))
table(test$happy, test$sp)

## ---------------------------------------------------------------------------------------------------------------------
mod <- glm(happy ~ sp, data = test, family = binomial())

## ---------------------------------------------------------------------------------------------------------------------
test$happy[1] <- 1
table(test$happy, test$sp)
mod2 <- glm(happy ~ sp, data = test, family = binomial())

## ---------------------------------------------------------------------------------------------------------------------
summary(mod2)

## ---- message = FALSE-------------------------------------------------------------------------------------------------
test$eggs[1] <- 0  ## restore the original data
library(safeBinaryRegression)  ## overload the glm function
mod3 <- glm(happy ~ sp, data = test, family = binomial())
summary(mod3)$coef
AIC(mod3)

## ---- message = FALSE-------------------------------------------------------------------------------------------------
library(spaMM)  ## with package e1071 installed!
mod4 <- fitme(happy ~ sp, data = test, family = binomial())
summary(mod4)
AIC(mod4)

## ---- message = FALSE-------------------------------------------------------------------------------------------------
library(brglm)  ## there is a new version in development brglm2 but it still has issues
mod5 <- brglm(happy ~ sp, data = test, family = binomial())
summary(mod5)
AIC(mod5)

