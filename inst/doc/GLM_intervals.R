## ----setup, include=FALSE---------------------------------------------------------------------------------------------
library(LM2GLMM)
library(car)
library(MASS)
library(spaMM)
options(width = 120)
knitr::opts_chunk$set(cache = TRUE, cache.path = "./cache_knitr/GLM_uncertainty/", fig.path = "./fig_knitr/GLM_uncertainty/", fig.width = 4, fig.height = 4, fig.align = "center", error = FALSE)

## ---------------------------------------------------------------------------------------------------------------------
set.seed(1L)
Aliens <- simulate_Aliens_GLM()
head(Aliens)
attributes(Aliens)$param.eta$size
attributes(Aliens)$param.eta$blue_eyes

## ---------------------------------------------------------------------------------------------------------------------
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
new_betas <- t(replicate(1000, update(mod_gauss, data = simulate_Aliens_GLM())$coef))

## ---------------------------------------------------------------------------------------------------------------------
qqnorm(new_betas[, 1])
qqline(new_betas[, 1], col = "red", lwd = 2)

## ---------------------------------------------------------------------------------------------------------------------
qqnorm(new_betas[, 2])
qqline(new_betas[, 2], col = "red", lwd = 2)

## ---------------------------------------------------------------------------------------------------------------------
new_betas <- t(replicate(1000, update(mod_poiss, data = simulate_Aliens_GLM())$coef))

## ---------------------------------------------------------------------------------------------------------------------
qqnorm(new_betas[, 1])
qqline(new_betas[, 1], col = "red", lwd = 2)

## ---------------------------------------------------------------------------------------------------------------------
qqnorm(new_betas[, 2])
qqline(new_betas[, 2], col = "red", lwd = 2)

## ---------------------------------------------------------------------------------------------------------------------
new_betas <- t(replicate(1000, update(mod_binar, data = simulate_Aliens_GLM())$coef))

## ---------------------------------------------------------------------------------------------------------------------
qqnorm(new_betas[, 1])
qqline(new_betas[, 1], col = "red", lwd = 2)

## ---------------------------------------------------------------------------------------------------------------------
qqnorm(new_betas[, 2])
qqline(new_betas[, 2], col = "red", lwd = 2)

## ---------------------------------------------------------------------------------------------------------------------
new_betas <- t(replicate(1000, update(mod_binom, data = simulate_Aliens_GLM())$coef))

## ---------------------------------------------------------------------------------------------------------------------
qqnorm(new_betas[, 1])
qqline(new_betas[, 1], col = "red", lwd = 2)

## ---------------------------------------------------------------------------------------------------------------------
qqnorm(new_betas[, 2])
qqline(new_betas[, 2], col = "red", lwd = 2)

## ---------------------------------------------------------------------------------------------------------------------
confint.approx <- function(mod, print = FALSE) {
  
  intervals <- cbind(lwr = (mod$coefficients + qnorm(0.025) *  sqrt(diag(vcov(mod)))),
                     upr = (mod$coefficients + qnorm(0.975) *  sqrt(diag(vcov(mod)))))
  
  if (print) print(intervals)
  name.response <- mod$terms[[2]]
  if (length(name.response) == 3) name.response <- name.response[[2]]
  true.intercept <- attributes(mod$data)$param.eta[[paste(name.response)]][1]
  true.slope <- attributes(mod$data)$param.eta[[paste(name.response)]][2]
  if (print) cat("\n true parameters in 95% CI? \n")
  
  c(true.intercept > intervals["(Intercept)", "lwr"] & true.intercept < intervals["(Intercept)", "upr"],
    true.slope > intervals["humans_eaten", "lwr"] & true.slope < intervals["humans_eaten", "upr"])
}

## ---------------------------------------------------------------------------------------------------------------------
confint.approx(mod_poiss, print = TRUE)

## ---------------------------------------------------------------------------------------------------------------------
set.seed(1L)
replicate(5, confint.approx(update(mod_poiss, data = simulate_Aliens_GLM())))

## ----coverage1--------------------------------------------------------------------------------------------------------
set.seed(1L)
test_poiss <- replicate(1000, confint.approx(update(mod_poiss, data = simulate_Aliens_GLM())))
test_binar <- replicate(1000, confint.approx(update(mod_binar, data = simulate_Aliens_GLM())))
test_binom <- replicate(1000, confint.approx(update(mod_binom, data = simulate_Aliens_GLM())))
test_gauss <- replicate(1000, confint.approx(update(mod_gauss, data = simulate_Aliens_GLM())))
rbind(poiss = apply(test_poiss, 1, mean),
      binar = apply(test_binar, 1, mean),
      binom = apply(test_binom, 1, mean),
      gauss = apply(test_gauss, 1, mean))

## ----coverage1b, warning = FALSE--------------------------------------------------------------------------------------
set.seed(3L)
test_poiss <- replicate(1000, confint.approx(update(mod_poiss, data = simulate_Aliens_GLM(N = 30))))
test_binar <- replicate(1000, confint.approx(update(mod_binar, data = simulate_Aliens_GLM(N = 30))))
test_binom <- replicate(1000, confint.approx(update(mod_binom, data = simulate_Aliens_GLM(N = 30))))
test_gauss <- replicate(1000, confint.approx(update(mod_gauss, data = simulate_Aliens_GLM(N = 30))))
rbind(poiss = apply(test_poiss, 1, mean, na.rm = TRUE),
      binar = apply(test_binar, 1, mean, na.rm = TRUE),
      binom = apply(test_binom, 1, mean, na.rm = TRUE),
      gauss = apply(test_gauss, 1, mean, na.rm = TRUE))

## ----coverage1c, error = TRUE-----------------------------------------------------------------------------------------
set.seed(1L)
test_poiss <- replicate(1000, confint.approx(update(mod_poiss, data = simulate_Aliens_GLM(N = 1000))))
test_binar <- replicate(1000, confint.approx(update(mod_binar, data = simulate_Aliens_GLM(N = 1000))))
test_binom <- replicate(1000, confint.approx(update(mod_binom, data = simulate_Aliens_GLM(N = 1000))))
test_gauss <- replicate(1000, confint.approx(update(mod_gauss, data = simulate_Aliens_GLM(N = 1000))))
rbind(poiss = apply(test_poiss, 1, mean),
      binar = apply(test_binar, 1, mean),
      binom = apply(test_binom, 1, mean),
      gauss = apply(test_gauss, 1, mean))

## ---- message = FALSE-------------------------------------------------------------------------------------------------
library(ellipse)
el <- ellipse(mod_poiss)
plot(el, type = "l")

## ---------------------------------------------------------------------------------------------------------------------
library(MASS)
confint(mod_poiss)
confint(mod_binar)

## ---------------------------------------------------------------------------------------------------------------------
prof <- profile(mod_poiss, alpha = (1 - 0.95)/4)  ## not sure why this alpha, but confint.glm does that
pro <- prof$`(Intercept)`
pro[1:3, ]
m <- update(mod_poiss, . ~ 0 + offset(-1.17095138 + 0.10732524 * humans_eaten))
sqrt((m$deviance - mod_poiss$deviance)/1)
sp <- spline(x = pro[, "par.vals"][, 1], y = pro[, 1])
approx(sp$y, sp$x, xout = qnorm(c(0.025, 0.975)))$y

## ---------------------------------------------------------------------------------------------------------------------
confint.profile <- function(mod, print = FALSE) {
  
  suppressMessages(intervals <- MASS:::confint.glm(mod))
  
  if (print) print(intervals)
  name.response <- mod$terms[[2]]
  if (length(name.response) == 3) name.response <- name.response[[2]]  ## for  binomial with cbind
  true.intercept <- attributes(mod$data)$param.eta[[paste(name.response)]][1]
  true.slope <- attributes(mod$data)$param.eta[[paste(name.response)]][2]
  if (print) cat("\n true parameters in 95% CI? \n")
  
  c(true.intercept > intervals["(Intercept)", "2.5 %"] & true.intercept < intervals["(Intercept)", "97.5 %"],
    true.slope > intervals["humans_eaten", "2.5 %"] & true.slope < intervals["humans_eaten", "97.5 %"])
}

## ----coverage2, message = FALSE, warning = FALSE----------------------------------------------------------------------
set.seed(1L)
test_poiss <- replicate(1000, confint.profile(update(mod_poiss, data = simulate_Aliens_GLM())))
test_binar <- replicate(1000, confint.profile(update(mod_binar, data = simulate_Aliens_GLM())))
test_binom <- replicate(1000, confint.profile(update(mod_binom, data = simulate_Aliens_GLM())))
test_gauss <- replicate(1000, confint.profile(update(mod_gauss, data = simulate_Aliens_GLM())))
rbind(poiss = apply(test_poiss, 1, mean),
      binar = apply(test_binar, 1, mean),
      binom = apply(test_binom, 1, mean),
      gauss = apply(test_gauss, 1, mean))

## ----coverage3, message = FALSE, warning = FALSE----------------------------------------------------------------------
set.seed(3L)
test_poiss <- replicate(1000, confint.profile(update(mod_poiss, data = simulate_Aliens_GLM(N = 30))))
test_binar <- replicate(1000, confint.profile(update(mod_binar, data = simulate_Aliens_GLM(N = 30))))
test_binom <- replicate(1000, confint.profile(update(mod_binom, data = simulate_Aliens_GLM(N = 30))))
test_gauss <- replicate(1000, confint.profile(update(mod_gauss, data = simulate_Aliens_GLM(N = 30))))
rbind(poiss = apply(test_poiss, 1, mean, na.rm = TRUE),
      binar = apply(test_binar, 1, mean, na.rm = TRUE),
      binom = apply(test_binom, 1, mean, na.rm = TRUE),
      gauss = apply(test_gauss, 1, mean, na.rm = TRUE))

## ----coverage4, message = FALSE, warning = FALSE----------------------------------------------------------------------
set.seed(1L)
test_poiss <- replicate(1000, confint.profile(update(mod_poiss, data = simulate_Aliens_GLM(N = 1000))))
test_binar <- replicate(1000, confint.profile(update(mod_binar, data = simulate_Aliens_GLM(N = 1000))))
test_binom <- replicate(1000, confint.profile(update(mod_binom, data = simulate_Aliens_GLM(N = 1000))))
test_gauss <- replicate(1000, confint.profile(update(mod_gauss, data = simulate_Aliens_GLM(N = 1000))))
rbind(poiss = apply(test_poiss, 1, mean),
      binar = apply(test_binar, 1, mean),
      binom = apply(test_binom, 1, mean),
      gauss = apply(test_gauss, 1, mean))

## ---------------------------------------------------------------------------------------------------------------------
set.seed(1)
new_betas <- replicate(100, {
  Aliens$newY <- as.matrix(simulate(mod_binar, 1))
  update(mod_binar, newY ~ .)$coefficients})
cbind(intercept = quantile(new_betas[1, ], c(0.025, 0.975)),
      slope = quantile(new_betas[2, ], c(0.025, 0.975)))

## ---------------------------------------------------------------------------------------------------------------------
confint.myboot <- function(mod, print = FALSE) {
  
  new_betas <- replicate(100, {
    mod$data$newY <- simulate(mod, 1)[, 1]
    update(mod, newY ~ ., data = mod$data)$coef})
 
  intervals <- rbind("(Intercept)" = quantile(new_betas[1, ], c(0.025, 0.975)),
      "humans_eaten" = quantile(new_betas[2, ], c(0.025, 0.975)))
  
  if (print) print(intervals)
  name.response <- mod$terms[[2]]
  if (length(name.response) == 3) name.response <- name.response[[2]]  ## for  binomial with cbind
  true.intercept <- attributes(mod$data)$param.eta[[paste(name.response)]][1]
  true.slope <- attributes(mod$data)$param.eta[[paste(name.response)]][2]
  if (print) cat("\n true parameters in 95% CI? \n")
  
  c(true.intercept > intervals["(Intercept)", 1] & true.intercept < intervals["(Intercept)", 2],
    true.slope > intervals["humans_eaten", 1] & true.slope < intervals["humans_eaten", 2])
}

## ----coverage boot, message = FALSE, warning = FALSE------------------------------------------------------------------
set.seed(1L)
test_poiss <- replicate(1000, {
  new.data <- simulate_Aliens_GLM()
  mod <- update(mod_poiss, data = new.data)
  confint.myboot(mod)
  })

test_binar <- replicate(1000, {
  new.data <- simulate_Aliens_GLM()
  mod <- update(mod_binar, data = new.data)
  confint.myboot(mod)
  })

test_binom <- replicate(1000, {
  new.data <- simulate_Aliens_GLM()
  mod <- update(mod_binom, data = new.data)
  confint.myboot(mod)
  })

test_gauss <- replicate(1000, {
  new.data <- simulate_Aliens_GLM()
  mod <- update(mod_gauss, data = new.data)
  confint.myboot(mod)
  })

## ---------------------------------------------------------------------------------------------------------------------
rbind(poiss = apply(test_poiss, 1, mean),
      binar = apply(test_binar, 1, mean),
      binom = apply(test_binom, 1, mean),
      gauss = apply(test_gauss, 1, mean))

## ---------------------------------------------------------------------------------------------------------------------
confint.myboot2 <- function(mod, print = FALSE) {
  new_betas <- replicate(100, {
    mod$data$newY <- simulate(mod, 1)[, 1]
    update(mod, newY ~ ., data = mod$data)$coef})
  
  intervals <- rbind(
    "(Intercept)" = boot::boot.ci(list(t0 = as.matrix(rep(coef(mod)[1][[1]], 100)),
                                       t = as.matrix(new_betas[1, ]), 
                                       R = 100), type = "basic")$basic[4:5],
    "humans_eaten" = boot::boot.ci(list(t0 = as.matrix(rep(coef(mod)[2][[1]], 100)),
                                        t = as.matrix(new_betas[2, ]),
                                        R = 100), type = "basic")$basic[4:5])
  if (print) print(intervals)
  name.response <- mod$terms[[2]]
  if (length(name.response) == 3) name.response <- name.response[[2]]  ## for  binomial with cbind
  true.intercept <- attributes(mod$data)$param.eta[[paste(name.response)]][1]
  true.slope <- attributes(mod$data)$param.eta[[paste(name.response)]][2]
  if (print) cat("\n true parameters in 95% CI? \n")
  c(true.intercept > intervals["(Intercept)", 1] & true.intercept < intervals["(Intercept)", 2],
    true.slope > intervals["humans_eaten", 1] & true.slope < intervals["humans_eaten", 2])
}

## ----coverage boot2, message = FALSE, warning = FALSE-----------------------------------------------------------------
set.seed(1L)
test_poiss <- replicate(1000, {
  new.data <- simulate_Aliens_GLM()
  mod <- update(mod_poiss, data = new.data)
  confint.myboot2(mod)
  })
test_binar <- replicate(1000, {
  new.data <- simulate_Aliens_GLM()
  mod <- update(mod_binar, data = new.data)
  confint.myboot2(mod)
  })
test_binom <- replicate(1000, {
  new.data <- simulate_Aliens_GLM()
  mod <- update(mod_binom, data = new.data)
  confint.myboot2(mod)
  })
test_gauss <- replicate(1000, {
  new.data <- simulate_Aliens_GLM()
  mod <- update(mod_gauss, data = new.data)
  confint.myboot2(mod)
  })

## ---------------------------------------------------------------------------------------------------------------------
rbind(poiss = apply(test_poiss, 1, mean),
      binar = apply(test_binar, 1, mean),
      binom = apply(test_binom, 1, mean),
      gauss = apply(test_gauss, 1, mean))

## ---------------------------------------------------------------------------------------------------------------------
mod_lm <- lm(size ~ humans_eaten, data = Aliens)
newdata <- data.frame(humans_eaten = c(5, 15))
predict(mod_lm, newdata = newdata, interval = "confidence")

pred <- predict(mod_gauss, newdata = newdata, se.fit = TRUE)
lwr <- pred$fit + qt(0.025, df = mod_lm$df.residual) * pred$se.fit
upr <- pred$fit + qt(0.975, df = mod_lm$df.residual) * pred$se.fit
pred.table <- cbind(fit = pred$fit, lwr, upr)
pred.table

## ---------------------------------------------------------------------------------------------------------------------
pred <- predict(mod_gauss, newdata = newdata, se.fit = TRUE)
lwr <- pred$fit + qt(0.025, df = mod_lm$df.residual) * pred$se.fit
upr <- pred$fit + qt(0.975, df = mod_lm$df.residual) * pred$se.fit
pred.table <- cbind(fit = pred$fit, lwr, upr)
pred.table

## ---------------------------------------------------------------------------------------------------------------------
pred <- predict(mod_gauss, newdata = newdata, se.fit = TRUE)
lwr <- pred$fit + qnorm(0.025) * pred$se.fit
upr <- pred$fit + qnorm(0.975) * pred$se.fit
pred.table <- cbind(fit = pred$fit, lwr, upr)
pred.table

## ---- message = FALSE-------------------------------------------------------------------------------------------------
library(spaMM)
mod_gauss_spaMM <- fitme(size ~ humans_eaten, family = gaussian(), data = Aliens, method = "REML")
p <- predict(mod_gauss_spaMM, newdata = newdata, intervals = "predVar")
attr(p, "intervals")
get_intervals(mod_gauss_spaMM, newdata = newdata, intervals = "predVar")

## ---------------------------------------------------------------------------------------------------------------------
pred <- predict(mod_poiss, newdata = newdata, se.fit = TRUE)
lwr <- pred$fit + qnorm(0.025) * pred$se.fit
upr <- pred$fit + qnorm(0.975) * pred$se.fit
cbind(lwr, upr)

mod_poiss_spaMM <- fitme(eggs ~ humans_eaten, family = poisson(), data = Aliens, method = "REML")
log(get_intervals(mod_poiss_spaMM, newdata = newdata, intervals = "predVar"))  ## log to get eta!

## ---------------------------------------------------------------------------------------------------------------------
pred <- predict(mod_poiss, newdata = newdata, se.fit = TRUE)
lwr <- pred$fit + qnorm(0.025) * pred$se.fit
upr <- pred$fit + qnorm(0.975) * pred$se.fit
exp(cbind(lwr, upr))

get_intervals(mod_poiss_spaMM, newdata = newdata, intervals = "predVar")

## ---------------------------------------------------------------------------------------------------------------------
pred <- predict(mod_gauss, newdata = newdata, se.fit = TRUE)
lwr <- pred$fit + qt(0.025, df = mod_lm$df.residual) * sqrt(pred$se.fit^2 + pred$residual.scale^2)
upr <- pred$fit + qt(0.975, df = mod_lm$df.residual) * sqrt(pred$se.fit^2 + pred$residual.scale^2)
pred.table <- cbind(fit = pred$fit, lwr, upr)
pred.table
predict(mod_lm, newdata = newdata, interval = "prediction") ## uses t distribution

## ---------------------------------------------------------------------------------------------------------------------
lwr <- pred$fit + qnorm(0.025) * sqrt(pred$se.fit^2 + pred$residual.scale^2)
upr <- pred$fit + qnorm(0.975) * sqrt(pred$se.fit^2 + pred$residual.scale^2)
cbind(lwr, upr)
get_intervals(mod_gauss_spaMM, newdata = newdata, intervals = "respVar")  ## uses gaussian approximation

## ---------------------------------------------------------------------------------------------------------------------
pred <- predict(mod_poiss, newdata = newdata, se.fit = TRUE)
lwr <- pred$fit + qnorm(0.025) * sqrt(pred$se.fit^2 + poisson()$variance(exp(pred$fit)))
upr <- pred$fit + qnorm(0.975) * sqrt(pred$se.fit^2 + poisson()$variance(exp(pred$fit)))
exp(cbind(lwr, upr))  ## don't forget the inverse link!
get_intervals(mod_poiss_spaMM, newdata = newdata, intervals = "respVar")  ## uses gaussian approximation

## ---------------------------------------------------------------------------------------------------------------------
set.seed(1L)
Aliens2 <- simulate_Aliens_GLM(N = 20)
mod_lm2    <- lm(size  ~ humans_eaten, data = Aliens2)
mod_gauss2 <- glm(size  ~ humans_eaten, family = gaussian(), data = Aliens2)
mod_poiss2 <- glm(eggs  ~ humans_eaten, family = poisson(),  data = Aliens2)
mod_binar2 <- glm(happy ~ humans_eaten, family = binomial(), data = Aliens2)
mod_gauss2_spaMM  <- fitme(size  ~ humans_eaten, family = gaussian(), data = Aliens2) ## not REML!
mod_gauss2_spaMM0 <- fitme(size  ~ 1, family = gaussian(), data = Aliens2)
mod_poiss2_spaMM  <- fitme(eggs  ~ humans_eaten, family = poisson(),  data = Aliens2)
mod_poiss2_spaMM0 <- fitme(eggs  ~ 1, family = poisson(),  data = Aliens2)
mod_binar2_spaMM <- fitme(happy ~ humans_eaten, family = binomial(), data = Aliens2)

## ---------------------------------------------------------------------------------------------------------------------
summary(mod_lm2)

## ---------------------------------------------------------------------------------------------------------------------
summary(mod_gauss2)

## ---------------------------------------------------------------------------------------------------------------------
summary(mod_gauss2)$coef
summary(mod_lm2)$coef

## ---------------------------------------------------------------------------------------------------------------------
summary(mod_poiss2)$coef
z <- (mod_poiss2$coef - 0) / sqrt(diag(vcov(mod_poiss2)))
pvalues <- 2 * (1 - pnorm(abs(z)))
cbind(z, pvalues)

## ---- cache = FALSE---------------------------------------------------------------------------------------------------
summary(mod_binar2)$coef
z <- (mod_binar2$coef - 0) / sqrt(diag(vcov(mod_binar2)))
pvalues <- 2 * (1 - pnorm(abs(z)))
cbind(z, pvalues)

## ---------------------------------------------------------------------------------------------------------------------
library(car)
Anova(mod_gauss2, test = "F")

## ---------------------------------------------------------------------------------------------------------------------
Anova(mod_gauss2, test = "LR")
anova(mod_gauss2_spaMM, mod_gauss2_spaMM0)  ## "same" with spaMM
c(-2 * (logLik(update(mod_gauss2, . ~ 1)) - logLik(mod_gauss2)), 
  (deviance(update(mod_gauss2, . ~ 1)) - deviance(mod_gauss2)) / summary(mod_gauss2)$dispersion)

## ---------------------------------------------------------------------------------------------------------------------
summary(mod_gauss2)$coef["humans_eaten", "t value"]
summary(mod_gauss2)$coef["humans_eaten", "t value"]^2
Anova(mod_gauss2, test = "F")

## ---------------------------------------------------------------------------------------------------------------------
plot(ecdf(rt(10000, df = 5)^2), col = "blue", lwd = 3, main = "")
plot(ecdf(rf(10000, df1 = 1, df2 = 5)), add = TRUE, col = "red", lwd = 2)
plot(ecdf(rchisq(10000, df = 1)), add = TRUE, col = "green", lwd = 1)

## ---------------------------------------------------------------------------------------------------------------------
plot(ecdf(rt(10000, df = 500)^2), col = "blue", lwd = 3, main = "")
plot(ecdf(rf(10000, df1 = 1, df2 = 500)), add = TRUE, col = "red", lwd = 2)
plot(ecdf(rchisq(10000, df = 1)), add = TRUE, col = "green", lwd = 1)

## ---------------------------------------------------------------------------------------------------------------------
Anova(mod_poiss2, test = "LR")
LR <- -2 * (logLik(update(mod_poiss2, . ~ 1)) - logLik(mod_poiss2))
pvalue <- 1 - pchisq(LR, 1)
cbind(LR, pvalue)

## ----lrt boot 1, message=FALSE----------------------------------------------------------------------------------------
anova(mod_gauss2_spaMM, mod_gauss2_spaMM0, boot.repl = 200)

## ----lrt boot 2, message=FALSE----------------------------------------------------------------------------------------
anova(mod_poiss2_spaMM, mod_poiss2_spaMM0, boot.repl = 200)

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
mean(logLikH0 < (logLik(mod_lm) - logLik(mod_glm)))

