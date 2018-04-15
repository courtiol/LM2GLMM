## ----setup, include=FALSE------------------------------------------------
library(LM2GLMM)
library(car)
knitr::opts_chunk$set(cache = TRUE, fig.align = "center", fig.width = 6, fig.height = 6,
                      cache.path = "./cache_knitr/Exo_GLM/", fig.path = "./fig_knitr/Exo_GLM/")

## ------------------------------------------------------------------------
mod_insect <- lm(count ~ spray, data = InsectSprays)

## ------------------------------------------------------------------------
InsectSprays$count_bis <- InsectSprays$count + 1
mod_insect_bis <- update(mod_insect, count_bis ~ .)
bc <- car::powerTransform(mod_insect_bis)
InsectSprays$count_bc <- car::bcPower(InsectSprays$count_bis, bc$lambda)
mod_insect_bc <- update(mod_insect_bis, count_bc ~ .)

## ------------------------------------------------------------------------
mod_insect_glm <- glm(count ~ spray, data = InsectSprays, family = poisson())

## ------------------------------------------------------------------------
anova(mod_insect, update(mod_insect, . ~ 1))
anova(mod_insect_bc, update(mod_insect_bc, . ~ 1))
anova(mod_insect_glm, update(mod_insect_glm, . ~ 1), test = "LR")

## ------------------------------------------------------------------------
data.for.pred <- data.frame(spray = levels(InsectSprays$spray))
pred_lm <- predict(mod_insect, newdata = data.for.pred, interval = "confidence")
pred_bc <- predict(mod_insect_bc, newdata = data.for.pred, interval = "confidence")
unboxcox <- function(x, lambda) (x*lambda + 1)^(1/lambda)
pred_bc_unboxcox <- unboxcox(x = pred_bc, lambda = bc$lambda) - 1  ## we remove one as we added one so to be able to do the BoxCox
pred_glm_eta <- predict(mod_insect_glm, newdata = data.for.pred, se.fit = TRUE)
pred_glm_table <- data.frame(fit = exp(pred_glm_eta$fit))
pred_glm_table$lwr <- exp(pred_glm_eta$fit + qnorm(0.025)*pred_glm_eta$se.fit)
pred_glm_table$upr <- exp(pred_glm_eta$fit + qnorm(0.975)*pred_glm_eta$se.fit)

## ------------------------------------------------------------------------
plot(InsectSprays$count ~  as.numeric(InsectSprays$spray), xlim = c(0.5, 6.5), ylim = c(0, 30),
     ylab = "Predicted number of instects", xlab = "Sprays", axes = FALSE)
axis(2, las = 2)
axis(1, at = 1:6, labels = levels(InsectSprays$spray))
box()

points(pred_lm[, "fit"] ~  I(-0.2 + 1:6), col = "blue", pch = 20)

arrows(x0 = -0.2 + 1:6, x1 = -0.2 + 1:6, y0 = pred_lm[, "lwr"],
       y1 = pred_lm[, "upr"], code = 3, col = "blue", angle = 90, length = 0.1)

points(pred_bc_unboxcox[, "fit"] ~  I(-0.1 + 1:6), col = "green", pch = 20)

arrows(x0 = -0.1 + 1:6, x1 = -0.1 + 1:6, y0 = pred_bc_unboxcox[, "lwr"],
       y1 = pred_bc_unboxcox[, "upr"], code = 3, col = "green", angle = 90, length = 0.1)

points(pred_glm_table[, "fit"] ~  I(0.1 + 1:6), col = "red", pch = 20)

arrows(x0 = 0.1 + 1:6, x1 = 0.1 + 1:6, y0 = pred_glm_table[, "lwr"],
       y1 = pred_glm_table[, "upr"], code = 3, col = "red", angle = 90, length = 0.1)

legend("top", horiz = TRUE, fill = c("blue", "green", "red"), legend = c("LM", "LM + BoxCox", "GLM Poisson"), bty = "n")

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
p <- predict(mod_binar, newdata = data.for.pred, se.fit = TRUE)
p$upr <- p$fit + qnorm(0.975) * sqrt(p$se.fit^2 + binomial()$variance(plogis(p$fit)))
p$lwr <- p$fit + qnorm(0.025) * sqrt(p$se.fit^2 + binomial()$variance(plogis(p$fit)))
p.mu <- data.frame(fit = plogis(p$fit),
                   upr = binomial()$linkinv(p$upr), ## same as plogis
                   lwr = 1/(1 + exp(-p$lwr))) ## same as plogis

plot(p.mu$fit ~ data.for.pred$humans_eaten, type = "l", lwd = 2, las = 1, ylim = c(0, 1),
     ylab = "Probability of being happy", xlab = "Number of humans eaten")
points(p.mu$upr ~  data.for.pred$humans_eaten, type = "l", lwd = 2, lty = 2)
points(p.mu$lwr ~  data.for.pred$humans_eaten, type = "l", lwd = 2, lty = 2)

## ------------------------------------------------------------------------
p2 <- predict(mod_binar_spaMM, newdata = data.for.pred, intervals = "respVar")
plot(p2 ~ data.for.pred$humans_eaten, type = "l", lwd = 2, las = 1, ylim = c(0, 1),
     col = NULL, ylab = "Probability of being happy", xlab = "Number of humans eaten")
lwr <- attr(p2, "interval")[, 1]
upr <- attr(p2, "interval")[, 2]
polygon(y = c(lwr, rev(upr), lwr[1]),
        x = c(0:15, 15:0, 0), col = "green", border = NA)
points(p2 ~  data.for.pred$humans_eaten, type = "l", lwd = 2)

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

## ---- message = FALSE----------------------------------------------------
library(effects)
plot(allEffects(mod_poiss))

library(visreg)
visreg(mod_poiss, scale = "response")

## ------------------------------------------------------------------------
p$upr <- p$fit + qnorm(0.975) * sqrt(p$se.fit^2 + poisson()$variance(exp(p$fit)))
p$lwr <- p$fit + qnorm(0.025) * sqrt(p$se.fit^2 + poisson()$variance(exp(p$fit)))
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

## ------------------------------------------------------------------------
HSE98women$menopause <-  HSE98women$period == FALSE

## ------------------------------------------------------------------------
mod_menop_logit <- glm(menopause ~ age + bmi + smoked, data = HSE98women, family = binomial())
nrow(HSE98women) ## original number of observations
nrow(mod_menop_logit$model)  ## number of observations considered in the model

## ------------------------------------------------------------------------
summary(HSE98women[, c("menopause", "age", "bmi", "smoked")])
summary(mod_menop_logit$model)

## ------------------------------------------------------------------------
cor.test(HSE98women$age, HSE98women$bmi)
cor.test(mod_menop_logit$model$age, mod_menop_logit$model$bmi)

## ------------------------------------------------------------------------
library(car)
mod_menop_logit$model$smoked <- as.factor(mod_menop_logit$model$smoked) ## patch to solve bug in car 3.0
scatterplot(menopause ~ age + smoked, data = mod_menop_logit$model)
scatterplot(menopause ~ bmi + smoked, data = mod_menop_logit$model)

## ------------------------------------------------------------------------
mod_menop_logit   <- glm(menopause ~ age * bmi + smoked, data = HSE98women, family = binomial())
mod_menop_probit  <- glm(menopause ~ age * bmi + smoked, data = HSE98women, family = binomial(link = "probit"))
mod_menop_cauchit <- glm(menopause ~ age * bmi + smoked, data = HSE98women, family = binomial(link = "cauchit"))

## ---- message=FALSE------------------------------------------------------
library(mgcv)
mod_menop_logit_gam   <- gam(menopause ~ s(age, bmi) + smoked, data = HSE98women, family = binomial())
mod_menop_probit_gam  <- gam(menopause ~ s(age, bmi) + smoked, data = HSE98women, family = binomial(link = "probit"))
mod_menop_cauchit_gam <- gam(menopause ~ s(age, bmi) + smoked, data = HSE98women, family = binomial(link = "cauchit"))

## ------------------------------------------------------------------------
cbind(logit = AIC(mod_menop_logit),
      probit = AIC(mod_menop_probit),
      cauchit = AIC(mod_menop_cauchit),
      logit_GAM = AIC(mod_menop_logit_gam),
      probit_GAM = AIC(mod_menop_probit_gam),
      cauchit_GAM = AIC(mod_menop_cauchit_gam)
      )

## ------------------------------------------------------------------------
mod_menop_logit_gam_no_int <- gam(menopause ~ s(age) + s(bmi) + smoked, data = HSE98women, family = binomial())
LR <- -2 * (logLik(mod_menop_logit_gam_no_int)[[1]] - logLik(mod_menop_logit_gam)[[1]])
DF <- (mod_menop_logit_gam$df.null - mod_menop_logit_gam$df.residual) - (mod_menop_logit_gam_no_int$df.null - mod_menop_logit_gam_no_int$df.residual)
(PV <- 1 - pchisq(LR, DF))

## ------------------------------------------------------------------------
mod_menop_logit_gam_no_bmi <- gam(menopause ~ s(age) + smoked, data = HSE98women, family = binomial())
LR <- -2 * (logLik(mod_menop_logit_gam_no_bmi)[[1]] - logLik(mod_menop_logit_gam)[[1]])
DF <- (mod_menop_logit_gam$df.null - mod_menop_logit_gam$df.residual) - (mod_menop_logit_gam_no_bmi$df.null - mod_menop_logit_gam_no_bmi$df.residual)
(PV <- 1 - pchisq(LR, DF))

## ------------------------------------------------------------------------
mod_menop_logit_gam_no_age <- gam(menopause ~ s(bmi) + smoked, data = HSE98women, family = binomial())
LR <- -2 * (logLik(mod_menop_logit_gam_no_age)[[1]] - logLik(mod_menop_logit_gam)[[1]])
DF <- (mod_menop_logit_gam$df.null - mod_menop_logit_gam$df.residual) - (mod_menop_logit_gam_no_age$df.null - mod_menop_logit_gam_no_age$df.residual)
(PV <- 1 - pchisq(LR, DF))

## ------------------------------------------------------------------------
mod_menop_logit_gam_never_smoked <- gam(menopause ~ s(age, bmi), data = HSE98women, family = binomial())
LR <- -2 * (logLik(mod_menop_logit_gam_never_smoked)[[1]] - logLik(mod_menop_logit_gam)[[1]])
DF <- (mod_menop_logit_gam$df.null - mod_menop_logit_gam$df.residual) - (mod_menop_logit_gam_never_smoked$df.null - mod_menop_logit_gam_never_smoked$df.residual)
(PV <- 1 - pchisq(LR, DF))

## ------------------------------------------------------------------------
library(visreg)
visreg(mod_menop_logit_gam)

## ------------------------------------------------------------------------
visreg(mod_menop_logit_gam, scale = "response")

## ------------------------------------------------------------------------
age.for.pred <- seq(min(mod_menop_cauchit_gam$model$age), max(mod_menop_cauchit_gam$model$age), length = 100)
data.for.pred <- expand.grid(
  age = age.for.pred,
  bmi = median(mod_menop_cauchit_gam$model$bmi),
  smoked = TRUE
  )

  gam.p <- predict(mod_menop_logit_gam, newdata = data.for.pred, se.fit = TRUE)
gam.upr <- binomial(link = "logit")$linkinv(gam.p$fit + qnorm(0.975) * gam.p$se.fit)
gam.lwr <- binomial(link = "logit")$linkinv(gam.p$fit + qnorm(0.025) * gam.p$se.fit)
gam.fit <- binomial(link = "logit")$linkinv(gam.p$fit)

cauchit.p <- predict(mod_menop_cauchit, newdata = data.for.pred, se.fit = TRUE)
cauchit.upr <- binomial(link = "cauchit")$linkinv(cauchit.p$fit + qnorm(0.975) * cauchit.p$se.fit)
cauchit.lwr <- binomial(link = "cauchit")$linkinv(cauchit.p$fit + qnorm(0.025) * cauchit.p$se.fit)
cauchit.fit <- binomial(link = "cauchit")$linkinv(cauchit.p$fit)

logit.p <- predict(mod_menop_logit, newdata = data.for.pred, se.fit = TRUE)
logit.upr <- binomial(link = "logit")$linkinv(logit.p$fit + qnorm(0.975) * logit.p$se.fit)
logit.lwr <- binomial(link = "logit")$linkinv(logit.p$fit + qnorm(0.025) * logit.p$se.fit)
logit.fit <- binomial(link = "logit")$linkinv(logit.p$fit)

plot(gam.fit ~ age.for.pred, type = "l", las = 1, ylab = "Probability of being menopaused", xlab = "Age (yrs)", ylim = c(0, 1))
points(gam.upr ~ age.for.pred, type = "l", lty = 2)
points(gam.lwr ~ age.for.pred, type = "l", lty = 2)

points(cauchit.fit ~ age.for.pred, type = "l", lty = 1, col = "green")
points(cauchit.upr ~ age.for.pred, type = "l", lty = 2, col = "green")
points(cauchit.lwr ~ age.for.pred, type = "l", lty = 2, col = "green")

points(logit.fit ~ age.for.pred, type = "l", lty = 1, col = "red")
points(logit.upr ~ age.for.pred, type = "l", lty = 2, col = "red")
points(logit.lwr ~ age.for.pred, type = "l", lty = 2, col = "red")

legend("topleft", fill = c("black", "green", "red"), legend = c("logit GAM", "cauchit", "logit"), bty = "n")

## ------------------------------------------------------------------------
data.for.pred <- expand.grid(
  age = age.for.pred,
  bmi = c(18.5, 25, 30),
  smoked = TRUE
  )

gam.skiny.p <- predict(mod_menop_logit_gam, newdata = subset(data.for.pred, bmi == 18.5), se.fit = TRUE)
gam.skiny.upr <- binomial(link = "logit")$linkinv(gam.skiny.p$fit + qnorm(0.975) * gam.skiny.p$se.fit)
gam.skiny.lwr <- binomial(link = "logit")$linkinv(gam.skiny.p$fit + qnorm(0.025) * gam.skiny.p$se.fit)
gam.skiny.fit <- binomial(link = "logit")$linkinv(gam.skiny.p$fit)

gam.overweight.p <- predict(mod_menop_logit_gam, newdata = subset(data.for.pred, bmi == 25), se.fit = TRUE)
gam.overweight.upr <- binomial(link = "logit")$linkinv(gam.overweight.p$fit + qnorm(0.975) * gam.overweight.p$se.fit)
gam.overweight.lwr <- binomial(link = "logit")$linkinv(gam.overweight.p$fit + qnorm(0.025) * gam.overweight.p$se.fit)
gam.overweight.fit <- binomial(link = "logit")$linkinv(gam.overweight.p$fit)

gam.obese.p <- predict(mod_menop_logit_gam, newdata = subset(data.for.pred, bmi == 30), se.fit = TRUE)
gam.obese.upr <- binomial(link = "logit")$linkinv(gam.obese.p$fit + qnorm(0.975) * gam.obese.p$se.fit)
gam.obese.lwr <- binomial(link = "logit")$linkinv(gam.obese.p$fit + qnorm(0.025) * gam.obese.p$se.fit)
gam.obese.fit <- binomial(link = "logit")$linkinv(gam.obese.p$fit)

plot(gam.skiny.fit ~ age.for.pred, type = "l", las = 1, ylab = "Probability of being menopaused", xlab = "Age (yrs)", ylim = c(0, 1))
points(gam.skiny.upr ~ age.for.pred, type = "l", lty = 2)
points(gam.skiny.lwr ~ age.for.pred, type = "l", lty = 2)

points(gam.overweight.fit ~ age.for.pred, type = "l", lty = 1, col = "green")
points(gam.overweight.upr ~ age.for.pred, type = "l", lty = 2, col = "green")
points(gam.overweight.lwr ~ age.for.pred, type = "l", lty = 2, col = "green")

points(gam.obese.fit ~ age.for.pred, type = "l", lty = 1, col = "red")
points(gam.obese.upr ~ age.for.pred, type = "l", lty = 2, col = "red")
points(gam.obese.lwr ~ age.for.pred, type = "l", lty = 2, col = "red")

legend("topleft", fill = c("black", "green", "red"), legend = c("skiny", "overweight", "obese"), bty = "n")

## ------------------------------------------------------------------------
data.for.pred <- expand.grid(
  age = c(40, 45, 50, 55, 60),
  bmi = c(21.75, 30),
  smoked = TRUE
  )
pred_normal <- predict(mod_menop_logit_gam, newdata = subset(data.for.pred, bmi == 21.75), se.fit = TRUE)
tab_normal <- cbind(
  predict = plogis(pred_normal$fit),
  lwr.CI = plogis(pred_normal$fit + qnorm(0.025)*pred_normal$se.fit),
  upr.CI = plogis(pred_normal$fit + qnorm(0.975)*pred_normal$se.fit)
)
rownames(tab_normal) <- c(40, 45, 50, 55, 60)
round(tab_normal, 2)

## ------------------------------------------------------------------------
pred_obses <- predict(mod_menop_logit_gam, newdata = subset(data.for.pred, bmi == 30), se.fit = TRUE)
tab_obses <- cbind(
  predict = plogis(pred_obses$fit),
  lwr.CI = plogis(pred_obses$fit + qnorm(0.025)*pred_obses$se.fit),
  upr.CI = plogis(pred_obses$fit + qnorm(0.975)*pred_obses$se.fit)
)
rownames(tab_obses) <- c(40, 45, 50, 55, 60)
round(tab_obses, 2)

## ------------------------------------------------------------------------
data.for.pred <- expand.grid(age = c(40, 45, 50, 55, 60), bmi = 21.75, smoked = TRUE)
pred_GAM <- predict(mod_menop_logit_gam, newdata = subset(data.for.pred, bmi == 21.75), se.fit = TRUE)
pred_logit <- predict(mod_menop_logit, newdata = subset(data.for.pred, bmi == 21.75), se.fit = TRUE)
pred_cauchit <- predict(mod_menop_cauchit, newdata = subset(data.for.pred, bmi == 21.75), se.fit = TRUE)
tab <- cbind(
  logit_GAM = plogis(pred_GAM$fit),
  logit_GLM = plogis(pred_logit$fit),
  cauchit_GLM = binomial(link = "cauchit")$linkinv(pred_cauchit$fit)
)
rownames(tab) <- c(40, 45, 50, 55, 60)
round(tab, 2)

## ------------------------------------------------------------------------
tab <- with(HSE98women,
     rbind(sum(age >= 39 & age <= 41 & bmi > 18.5 & bmi < 25, na.rm = TRUE),
           sum(age >= 44 & age <= 46 & bmi > 18.5 & bmi < 25, na.rm = TRUE),
           sum(age >= 49 & age <= 51 & bmi > 18.5 & bmi < 25, na.rm = TRUE),
           sum(age >= 54 & age <= 56 & bmi > 18.5 & bmi < 25, na.rm = TRUE),
           sum(age >= 59 & age <= 61 & bmi > 18.5 & bmi < 25, na.rm = TRUE)
           )
)
rownames(tab) <- c(40, 45, 50, 55, 60)
tab

## ------------------------------------------------------------------------
tab <- with(HSE98women, 
     rbind(
       mean(menopause[age >= 39 & age <= 41 & bmi > 18.5 & bmi < 25], na.rm = TRUE),
       mean(menopause[age >= 44 & age <= 46 & bmi > 18.5 & bmi < 25], na.rm = TRUE),
       mean(menopause[age >= 49 & age <= 51 & bmi > 18.5 & bmi < 25], na.rm = TRUE),
       mean(menopause[age >= 54 & age <= 56 & bmi > 18.5 & bmi < 25], na.rm = TRUE),
       mean(menopause[age >= 59 & age <= 61 & bmi > 18.5 & bmi < 25], na.rm = TRUE)
     )
)
rownames(tab) <- c(40, 45, 50, 55, 60)
round(tab, 2)

## ------------------------------------------------------------------------
data.for.pred.nosmoke <- expand.grid(
  age = age.for.pred,
  bmi = median(mod_menop_cauchit_gam$model$bmi),
  smoked = FALSE
  )

gam.p.nosmoke <- predict(mod_menop_logit_gam, newdata = data.for.pred.nosmoke, se.fit = TRUE)
gam.upr.nosmoke <- binomial(link = "logit")$linkinv(gam.p.nosmoke$fit + qnorm(0.975) * gam.p.nosmoke$se.fit)
gam.lwr.nosmoke <- binomial(link = "logit")$linkinv(gam.p.nosmoke$fit + qnorm(0.025) * gam.p.nosmoke$se.fit)
gam.fit.nosmoke <- binomial(link = "logit")$linkinv(gam.p.nosmoke$fit)


plot(gam.fit ~ age.for.pred, type = "l", las = 1, ylab = "Probability of being menopaused", xlab = "Age (yrs)", ylim = c(0, 1), col = "red")
points(gam.upr ~ age.for.pred, type = "l", lty = 2, col = "red")
points(gam.lwr ~ age.for.pred, type = "l", lty = 2, col = "red")

points(gam.fit.nosmoke ~ age.for.pred, type = "l", col = "green")
points(gam.upr.nosmoke ~ age.for.pred, type = "l", lty = 2, col = "green")
points(gam.lwr.nosmoke ~ age.for.pred, type = "l", lty = 2, col = "green")

legend("topleft", fill = c("green", "red"), legend = c("non-smoker", "smoker"), bty = "n")

