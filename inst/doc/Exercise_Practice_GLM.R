## ----setup, include=FALSE------------------------------------------------
library(LM2GLMM)
library(car)
knitr::opts_chunk$set(cache = FALSE, fig.align = "center", fig.width = 6, fig.height = 6)

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

