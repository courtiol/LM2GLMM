## ----setup, include=FALSE------------------------------------------------
library(LM2GLMM)
library(car)
knitr::opts_chunk$set(cache = FALSE, fig.align = "center", fig.width = 6, fig.height = 6)

## ---- fig.width = 8------------------------------------------------------
str(chickwts)
plot(weight ~ feed, data = chickwts)

## ------------------------------------------------------------------------
crossprod(model.matrix( ~ feed, data = chickwts))

## ------------------------------------------------------------------------
table(chickwts$feed)

## ------------------------------------------------------------------------
any(is.na(chickwts))

## ------------------------------------------------------------------------
(mod_chick <- lm(weight ~ feed, data = chickwts))

## ------------------------------------------------------------------------
par(mfrow = c(2, 2))
plot(mod_chick)

## ------------------------------------------------------------------------
anova(mod_chick)

## ------------------------------------------------------------------------
pred <- predict(mod_chick, newdata = data.frame(feed = levels(chickwts$feed)),
        interval = "confidence")
rownames(pred) <- levels(chickwts$feed)
pred

## ---- results="hide"-----------------------------------------------------
plot(pred[, "fit"] ~ I(1:nrow(pred) + 0.05), axes = FALSE, pch = 20,
     ylim = range(mod_chick$model$weight),
     ylab = "Chicken weight (gr)", xlab = "Diet")
axis(1, 1:nrow(pred), labels = rownames(pred))
axis(2)
box()
for (row in 1:nrow(pred)) {
  arrows(x0 = row + 0.05, y0 = pred[row, "lwr"], x1 = row + 0.05, y1 = pred[row, "upr"],
         code = 3, angle = 90, length = 0.1)
  diet <- levels(chickwts$feed)[row]
  weights.to.plot <- chickwts$weight[chickwts$feed == diet]
  points(rep(row, length(weights.to.plot)) - 0.05, weights.to.plot, col = "grey")
  }

## ---- message = FALSE, fig.width = 8-------------------------------------
summary(posthoc <- multcomp::glht(mod_chick, linfct = multcomp::mcp(feed = "Tukey")))
par(oma = c(0, 8, 0, 0))
plot(posthoc)

## ------------------------------------------------------------------------
pred2 <- predict(mod_chick,
                newdata = data.frame(feed = levels(chickwts$feed)),
                interval = "prediction",
                se.fit = TRUE)

se.pred <- sqrt(pred2$se.fit^2 + pred2$residual.scale^2)

freq_more300 <- pnorm(rep(300, 6),
                      mean = pred2$fit[, "fit"],
                      sd = se.pred, lower.tail = FALSE)

output <- cbind(pred2$fit, "%>300gr" = round(freq_more300, 2))

rownames(output) <- levels(chickwts$feed)

output

## ---- fig.width = 8------------------------------------------------------
barplot(sort(output[, "%>300gr"]), ylim = c(0, 1), ylab = "Predicted proportion of chicken larger than 300 gr")

## ------------------------------------------------------------------------
rm(list = ls())

## ------------------------------------------------------------------------
str(InsectSprays)
range(InsectSprays$count)
table(InsectSprays$spray)
boxplot(count ~ spray, data = InsectSprays)
any(is.na(InsectSprays))

## ------------------------------------------------------------------------
(mod_insect <- lm(count ~ spray, data = InsectSprays))

## ------------------------------------------------------------------------
par(mfrow = c(2, 2))
plot(mod_insect)

## ------------------------------------------------------------------------
lmtest::bptest(mod_insect)

## ------------------------------------------------------------------------
InsectSprays$count_bis <- InsectSprays$count + 1
mod_insect_bis <- update(mod_insect, count_bis ~ .)

## ------------------------------------------------------------------------
car::boxCox(mod_insect_bis)
summary(bc <- car::powerTransform(mod_insect_bis))

## ------------------------------------------------------------------------
InsectSprays$count_bc <- car::bcPower(InsectSprays$count_bis, bc$lambda)
mod_insect_bc <- update(mod_insect_bis, count_bc ~ .)
par(mfrow = c(2, 2))
plot(mod_insect_bc)

## ------------------------------------------------------------------------
lmtest::bptest(mod_insect_bc)

## ------------------------------------------------------------------------
lmtest::dwtest(mod_insect_bc)
car::durbinWatsonTest(mod_insect_bc)

## ------------------------------------------------------------------------
nortest::lillie.test(mod_insect_bc$residuals)
shapiro.test(mod_insect_bc$residuals)

## ------------------------------------------------------------------------
anova(mod_insect_bc)

## ------------------------------------------------------------------------
(meanC_BC <- predict(mod_insect_bc, newdata = data.frame(spray = "C"), interval = "confidence"))

## ------------------------------------------------------------------------
(meanC <- ((meanC_BC * bc$lambda) + 1)^(1/bc$lambda) - 1)

## ------------------------------------------------------------------------
rm(list = ls())

## ------------------------------------------------------------------------
str(swiss)
summary(swiss)
pairs(swiss)
any(is.na(swiss))

## ------------------------------------------------------------------------
mod_swiss <- lm(Fertility ~ ., data = swiss)

## ------------------------------------------------------------------------
for (col in 2:ncol(model.matrix(mod_swiss))) {
  plot(rstandard(mod_swiss) ~ model.matrix(mod_swiss)[, col])
  title(main = colnames(model.matrix(mod_swiss))[col])
}

## ------------------------------------------------------------------------
cor(model.matrix(mod_swiss)[, -1])  ## we discard the intercept for that

## ------------------------------------------------------------------------
cov2cor(vcov(mod_swiss))

## ------------------------------------------------------------------------
vif(mod_swiss)

## ------------------------------------------------------------------------
mod_swiss_bis <- update(mod_swiss, . ~ . - Examination)
vif(mod_swiss_bis)

## ------------------------------------------------------------------------
par(mfrow = c(2, 2))
plot(mod_swiss)
plot(mod_swiss_bis)

## ------------------------------------------------------------------------
mod_swiss_H0 <- update(mod_swiss, . ~ 1)
anova(mod_swiss_H0, mod_swiss)
anova(mod_swiss_H0, mod_swiss_bis)

## ---- fig.height = 8, fig.width = 8--------------------------------------
plot(effects::allEffects(mod_swiss))
plot(effects::allEffects(mod_swiss_bis))

## ------------------------------------------------------------------------
data.for.pred <- expand.grid("Agriculture" = mean(swiss$Agriculture), 
                       "Examination" = mean(swiss$Examination),
                       "Education" = seq(min(swiss$Education), max(swiss$Education), length = 30),
                       "Catholic" = mean(swiss$Catholic),
                       "Infant.Mortality" = mean(swiss$Infant.Mortality)
                       )
pred1 <- predict(mod_swiss, newdata = data.for.pred, interval = "confidence")
pred2 <- predict(mod_swiss_bis, newdata = data.for.pred, interval = "confidence")
plot(pred1[, "fit"] ~ data.for.pred$Education, col = "blue", lwd = 2, type = "l",
     ylab = "Predicted Standardized Fertility (+/- 95% CI)", xlab = "Education (%)")
points(pred1[, "lwr"] ~ data.for.pred$Education,
       col = "blue", lwd = 2, lty = 2, type = "l")
points(pred1[, "upr"] ~ data.for.pred$Education,
       col = "blue", lwd = 2, lty = 2, type = "l")
points(pred2[, "fit"] ~ data.for.pred$Education,
       col = "orange", lwd = 2, type = "l")
points(pred2[, "lwr"] ~ data.for.pred$Education,
       col = "orange", lwd = 2, lty = 2, type = "l")
points(pred2[, "upr"] ~ data.for.pred$Education,
       col = "orange", lwd = 2, lty = 2, type = "l")
legend("topright", fill = c("blue", "orange"),
       legend = c("mod_swiss", "mod_swiss_bis"), title = "Model:", bty = "n")

## ------------------------------------------------------------------------
rm(list = ls())

## ----trees---------------------------------------------------------------
str(trees)
summary(trees)
pairs(trees)
any(is.na(trees))

## ------------------------------------------------------------------------
trees$log_Volume <- log(trees$Volume)
trees$log_Height <- log(trees$Height)
trees$log_Girth <- log(trees$Girth)
mod_trees <- lm(log_Volume ~ 1 + offset(log_Height + 2 * log_Girth), data = trees)

## ------------------------------------------------------------------------
par(mfrow = c(2, 2))
plot(mod_trees)

## ------------------------------------------------------------------------
coef(mod_trees)
confint(mod_trees)

## ------------------------------------------------------------------------
exp(coef(mod_trees))
exp(confint(mod_trees))

## ------------------------------------------------------------------------
new_k <- log(pi) - 2*log(24)
exp(new_k)

## ------------------------------------------------------------------------
mod_trees_cylinder <- lm(log_Volume ~ 0 + offset(new_k + log_Height + 2 * log_Girth), data = trees)
anova(mod_trees_cylinder, mod_trees)

## ------------------------------------------------------------------------
girth.for.pred <- seq(min(trees$Girth), max(trees$Girth), length = 4)
height.for.pred <- seq(min(trees$Height), max(trees$Height), length = 10)
data.for.pred <- expand.grid(log_Girth = log(girth.for.pred), log_Height = log(height.for.pred))

pred1 <- predict(mod_trees, newdata = data.for.pred, interval = "confidence")
pred2 <- predict(mod_trees_cylinder, newdata = data.for.pred)  ## no confidence as nothing estimated!

data.for.plot <- as.data.frame(cbind(data.for.pred, pred1, fit2 = pred2))

plot(Volume ~ Height, data = trees, cex = 5*trees$Girth/max(trees$Girth), pch = 21, log = "xy",
     ylim = range(exp(data.for.plot$fit), exp(data.for.plot$fit2), trees$Volume),
     ylab = "Volume (cubic feet)", xlab = "Height (meters)", col = "brown", bg = "yellow", lwd = 3)

for (girth_class in 1:length(unique(girth.for.pred))) {
  with(subset(data.for.plot, data.for.plot$log_Girth == log(unique(girth.for.pred)[girth_class])), {
    points(exp(fit) ~ exp(log_Height), type = "l", lwd = girth_class)
    points(exp(fit2) ~ exp(log_Height), type = "l", lwd = girth_class, col = "red")
  })
}

## ------------------------------------------------------------------------
rm(list = ls())

## ------------------------------------------------------------------------
str(stackloss)
summary(stackloss)
pairs(stackloss)
any(is.na(stackloss))

## ------------------------------------------------------------------------
mod_stack <- lm(stack.loss ~ ., data = stackloss)

## ------------------------------------------------------------------------
for (col in 2:ncol(model.matrix(mod_stack))) {
  plot(rstandard(mod_stack) ~ model.matrix(mod_stack)[, col])
  title(main = colnames(model.matrix(mod_stack))[col])
}

## ------------------------------------------------------------------------
cor(model.matrix(mod_stack)[ , -1])
cov2cor(vcov(mod_stack))
vif(mod_stack)

## ------------------------------------------------------------------------
par(mfrow = c(2, 2))
plot(mod_stack)

## ------------------------------------------------------------------------
par(mfrow = c(2, 2))
plot(update(mod_stack, as.matrix(simulate(mod_stack)) ~ .))  ## simulation 1
plot(update(mod_stack, as.matrix(simulate(mod_stack)) ~ .))  ## simulation 2

## ------------------------------------------------------------------------
mod_stack_H0 <- update(mod_stack, . ~ 1)
anova(mod_stack_H0, mod_stack)

## ------------------------------------------------------------------------
summary(mod_stack)$coef["Acid.Conc.", ]

## ------------------------------------------------------------------------
plot(effects::effect("Acid.Conc.", mod = mod_stack))

## ------------------------------------------------------------------------
mod_stack_int <- lm(stack.loss ~ Acid.Conc. * (Air.Flow + Water.Temp), data = stackloss)
anova(mod_stack, mod_stack_int)
vif(mod_stack_int)

