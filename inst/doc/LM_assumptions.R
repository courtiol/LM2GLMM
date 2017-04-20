## ----setup, include=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(LM2GLMM)
library(lmtest)
knitr::opts_chunk$set(cache = FALSE, cache.path = "./cache_knitr/LM_assumptions/", fig.path = "./fig_knitr/LM_assumptions/", fig.align = "center", fig.width = 4, fig.height = 4)
options(width = 200)
set.seed(1L)

## ----simple non linear--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(1L)
Alien0 <- data.frame(humans_eaten = sample(1:100)) 
Alien0$size <- 10 + 50 * Alien0$humans_eaten - 0.02 * (Alien0$humans_eaten^2) + rnorm(100, sd = 5)
mod0a <- lm(size ~ humans_eaten, data = Alien0)
coef(mod0a)
plot(size ~ humans_eaten, data = Alien0)

## ----simple non linear2-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plot(rstandard(mod0a) ~ model.matrix(mod0a)[,2])

## ----simple non linear3-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
mod0b <- lm(size ~ poly(humans_eaten, 2, raw = TRUE), data = Alien0)
summary(mod0b)$coef

## ----poison-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
data(poison, package = "fastR")  ##  ?fastR::poison for description
poison$treat <- factor(poison$Treatment)
poison$poison <- factor(poison$Poison)
mod_poison <- lm(Time ~ poison + treat, data = poison)
plot(residuals(mod_poison) ~ fitted(mod_poison), xlab = "fitted values", ylab = "residuals")
abline(h = 0, col = "red", lty = 2)

## ----boxcox-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(MASS)
bc <- boxcox(mod_poison)  ## makes logLik profile!

## ----poison bc----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
mod_poison2 <- update(mod_poison, (Time^-1 - 1)/(-1) ~ .)
plot(residuals(mod_poison2) ~ fitted(mod_poison2), xlab = "fitted values", ylab = "residuals")
abline(h = 0, col = "red", lty = 2)

## ----poison pred--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
data.for.pred <- expand.grid(treat = levels(poison$treat), poison = levels(poison$poison))
(pred <- cbind(data.for.pred, predict(mod_poison2, newdata = data.for.pred, interval = "confidence")))

## ----pred unbc----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
lambda <- -1; (pred$fit * lambda + 1)^(1/lambda)

## ----poison summary-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(mod_poison) ## the original model

## ----poison bc summary--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(mod_poison2) ## the boxcoxed model

## ----degenerated n too small, error = TRUE------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(1L)
N <- 3
Alien <- data.frame(humans_eaten = 1:N,
                     flowers_eaten = round(runif(N, min = 1, max = 15)),
                     cactus_eaten =  round(runif(N, min = 1, max = 10)))

Alien$size <- rnorm(n = nrow(Alien),
  mean = 50 + 0.2 * Alien$humans_eaten + 0.9 * Alien$flowers_eaten + 0.1 * Alien$cactus_eaten,
  sd = sqrt(25))

mod_alien1a <- lm(size ~  cactus_eaten + humans_eaten + flowers_eaten, data = Alien)
coef(mod_alien1a)
mod_alien1b <- lm(size ~  humans_eaten + flowers_eaten + cactus_eaten, data = Alien)
coef(mod_alien1b)

## ----degenerated redundancy, error = TRUE-------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(1L)
Alien2 <- simulate_Aliens()
Alien2$half_humans_eaten <-  0.5 * Alien2$humans_eaten
mod_alien2 <- lm(size ~ humans_eaten + half_humans_eaten, data = Alien2)
coef(mod_alien2)
det(crossprod(model.matrix(mod_alien2)))  ## when det(XTX) <= 0, XTX has no inverse!
mod_alien2$rank  == ncol(model.matrix(mod_alien2))

## ----degenerated redundancy subtle, error = TRUE------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(1L)
Alien3 <- data.frame(humans_eaten = 1:12,
                     flowers_eaten = round(runif(12, min = 1, max = 15)),
                     cactus_eaten = 0)
Alien3$food_units <- 1.2*Alien3$humans_eaten + 0.6*Alien3$flowers_eaten
Alien3$size <- rnorm(n = 12, mean = 50 + 1*Alien3$food_units, sd = sqrt(25))
mod_alien3 <- lm(size ~ food_units + humans_eaten + flowers_eaten + cactus_eaten, data = Alien3)
coef(mod_alien3)
caret::findLinearCombos(model.matrix(mod_alien3))  ## Tip: help to see what creates the issue

## ----fungi--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
head(Fungi)

## ----USA----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(mod_US  <- lm(Rape ~ Assault + Murder, data = USArrests))$coef
summary(mod_US2 <- lm(Rape ~ Murder, data = USArrests))$coef

## ----USA 2--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
crossprod(model.matrix(mod_US))  ## remember: high values show collinearity
cor(USArrests$Assault, USArrests$Murder)
cov2cor(vcov(mod_US))

## ----pca----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
pca <- prcomp(~ Assault + Murder, data = USArrests, scale. = TRUE)
USArrests$PC1 <- pca$x[, 1]
summary(mod_US3 <- lm(Rape ~ PC1, data = USArrests))

## ----measurement error--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(1L)
Alien4 <- simulate_Aliens(100)
summary(lm(size ~ humans_eaten, data = Alien4))$coef
Alien4$humans_eaten_err <- Alien4$humans_eaten + rnorm(nrow(Alien4), sd = 10)
summary(lm(size ~ humans_eaten_err, data = Alien4))$coef

## ----sem----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(sem)
eqns <- specifyEquations(text = "
                        size = alpha*Intercept + slope*humans_eaten
                        humans_eaten = 1*humans_eaten_err
                        V(size) = sigma
                        V(humans_eaten) = 1
                        V(humans_eaten_err) = phi
                        ")
fitted.mod <- sem(eqns, data = Alien4, raw = TRUE, fixed.x = "Intercept")
summary(fitted.mod, analytic.se = FALSE)$coef  ## use analytic.se = FALSE (uses z as consider variance known)

## ----DW-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(car)
set.seed(1L)
durbinWatsonTest(modConv <- lm(fconvict ~ tfr + partic + degrees + mconvict, data = Hartnagel), max.lag = 3)

## ----DW 2, message = FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(lmtest)
dwtest(modConv, order.by = modConv$model$degrees)

## ----DW by eye----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plot(residuals(modConv) ~ fitted(modConv))
abline(h = 0, lty = 2, col = "red")

## ----DW by eye 2--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plot(residuals(modConv) ~ Hartnagel$year, type = "o")
abline(h = 0, lty = 2, col = "red")

## ----rpois--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(1L)
Alien5 <- simulate_Aliens(N = 100)
Alien5$eggs <- rpois(100, lambda = 2 + 1 * Alien5$humans_eaten)
mod_alien5 <- lm(eggs ~ humans_eaten, data = Alien5)
summary(mod_alien5)$coef
bptest(mod_alien5)

## ----plot heterosced----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plot(abs(rstandard(mod_alien5)) ~ fitted(mod_alien5))

## ----rpois correction---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
vcov(mod_alien5)
hccm(mod_alien5)  ## correct the covariance matrix of parameter estimates
estimates <- coef(mod_alien5)
std.errors <- sqrt(diag(hccm(mod_alien5)))
t.values <- estimates/std.errors
p.values <- 2*pt(abs(t.values), df = mod_alien5$df.residual, lower.tail = FALSE)
cbind(estimates, std.errors, t.values, p.values)

## ----norm test----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(nortest)
lillie.test(mod_poison$residuals)   ## stat = 0 when normal
shapiro.test(mod_poison$residuals)  ## stat = 1 when normal

## ----norm by eye--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
qqnorm(mod_poison$residuals)
qqline(mod_poison$residuals, col = 2, lty = 2)

## ----norm by eye 2------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
qqnorm(mod_poison2$residuals)
qqline(mod_poison2$residuals, col = 2, lty = 2)

## ----plot mod, fig.height = 5, fig.width = 5----------------------------------------------------------------------------------------------------------------------------------------------------------
par(mfrow = c(2, 2))
plot(mod_poison)

## ----Davis--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
data(Davis)
mod_davis <- lm(weight ~ height, data = Davis)
plot(weight ~ height, data = Davis)
abline(mod_davis, col = "red", lwd = 2)

## ----Davis 2------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# with(Davis, identify(height, weight, 
#   row.names(Davis))) # click and escape
mod_davis2 <- update(mod_davis, data = Davis[-12, ])
plot(weight ~ height, data = Davis[-12, ])
abline(mod_davis2, col = "blue", lwd = 2)

## ----hat----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
head(sort(hatvalues(mod_davis), decreasing = TRUE))
X <- model.matrix(mod_davis)
head(sort(diag(X %*% solve(crossprod(X)) %*% t(X)), decreasing = TRUE))

## ----hat plot-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plot(mod_davis, which = 5)

## ----influence----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
head(sort(dffits(mod_davis), decreasing = TRUE))
head(sort(cooks.distance(mod_davis), decreasing = TRUE))

## ----influence2---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plot(dffits(mod_davis2), cooks.distance(mod_davis2), xlab = "DFFITS", ylab = "Cook's distance")

## ----influence3, fig.width = 10-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
par(mfrow = c(1, 3))
plot(mod_davis, which = 4:6)

## ----influence4, fig.width = 10-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
mod_UK <- lm(height ~ sex * milk, data = UK[1:20, ])
par(mfrow = c(1, 3))
plot(mod_UK, which = 4:6)

## ----influence5, fig.width = 10-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
mod_UK2 <- lm(height ~ sex * milk, data = UK)
par(mfrow = c(1, 3))
plot(mod_UK2, which = 4:6)

## ----dfbetas------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
head(dfbeta(mod_davis), n = 3)
coef(mod_davis) - coef(update(mod_davis, data = Davis[-1, ]))
head(dfbetas(mod_davis), n = 3) ## same in SE units

## ----cov----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
head(sort(covratio(mod_davis), decreasing = TRUE))

## ----influence.measures-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
influence.measures(mod_davis)  ## stars are just there to attract your attention, there is no proper tests!

