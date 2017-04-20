## ----setup, include=FALSE------------------------------------------------
library(LM2GLMM)

## ----alien data----------------------------------------------------------
set.seed(123L)
Alien <- data.frame(humans_eaten = sample(1:12))
Alien$size <- rnorm(n = 12, mean = 50 + 1.5*Alien$humans_eaten, sd = sqrt(25))

## ----alien lm, echo = FALSE----------------------------------------------
mod_alien_lm <- lm(size ~ humans_eaten, data = Alien)

## ----alien data 2 eval, fig.align='center', fig.asp=1, fig.width=4, echo = FALSE, fig.cap="True relationship"----
plot(size ~ humans_eaten, data = Alien, ylab = "Alien size (cm)", xlab = "No. of humans eaten")
abline(a = 50, b = 1.5, col = "green", lwd = 2)

## ----alien data 3 eval, fig.align='center', fig.asp=1, fig.width=4, echo = FALSE, fig.cap="Inferred relationship"----
plot(size ~ humans_eaten, data = Alien, ylab = "Alien size (cm)", xlab = "No. of humans eaten")
text(x = nrow(Alien)/2, y = 82, labels = "?", cex = 5, col = "blue")
abline(mod_alien_lm, col = "blue", lwd = 2, lty = 2)

## ----fit alien lm--------------------------------------------------------
(mod_alien_lm <- lm(size ~ humans_eaten, data = Alien))
names(mod_alien_lm)

## ----fit coef------------------------------------------------------------
(coef_lm <- mod_alien_lm$coef) ## We expect something close to 50 and 1.5
summary(mod_alien_lm)$sigma^2 ## We expect something close to 25
logLik(mod_alien_lm)[[1]]
anova(mod_alien_lm)$"Sum Sq"[2]

## ----design matrix-------------------------------------------------------
X_alien <- model.matrix(mod_alien_lm)  ## Tip: same as model.matrix(~ humans_eaten, data = Alien)
head(X_alien)

## ----predict-------------------------------------------------------------
pred_auto   <- predict(mod_alien_lm)  ## Tip: same as mod_alien_lm$fitted.values / fitted(mod_alien_lm)
pred_auto2 <- predict(mod_alien_lm, newdata = Alien) ## can handle missing data!
pred_simple <- coef_lm[1] + coef_lm[2] * Alien$humans_eaten
pred_matrix <- X_alien %*% coef_lm

## ----show predict--------------------------------------------------------
head(cbind("auto" = pred_auto, "auto2" = pred_auto2, "simple" = pred_simple, "matrix" = c(pred_matrix)))
Alien$pred <- pred_auto ## we store the predicts in the dataframe

## ----residuals-----------------------------------------------------------
resid_auto   <- residuals(mod_alien_lm)  ## Tip: same as mod_alien_lm$residuals 
resid_simple <- Alien$size - (coef_lm[1] + coef_lm[2] * Alien$humans_eaten)
resid_matrix <- matrix(Alien$size) - X_alien %*% coef_lm

## ----show residuals------------------------------------------------------
head(cbind("auto" = resid_auto, "simple" = resid_simple, "matrix" = c(resid_matrix)))
Alien$resid <- resid_auto ## we store the residuals in the dataframe

## ----rss alien-----------------------------------------------------------
(rss_lm <- anova(mod_alien_lm)$"Sum Sq"[2])

sum(Alien$resid^2)

## ----alien RSS plot, fig.align='center', fig.asp=1, fig.width=5, echo = FALSE, fig.cap="Residuals on best fit"----
plot(size ~ humans_eaten, data = Alien, ylab = "Alien size (cm)", xlab = "No. of humans eaten", asp = 1)
points(pred ~ humans_eaten, col = "blue", data = Alien, pch = 20)
with(Alien, segments(x0 = humans_eaten, x1 = humans_eaten, y0 = size, y1 = pred, col = "orange"))
legend("topleft", bty = "n", col = c("black", "blue"), pch = c(1, 20), legend = c("obs", "pred"))

## ----alien RSS plot2, fig.align='center', fig.asp=1, fig.width=5, echo = FALSE, fig.cap="Squared residuals on best fit"----
plot(
  size ~ humans_eaten,
  data = Alien,
  ylab = "Alien size (cm)",
  xlab = "No. of humans eaten",
  asp = 1
  )
points(pred ~ humans_eaten, col = "blue", data = Alien, pch = 20)
for (i in 1:nrow(Alien)) {
  with(Alien, polygon(
    x = c(
    humans_eaten[i],
    humans_eaten[i],
    humans_eaten[i] + abs(resid[i]),
    humans_eaten[i] + abs(resid[i])
    ),
    y = c(pred[i], size[i], size[i], pred[i])
    ))
}
with(Alien, segments(x0 = humans_eaten, x1 = humans_eaten, y0 = size, y1 = pred, col = "orange"))
legend("topleft", bty = "n", col = c("black", "blue"), pch = c(1, 20), legend = c("obs", "pred"))

## ----alien RSS bad fit, fig.align='center', fig.asp=1, fig.width=5, echo = FALSE, fig.cap="Squared residuals on bad fit"----
plot(
  size ~ humans_eaten,
  data = Alien,
  ylab = "Alien size (cm)",
  xlab = "No. of humans eaten",
  asp = 1
  )
badslope <- 0.5
badintercept <- mean(Alien$size) - badslope * mean(Alien$humans_eaten)
badpred <- badintercept + badslope * Alien$humans_eaten
badresid <- Alien$size - badpred
points(badpred ~ humans_eaten, col = "blue", data = Alien, pch = 20)
for (i in 1:nrow(Alien)) {
  with(Alien, polygon(
    x = c(
    humans_eaten[i],
    humans_eaten[i],
    humans_eaten[i] + abs(badresid[i]),
    humans_eaten[i] + abs(badresid[i])
    ),
    y = c(badpred[i], size[i], size[i], badpred[i])
    ))
}
with(Alien, segments(x0 = humans_eaten, x1 = humans_eaten, y0 = size, y1 = badpred, col = "orange"))
legend("topleft", bty = "n", col = c("black", "blue"), pch = c(1, 20), legend = c("obs", "pred"))

## ----alien RSS plot2 again, fig.align='center', fig.asp=1, fig.width=5, echo = FALSE, fig.cap="Squared residuals on best fit"----
plot(
  size ~ humans_eaten,
  data = Alien,
  ylab = "Alien size (cm)",
  xlab = "No. of humans eaten",
  asp = 1
  )
points(pred ~ humans_eaten, col = "blue", data = Alien, pch = 20)
for (i in 1:nrow(Alien)) {
  with(Alien, polygon(
    x = c(
    humans_eaten[i],
    humans_eaten[i],
    humans_eaten[i] + abs(resid[i]),
    humans_eaten[i] + abs(resid[i])
    ),
    y = c(pred[i], size[i], size[i], pred[i])
    ))
}
with(Alien, segments(x0 = humans_eaten, x1 = humans_eaten, y0 = size, y1 = pred, col = "orange"))
legend("topleft", bty = "n", col = c("black", "blue"), pch = c(1, 20), legend = c("obs", "pred"))

## ----alien sigma2 error--------------------------------------------------
(sigma2_error <- summary(mod_alien_lm)$sigma^2)
sum(Alien$resid^2) / (nrow(Alien) - length(coef_lm))
var(Alien$resid) * (nrow(Alien) - 1) / (nrow(Alien) - length(coef_lm))

## ----alien sigma2 resid--------------------------------------------------
(sigma2_resid <- sum(Alien$resid^2) / nrow(Alien))
var(Alien$resid) * (nrow(Alien) - 1) / nrow(Alien) 
sigma2_error * (nrow(Alien) - length(coef_lm)) /  nrow(Alien)

## ----alien loglik--------------------------------------------------------
Alien$density <- dnorm(x = Alien$size, mean = Alien$pred, sd = sqrt(sigma2_resid))
Alien[1, ]

## ----alien lik plot, fig.align='center', fig.asp=1, fig.width=4, echo = FALSE----
par(mar = c(4, 4, 1, 1))
curve(dnorm(x, mean = Alien[1, "pred"], sd = sqrt(sigma2_resid)), from = 30, to = 30 + Alien[1, "pred"], ylab = "probability density", xlab = "size")
abline(h = 0, lty = 2)
abline(v = Alien[1, "pred"], lty = 2, col = "blue")
points(x = Alien[1, "pred"], y = 0, pch = 20, col = "blue")
points(x = Alien[1, "size"], y = 0)
segments(x0 = Alien[1, "pred"], x1 = Alien[1, "size"], y0 = 0, y1 = 0, col = "orange")
segments(x0 = Alien[1, "size"], x1 = Alien[1, "size"], y0 = 0, y1 = Alien[1, "density"], col = "purple")
arrows(x0 = Alien[1, "size"], x1 = 30, y0 = Alien[1, "density"], y1 = Alien[1, "density"], col = "purple", length = 0.1)

## ----alien log density---------------------------------------------------
logLik(mod_alien_lm)[[1]]
log(prod(Alien$density))
sum(log(Alien$density))

## ----deviance------------------------------------------------------------
deviance(mod_alien_lm)

(logLik_mod <- logLik(mod_alien_lm))
(logLik_sat <- sum(dnorm(Alien$size, mean = Alien$size, sd = sqrt(sigma2_resid), log = TRUE)))

(-2 * sigma2_resid * (logLik_mod - logLik_sat))  ## unscaled deviance

## ----def comput_logLik_Alien---------------------------------------------
compute_logLik_Alien <- function(param, data = Alien) {
  predicts <- param["intercept"] + param["slope"] * data$humans_eaten
  sigma2_resid <- abs(param["sigma2_resid"])  ## abs() as we don't want a negative var
  logL <- sum(dnorm(data$size, mean = predicts, sd = sqrt(sigma2_resid), log = TRUE))
  return(logL)
  }

(theta.lm <- c("intercept" = coef_lm[1][[1]], "slope" = coef_lm[2][[1]],
               "sigma2_resid" = sigma2_resid)) ## For testing
compute_logLik_Alien(param = theta.lm)

## ----def logLik Alien computation----------------------------------------
result_opt <- optim(c("intercept" = 0, "slope" = 1, "sigma2_resid" = 1), compute_logLik_Alien,
       control = list(fnscale = -1))
result_opt$par
result_opt$value

## ----def compute_rss_Alien-----------------------------------------------
compute_rss_Alien <- function(param, data = Alien) {
  predicts <- param["intercept"] + param["slope"] * data$humans_eaten
  rss <- sum((data$size - predicts)^2)
  return(rss)
}

all.equal(rss_lm, compute_rss_Alien(param = theta.lm))  ## For testing
optim(c("intercept" = 0, "slope" = 1), compute_rss_Alien)$par

## ----lin algebra---------------------------------------------------------
Y <- matrix(Alien$size)
X <- model.matrix(mod_alien_lm)
solve(t(X) %*% X) %*% t(X) %*% Y  ## Tip: solve(x) returns the inverse of the matrix x

## ----QR------------------------------------------------------------------
qr_list <- qr(X)  ## same as mod_alien_lm$qr
Q <- qr.Q(qr_list, complete = TRUE)  ## orthogonal matrix n * n (transpose = inverse)
R <- qr.R(qr_list, complete = TRUE)  ## upper triangular matrix n * p

## ----QR2, eval = FALSE---------------------------------------------------
#  all.equal(Q %*% R, X, check.attributes = FALSE)  ## TRUE: Q %*% R is equal to X!!

## ----QR 3----------------------------------------------------------------
QTY <- t(Q) %*% Y    ## same as mod_alien_lm$effects
backsolve(R, QTY)    ## RB = QTY 

## ----lm output-----------------------------------------------------------
names(mod_alien_lm)

## ----lm guts, eval = FALSE-----------------------------------------------
#  lm(size ~ humans_eaten, data = Alien)
#  mf <- model.frame(size ~ humans_eaten, data = Alien)
#  Y  <- model.response(mf)
#  X  <- model.matrix(~ humans_eaten, data = Alien)
#  lm.fit(X, Y)

## ----UK data-------------------------------------------------------------
head(UK)
mod_UK1 <- lm(height ~ drink + sex*weight, data = UK)

## ----UK model frame------------------------------------------------------
mf <- mod_UK1$model
str(mf)

## ----UK model frame 2----------------------------------------------------
head(mf)

## ----UK design matrix----------------------------------------------------
X <- model.matrix(mod_UK1)
head(X)

## ----UK coef-------------------------------------------------------------
data.frame(coef(mod_UK1))

## ----example predict-----------------------------------------------------
newX <- matrix(
  c(1, 1, 0, 0, 0, 30, 0,
    1, 1, 0, 0, 1, 30, 30,
    1, 0, 0, 0, 0, 35, 0),
  nrow = 3, byrow = TRUE)
colnames(newX) <- names(coef(mod_UK1))
newX

## ----example predict 2---------------------------------------------------
newX %*% coef(mod_UK1)

## ----example predict 3---------------------------------------------------
newdata1 <- data.frame(
  drink = c("Most days", "Most days", "2 to 3 times a week"),
  sex = c("Boy", "Girl", "Boy"),
  weight = c(30, 30, 35))
predict(mod_UK1, newdata = newdata1)

## ----stats---------------------------------------------------------------
mod_stats <- lm(size ~ humans_eaten, data = Alien)
coef(mod_stats)
summary(mod_stats)$sigma^2  ## estimate of error variance
summary(mod_stats)$sigma^2 * (nrow(Alien) - length(coef_lm)) /  nrow(Alien)
logLik(mod_stats)

## ----spaMM 1, message = FALSE--------------------------------------------
library(spaMM)
mod_spaMM_ML <- fitme(size ~ humans_eaten, data = Alien, method = "ML")  ## ML is the default
mod_spaMM_ML$fixef
mod_spaMM_ML$phi ## biased estimate of error variance (= here to residual variance)
mod_spaMM_ML$APHLs$p_v

## ----spaMM 2-------------------------------------------------------------
mod_spaMM_REML <- fitme(size ~ humans_eaten, data = Alien, method = "REML")
mod_spaMM_REML$fixef
mod_spaMM_REML$phi ## unbiased estimate of error variance
mod_spaMM_REML$APHLs$p_v

