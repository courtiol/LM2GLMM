## ----setup, include=FALSE---------------------------------------------------------------------------------------------
library(LM2GLMM)
options(width = 120)

## ----pred matrix------------------------------------------------------------------------------------------------------
somedata <- data.frame(x1 = 1:6)
(mm <- model.matrix(object = ~ x1, data = somedata))

## ----plot mm, fig.align = "center", fig.width = 5, fig.height = 5-----------------------------------------------------
matplot(mm)

## ----pred matrix 0----------------------------------------------------------------------------------------------------
model.matrix(object = ~ 0 + x1, data = somedata)
model.matrix(object = ~ -1 + x1, data = somedata)

## ----pred matrix 1----------------------------------------------------------------------------------------------------
model.matrix(object = ~ log(x1) + poly(x1, 2, raw = TRUE), data = somedata)

## ----pred matrix 1b---------------------------------------------------------------------------------------------------
model.matrix(object = ~ log(x1) + poly(x1, 2), data = somedata)

## ----pred matrix 2----------------------------------------------------------------------------------------------------
somedata$x2 <- c(10, 11, 12, 13, 14, 15)
somedata$x3 <- factor(c("b", "b", "a", "a", "c", "c"))
model.matrix(object = ~ x1 + x2 + x3, data = somedata)

## ----pred matrix 3----------------------------------------------------------------------------------------------------
model.matrix(object = ~ x1 + x2 + x3, data = somedata, contrasts.arg = list(x3 = "contr.helmert"))

## ----pred matrix 4----------------------------------------------------------------------------------------------------
model.matrix(object = ~ x1 * x2 + x1 * x3, data = somedata)

## ----convert----------------------------------------------------------------------------------------------------------
conv_betaXA_to_betaXB <- function(XA, XB, betaXA) {
  ## Test that inputs are OK:
  if (!any(dim(XA) == dim(XB)))   stop("design matrices differ in size")
  if (ncol(XA) != length(betaXA)) stop("betaXA of wrong length")
  ## Identify parameters that need to be converted
  id_col_keep <- which(apply(XA != XB, 2, sum) != 0) ## index columns to keep
  if (all(XA[, 1] == 1)) id_col_keep <- c(1, id_col_keep) ## add intercept
  id_col_drop <- setdiff(1:ncol(XA), id_col_keep) ## index columns to discard
  ## Conversion per se:
  betaXB_temp <- solve(coef(lm.fit(XA[, id_col_keep], XB[, id_col_keep])), betaXA[id_col_keep])
  ## Put back non converted parameters into output
  betaXB <- numeric(length(betaXA))
  betaXB[id_col_keep] <- betaXB_temp
  betaXB[id_col_drop] <- betaXA[id_col_drop]
  ## Output
  return(betaXB)
}
## homemade: this function may break in some legitimate cases...

## ----pred matrix poly a-----------------------------------------------------------------------------------------------
(DM_raw <- model.matrix(object = ~ poly(x1, 2, raw = TRUE), data = somedata))
DM_raw %*% c(1, 2, -2)  ## predictions using matrix multiplication with c(1, 2, -2) the model parameters

## ----convert poly-----------------------------------------------------------------------------------------------------
(DM_ortho <- model.matrix(object = ~ poly(x1, 2), data = somedata))
conv_betaXA_to_betaXB(XA = DM_raw, XB = DM_ortho, betaXA = c(1, 2, -2))

## ----pred matrix poly b-----------------------------------------------------------------------------------------------
DM_raw %*% c(1, 2, -2)  ## predictions
round(DM_ortho %*% c(-22.33333, -50.19960, -12.22020), 3)

## ----XTX_explained----------------------------------------------------------------------------------------------------
X <- matrix(c(1, 1, 1, 1, 2, 3, 4, 5, 6), byrow = FALSE, ncol = 3)
(XTX_1 <- t(X) %*% X)
XTX_2 <- crossprod(X)

XTX_3 <- matrix(0, nrow = 3, ncol = 3)

for (i in 1:3) for (j in 1:3) XTX_3[i, j] <- sum(X[, i]*X[, j])

identical(XTX_1, XTX_2) & identical(XTX_1, XTX_3)

## ----XTX--------------------------------------------------------------------------------------------------------------
crossprod(DM_raw)  ## high values mean that predictors are colinear -> will be hard to fit
zapsmall(crossprod(DM_ortho))  ## 0 means that predictors are orthogonal -> will be easy to fit

## ----helmert conv-----------------------------------------------------------------------------------------------------
DM_treat <- model.matrix(object = ~ x1 + x2 + x3, data = somedata)
DM_Helm <- model.matrix(object = ~ x1 + x2 + x3, data = somedata, contrasts.arg = list(x3 = "contr.helmert"))
beta_treat <- matrix(c(50, 1.5, 20, 2, 3))
(beta_Helm <- conv_betaXA_to_betaXB(XA = DM_treat, XB = DM_Helm, betaXA = beta_treat))

## ----param MA---------------------------------------------------------------------------------------------------------
DM_treat %*% beta_treat ## predictions

## ----param MB---------------------------------------------------------------------------------------------------------
DM_Helm %*% beta_Helm ## predictions

## ----factorial--------------------------------------------------------------------------------------------------------
data(poison, package = "fastR")  ##  ?fastR::poison for description
poison$treat <- factor(poison$Treatment)
poison$poison <- factor(poison$Poison)
X <- model.matrix( ~ poison + treat, data = poison)
head(X)

## ----factorial 2------------------------------------------------------------------------------------------------------
crossprod(X)  ## compute t(X) %*% X, i.e. x_ij = sum(X[, i]*X[, j])
with(poison, table(treat, poison))

## ----alien data-------------------------------------------------------------------------------------------------------
set.seed(123L)
Alien <- data.frame(humans_eaten = sample(1:6))
Alien$error <- rnorm(n = 6, mean = 0, sd = sqrt(25))
Alien$size <- 50 + 1.5*Alien$humans_eaten + Alien$error

## ----alien data2, eval = FALSE----------------------------------------------------------------------------------------
#  Alien$size <- rnorm(n = 6, mean = 50 + 1.5*Alien$humans_eaten, sd = sqrt(25))

## ----alien data show--------------------------------------------------------------------------------------------------
Alien

