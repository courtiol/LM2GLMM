## ----setup, include=FALSE---------------------------------------------------------------------------------------------
library(LM2GLMM)
library(spaMM)
library(lme4)
library(car)
spaMM.options(nb_cores = 4L)
options(width = 120)
knitr::opts_chunk$set(cache = TRUE, cache.path = "./cache_knitr/LMM_showcase/", fig.path = "./fig_knitr/LMM_showcase/", fig.width = 5, fig.height = 5, fig.align = "center")

## ---------------------------------------------------------------------------------------------------------------------
tail(Gryphon$pedigree)
library(nadiv)
A <- as(makeA(Gryphon$pedigree), "matrix")
colnames(A) <- rownames(A) <- Gryphon$pedigree$ID
A[1305:1309, 1296:1309]

## ----animal model-----------------------------------------------------------------------------------------------------
library(spaMM)
system.time(mod1 <- fitme(BWT ~ 1 + corrMatrix(1|ID), corrMatrix = A, data = Gryphon$data, method = "REML"))
(h2 <- as.numeric(mod1$lambda / (mod1$lambda + mod1$phi)))

## ----animal model 2---------------------------------------------------------------------------------------------------
Gryphon$data$sex    <- factor(Gryphon$data$SEX)
Gryphon$data$year   <- factor(Gryphon$data$BYEAR)
Gryphon$data$mother <- factor(Gryphon$data$MOTHER)

system.time(
  mod2 <- fitme(BWT ~ sex + (1|year) + (1|mother) + corrMatrix(1|ID), corrMatrix = A,
                data = Gryphon$data, method = "REML")
)

h2 <- as.numeric(mod2$lambda["ID"]     / (sum(mod2$lambda) + mod2$phi))
m2 <- as.numeric(mod2$lambda["mother"] / (sum(mod2$lambda) + mod2$phi))

round(rbind("heritability" = h2, "maternal effect size" = m2), 3)

## ---- fig.height=3.5, fig.width=4-------------------------------------------------------------------------------------
curve(dnorm(x, sd = sqrt(as.numeric(mod2$lambda["ID"]))), from = -3, to = 3, las = 1,
      ylab = "pdf", xlab = "predicted breeding value")
BLUPs <- ranef(mod2)$"corrMatrix(1 | ID)"
points(dnorm(BLUPs, sd = sqrt(as.numeric(mod2$lambda["ID"]))) ~ BLUPs, col = "blue", type = "h", lwd = 0.1)

## ---------------------------------------------------------------------------------------------------------------------
plot(BLUPs ~ scale(mod2$data$BWT), ylab = "Predicted breeding values", xlab = "Scaled phenotypic values")

## ---------------------------------------------------------------------------------------------------------------------
library(ade4)
data(carni70)
carni70$tab

## ---------------------------------------------------------------------------------------------------------------------
library(ape)
tree <- read.tree(text = carni70$tre)
plot(tree, cex = 0.3)

## ---------------------------------------------------------------------------------------------------------------------
(corrM <- vcv(tree, model = "Brownian", corr = TRUE))

## ---------------------------------------------------------------------------------------------------------------------
carni70$tab$sp <- factor(rownames(corrM))
(mod_carni <- fitme(range ~ size + corrMatrix(1|sp), corrMatrix = corrM, data = carni70$tab))

## ---------------------------------------------------------------------------------------------------------------------
mod_carni_no_size <- fitme(range ~ 1 + corrMatrix(1|sp), corrMatrix = corrM, data = carni70$tab)
anova(mod_carni, mod_carni_no_size)

## ---- message = FALSE-------------------------------------------------------------------------------------------------
library(nlme)
rownames(carni70$tab) <- rownames(corrM)
(mod_carni2 <- gls(range ~ size, correlation = corBrownian(1, phy = tree), method = "ML", data = carni70$tab))

## ---------------------------------------------------------------------------------------------------------------------
mod_carni2_no_size <- gls(range ~ 1, correlation = corBrownian(1, tree), method = "ML", data = carni70$tab)
anova(mod_carni2, mod_carni2_no_size)

## ---- message = FALSE-------------------------------------------------------------------------------------------------
library(metafor)
dat.bcg

## ---------------------------------------------------------------------------------------------------------------------
dat.bcg$RR  <- with(dat.bcg, log((tpos/(tpos + tneg)) / (cpos/(cpos + cneg))))
dat.bcg$sampling.var <- with(dat.bcg, 1/tpos - 1/tneg + 1/cpos - 1/cneg)
dat.bcg

## ---------------------------------------------------------------------------------------------------------------------
(mod <- rma(yi = RR ~ factor(alloc) + year + ablat, vi = sampling.var, data = dat.bcg, method = "REML"))

## ---------------------------------------------------------------------------------------------------------------------
dat.bcg$study <- factor(dat.bcg$trial)
(mod_spaMM <- fitme(RR ~ factor(alloc) + year + ablat + (1|study), data = dat.bcg,
                    fixed = list(phi = dat.bcg$sampling.var), method = "REML"))

## ---------------------------------------------------------------------------------------------------------------------
round(c(tau2_metafor = mod$tau2, lambda_spaMM = mod_spaMM$lambda[[1]]), 4)  ## heterogeneity between studies

mod_spaMMRE <- fitme(RR ~ 1 + (1|study), data = dat.bcg,
                     fixed = list(phi = dat.bcg$sampling.var), method = "REML")

R2_spaMM <- 100 * (mod_spaMMRE$lambda[[1]] - mod_spaMM$lambda[[1]]) / mod_spaMMRE$lambda[[1]]
round(c(R2_metafor = mod$R2, R2_spaMM = R2_spaMM), 4)  ## amount of heterogeneity accounted for by fixed effects

## ---------------------------------------------------------------------------------------------------------------------
c(mod$I2, mod$H2, mod$QE) ## from metafor, see ?print.rma.uni for details

get_meta_metrics <- function(model, vi) {
  wi <- 1/vi  ## weights = inverse of sampling variance
  k <- length(model$ranef)  ## number of groups in random term (here number of studies)
  p <- length(model$fixef)  ## number of fixed effect parameters
  W <- diag(wi, nrow = k, ncol = k)  ## matrix of weights
  X <- as.matrix(as.data.frame(model$X.pv))  ## design matrix
  stXWX <- solve(t(X) %*% W %*% X)  ## weighted vcov of estimates
  P <- W - W %*% X %*% stXWX %*% crossprod(X, W)  ## weighted vcov of ??
  vi.avg <- (k - p) / sum(diag(P))
  I2 <- as.numeric(100 * model$lambda / (vi.avg + model$lambda) )
  H2 <- as.numeric((vi.avg + model$lambda) / vi.avg)
  QE <- max(0, c(crossprod(model$y, P) %*% model$y))
  return(c(I2 = I2, H2 = H2, QE = QE, vi.avg = vi.avg))
}

get_meta_metrics(mod_spaMM, dat.bcg$sampling.var) ## add vi.avg: weighted mean within study sampling variance

## ---- fig.width = 6, fig.height = 5.5---------------------------------------------------------------------------------
metafor::plot.rma.uni(mod)  ## or just plot(mod)

## ---------------------------------------------------------------------------------------------------------------------
mod_spaMM2    <- fitme(RR ~ factor(alloc) + year + ablat + (1|study), data = dat.bcg,
                       fixed = list(phi = dat.bcg$sampling.var))
mod_spaMM2_H0 <- fitme(RR ~ 1 + (1|study), data = dat.bcg, fixed = list(phi = dat.bcg$sampling.var))
anova(mod_spaMM2, mod_spaMM2_H0)

## ----test for metafor-------------------------------------------------------------------------------------------------
res.boot <- anova(mod_spaMM2, mod_spaMM2_H0, boot.repl = 1000)

## ---------------------------------------------------------------------------------------------------------------------
res.boot

## ---------------------------------------------------------------------------------------------------------------------
(mod2 <- rma(yi = RR ~ factor(alloc) + year + ablat, vi = sampling.var, data = dat.bcg, method = "ML"))

## ---------------------------------------------------------------------------------------------------------------------
(mod3 <- rma(yi = RR ~ factor(alloc) + year + ablat, vi = sampling.var, data = dat.bcg, method = "ML",
             test = "knha"))

## ---------------------------------------------------------------------------------------------------------------------
mod_spaMM3    <- fitme(RR ~ 1 + (1|study), data = dat.bcg, fixed = list(phi = dat.bcg$sampling.var))
mod_spaMM3_H0 <- fitme(RR ~ 0 + (1|study), data = dat.bcg, fixed = list(phi = dat.bcg$sampling.var))
anova(mod_spaMM3, mod_spaMM3_H0)

## ----test for metafor 2-----------------------------------------------------------------------------------------------
res.boot2 <- anova(mod_spaMM3, mod_spaMM3_H0, boot.repl = 1000)

## ---------------------------------------------------------------------------------------------------------------------
res.boot2

## ---------------------------------------------------------------------------------------------------------------------
mod_spaMM3$fixef
get_intervals(mod_spaMM3, re.form = NA, intervals = "predVar")[1, ]

## ---------------------------------------------------------------------------------------------------------------------
(mod4 <- rma(yi = RR ~ 1, vi = sampling.var, data = dat.bcg, method = "ML"))

## ---------------------------------------------------------------------------------------------------------------------
forest(mod4)

