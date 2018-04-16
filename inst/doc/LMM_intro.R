## ----setup, include=FALSE-------------------------------------------------------------------------
library(LM2GLMM)
library(spaMM)
library(lme4)
library(car)
options(width = 100)
knitr::opts_chunk$set(cache = TRUE, cache.path = "./cache_knitr/LMM_intro/", fig.path = "./fig_knitr/LMM_intro/", fig.width = 5, fig.height = 5, fig.align = "center")

## -------------------------------------------------------------------------------------------------
SimulateMix <- function(intercept, slope, n, group_nb, var.rand, var.error){
  data <- data.frame(intercept = intercept, slope = slope, x = runif(n)) 
  group_compo <- rmultinom(n = 1, size = n, prob = c(rep(1/group_nb, group_nb)))
  data$group <- factor(rep(paste("group", 1:group_nb, sep = "_"), group_compo))
  data$b <- rep(rnorm(group_nb, mean = 0, sd = sqrt(var.rand)), group_compo)
  data$error <- rnorm(n, mean = 0, sd = sqrt(var.error))
  data$y <- data$intercept + data$slope*data$x + data$b + data$error
  return(data)
}

set.seed(1)
Aliens <- SimulateMix(intercept = 50, slope = 1.5, n = 30, group_nb = 10, var.rand = 2, var.error = 0.5)

## -------------------------------------------------------------------------------------------------
Aliens

## -------------------------------------------------------------------------------------------------
library(lme4)
(mod_lme4 <- lmer(y ~ x + (1|group), data = Aliens, REML = FALSE))
c(var.group = as.numeric(attr(VarCorr(mod_lme4)$group, "stddev")^2),
  var.error = as.numeric(attr(VarCorr(mod_lme4), "sc")^2))  ## note: those are biased estimates (ML not REML)

## -------------------------------------------------------------------------------------------------
library(spaMM)
(mod_spaMM <- fitme(y ~ x + (1|group), data = Aliens, method = "ML"))

## -------------------------------------------------------------------------------------------------
lik_b <- function(b.vec, level, intercept, slope, var.rand, var.error, data, scale = 1){
  lik <- sapply(b.vec, function(b){
    sub_data <- data[which(data$group == level), ]
    sub_data$pred <- intercept + slope*sub_data$x + b
    sub_data$conditional.density <- dnorm(sub_data$y, mean = sub_data$pred, sd = sqrt(var.error))
    return(dnorm(b, mean = 0, sd = sqrt(var.rand)) * prod(sub_data$conditional.density))
  })
  return(scale * lik)
}

## -------------------------------------------------------------------------------------------------
log_lik_b_prod <- function(param, data, scale = 1){
  log_lik_vec <- sapply(levels(data$group), function(level) {
    log(integrate(lik_b, -Inf, Inf, level = level, intercept = param[1], slope = param[2],
                  var.rand = param[3], var.error = param[4], data = data)$value)})
  return(scale * sum(log_lik_vec))
}

## -------------------------------------------------------------------------------------------------
log_lik_b_prod(param = c(50, 1.5, 2, 0.5), data = Aliens) ## test functions above
mod_constr <- fitme(y ~ 0 + (1|group) + offset(50 + 1.5*x), data = Aliens, fixed = list(lambda = 2, phi = 0.5))
logLik(mod_constr)

## -------------------------------------------------------------------------------------------------
bad_mod <- lm(y ~ x + group, data = Aliens)

## -------------------------------------------------------------------------------------------------
(init_values <- c(bad_mod$coefficients[1], bad_mod$coefficients[2],
                  var.group = var(bad_mod$coefficients[-c(1:2)]),
                  var.error = deviance(bad_mod) / bad_mod$df.residual))

## ----numerical fit--------------------------------------------------------------------------------
system.time(
  opt <-  nloptr::nloptr(x0 = init_values, eval_f = log_lik_b_prod, data = Aliens, scale = -1,
                         lb = 0.5*init_values, ub = 2*init_values,
                         opts = list(algorithm = "NLOPT_LN_BOBYQA", xtol_rel = 1.0e-4, maxeval = -1))
)

## -------------------------------------------------------------------------------------------------
estimates <- rbind(opt$solution, as.numeric(c(mod_spaMM$fixef, mod_spaMM$lambda, mod_spaMM$phi)))
colnames(estimates) <- c("intercept", "slope", "var.group", "var.error")
estimates

c(logLik.num = -1 * opt$objective, logLik.spaMM = logLik(mod_spaMM)[[1]])

## ----fit b1---------------------------------------------------------------------------------------
my.ranef <- sapply(levels(Aliens$group), function(group) {
  nloptr::nloptr(x0 = 0, lik_b, level = group, intercept = opt$solution[1],
                 slope = opt$solution[2], var.rand = opt$solution[3],
                 var.error = opt$solution[4], data = Aliens, scale = -1,
                 lb = -4*sqrt(opt$solution[3]), ub = 4*sqrt(opt$solution[3]),
                 opts = list(algorithm = "NLOPT_LN_BOBYQA", xtol_rel = 1.0e-4, maxeval = -1))$solution})

rbind(my.ranef,
      ranef_lme4 = as.numeric(t(lme4::ranef(mod_lme4)[[1]])),
      ranef_spaMM = as.numeric(unlist(ranef(mod_spaMM))))

## ---- fig.height = 3.5, fig.width = 4-------------------------------------------------------------
curve(dnorm(x, mean = 0, sd = sqrt(estimates[1, "var.group"])), -5, 5, ylab = "density", xlab = "b")
points(dnorm(my.ranef, mean = 0, sd = sqrt(estimates[1, "var.group"])) ~ my.ranef, col = "blue", type = "h")
points(dnorm(my.ranef, mean = 0, sd = sqrt(estimates[1, "var.group"])) ~ my.ranef, col = "blue")

## -------------------------------------------------------------------------------------------------
data("Oats", package = "nlme")
Oats2 <- as.data.frame(Oats)
Oats2$Block <- factor(Oats2$Block, ordered = FALSE, levels = sort(levels(Oats2$Block)))
head(Oats2)
str(Oats2)

## -------------------------------------------------------------------------------------------------
coplot(yield ~ nitro | Variety + Block, data = Oats2, type = "b")

## -------------------------------------------------------------------------------------------------
mod_lm <- lm(yield ~ nitro + Variety + Block, data = Oats2)
data.for.pred <- expand.grid(nitro = 0.3, Variety = "Victory", Block = levels(Oats2$Block))
(p <- predict(mod_lm, newdata = data.for.pred, interval = "confidence", se.fit = TRUE))

## ---- fig.width = 4, fig.height = 4---------------------------------------------------------------
plot(Oats2$yield ~ unclass(Oats2$Block), axes = FALSE, ylab = "Yield", xlab = "Block",
     xlim = c(0.5, length(levels(Oats2$Block)) + 0.5), col = "blue", cex = 0.3)
points(p$fit[, "fit"] ~ I(1:length(levels(Oats2$Block)))) 
arrows(x0 = 1:length(levels(Oats2$Block)), y0 = p$fit[, "lwr"], y1 = p$fit[, "upr"],
       code = 3, angle = 90, length = 0.05)
axis(1, at = 1:length(levels(Oats2$Block)), labels = levels(Oats2$Block)); axis(2, las = 1); box()

## -------------------------------------------------------------------------------------------------
(mod_lmm_lme4 <- lmer(yield ~ nitro + Variety + (1|Block), data = Oats2, REML = FALSE))

## -------------------------------------------------------------------------------------------------
head(mod_lmm_lme4@pp$X)  ## X
mod_lmm_lme4@beta  ## beta estimates
c(var.group = as.numeric(attr(VarCorr(mod_lmm_lme4)$Block, "stddev")^2),
  var.error = as.numeric(attr(VarCorr(mod_lmm_lme4), "sc")^2))  ## lambda and phi estimates

## -------------------------------------------------------------------------------------------------
head(t(mod_lmm_lme4@pp$Zt))  ## Z
as.matrix(mod_lmm_lme4@pp$Zt)

## -------------------------------------------------------------------------------------------------
head(t(mod_lmm_lme4@pp$Zt))  ## Z
crossprod(t(as.matrix(mod_lmm_lme4@pp$Zt)))

## -------------------------------------------------------------------------------------------------
lme4::ranef(mod_lmm_lme4)  ## b estimates

## -------------------------------------------------------------------------------------------------
(mod_lmm_spaMM <- fitme(yield ~ nitro + Variety + (1|Block), data = Oats2))

## -------------------------------------------------------------------------------------------------
head(mod_lmm_spaMM$X.pv)  ## X
mod_lmm_spaMM$fixef  ## beta estimates
as.numeric(c(mod_lmm_spaMM$lambda, mod_lmm_spaMM$phi))  ## lambda and phi estimates

## -------------------------------------------------------------------------------------------------
head(mod_lmm_spaMM$ZAlist[[1]])  ## Z
crossprod(mod_lmm_spaMM$ZAlist[[1]])

## -------------------------------------------------------------------------------------------------
ranef(mod_lmm_spaMM)  ## b estimates

## -------------------------------------------------------------------------------------------------
data.for.pred <- expand.grid(nitro = 0.3, Variety = "Victory", Block = "new")
p1 <- predict(mod_lmm_lme4, newdata = data.for.pred, re.form = NA)
X <- matrix(c(1, 0.3, 0, 1), nrow = 1)
se.fit <- sqrt(X %*% vcov(mod_lmm_lme4) %*% t(X))
se.rand <- attr(VarCorr(mod_lmm_lme4)$Block, "stddev")
(se.predVar <- as.numeric(sqrt(se.fit^2 + se.rand^2)))
lwr <- as.numeric(p1 + qnorm(0.025) * se.predVar)
upr <- as.numeric(p1 + qnorm(0.975) * se.predVar)
c(fit = as.numeric(p1), lwr = lwr, upr = upr)  ## Wald CI for prediction

## -------------------------------------------------------------------------------------------------
data.for.pred <- expand.grid(nitro = 0.3, Variety = "Victory", Block = "new")
p2 <- predict(mod_lmm_spaMM, newdata = data.for.pred, intervals = "predVar")
sqrt(attr(p2, "predVar"))  ## se.predVar
c(fit = p2, attr(p2, "intervals"))

## -------------------------------------------------------------------------------------------------
data.for.pred <- expand.grid(nitro = 0.3, Variety = "Victory", Block = levels(Oats2$Block))
p3 <- predict(mod_lmm_spaMM, newdata = data.for.pred, intervals = "predVar")
sqrt(attr(p3, "predVar"))  ## se.predVar
cbind(p3, attr(p3, "intervals"))

## ---- fig.width = 4, fig.height = 4---------------------------------------------------------------
plot(Oats2$yield ~ unclass(Oats2$Block), axes = FALSE, ylab = "Yield", xlab = "Block",
     xlim = c(0.5, length(levels(Oats2$Block)) + 0.5), col = "blue", cex = 0.3)
points(p$fit[, "fit"] ~ I(1:length(levels(Oats2$Block))-0.1))
arrows(x0 = (1:length(levels(Oats2$Block))) - 0.1, y0 = p$fit[, "lwr"],
       y1 = p$fit[, "upr"], code = 3, angle = 90, length = 0.05)
points(p3 ~ I(1:length(levels(Oats2$Block)) + 0.1), col = "red")
arrows(x0 = (1:length(levels(Oats2$Block))) + 0.1, y0 = attr(p3, "intervals")[, 1],
       y1 = attr(p3, "intervals")[, 2], code = 3, angle = 90, length = 0.05, col = "red")
axis(1, at = 1:length(levels(Oats2$Block)), labels = levels(Oats2$Block)); axis(2, las = 1); box()

## -------------------------------------------------------------------------------------------------
mod_lm_nonitro <- lm(yield ~ Variety + Block, data = Oats2)
anova(mod_lm, mod_lm_nonitro)

## -------------------------------------------------------------------------------------------------
mod_lmm_lme4_nonitro <-  lmer(yield ~ Variety + (1|Block), data = Oats2, REML = FALSE)
anova(mod_lmm_lme4, mod_lmm_lme4_nonitro)

## ----simu lme4, fig.height = 4, fig.width = 4, warning = FALSE------------------------------------
Oats3 <- Oats2
pvalues <- replicate(1000, {
  Oats3$yield <- simulate(mod_lmm_lme4_nonitro)[, 1]
  mod_lmm_lme4_new <- lmer(yield ~ nitro + Variety + (1|Block), data = Oats3, REML = FALSE)
  mod_lmm_lme4_new_nonitro <-  lmer(yield ~ Variety + (1|Block), data = Oats3, REML = FALSE)
  anova(mod_lmm_lme4_new, mod_lmm_lme4_new_nonitro)$"Pr(>Chisq)"[2]})
plot(ecdf(pvalues)); abline(0, 1, col = 2)

## ----yield lme4 param boot------------------------------------------------------------------------
library(pbkrtest)
PBmodcomp(mod_lmm_lme4, mod_lmm_lme4_nonitro, nsim = 500)

## -------------------------------------------------------------------------------------------------
mod_lmm_spaMM_nonitro <-  fitme(yield ~ Variety + (1|Block), data = Oats2)
anova(mod_lmm_spaMM, mod_lmm_spaMM_nonitro)

## ----yield spaMM param boot, message = FALSE------------------------------------------------------
anova(mod_lmm_spaMM, mod_lmm_spaMM_nonitro, boot.repl = 500, nb_cores = 4) ## for 4 CPUs

## -------------------------------------------------------------------------------------------------
mod_aov <- aov(yield ~ nitro + Variety + Error(Block), data = Oats2)
coef(mod_aov)

## -------------------------------------------------------------------------------------------------
summary(mod_aov)

## ----simu aov, fig.height = 4, fig.width = 4------------------------------------------------------
Oats3 <- Oats2
pvalues <- replicate(1000, {
  Oats3$nitro <- runif(1:nrow(Oats3))  ## simulate H0 as there is no simulate method
  mod_aov_sim <- aov(yield ~ nitro + Variety + Error(Block), data = Oats3)
  summary(mod_aov_sim)[[2]][[1]][2, "Pr(>F)"]
})
plot(ecdf(pvalues))
abline(0, 1, col = 2)

## -------------------------------------------------------------------------------------------------
AIC(mod_lm)
AIC(mod_lmm_lme4)
print(AIC(mod_lmm_spaMM))

## -------------------------------------------------------------------------------------------------
plot(mod_lmm_lme4, type = c("p", "smooth"))  ## see ?lme4:::plot.merMod for details

## -------------------------------------------------------------------------------------------------
plot(mod_lmm_lme4, resid(., scaled=TRUE) ~ fitted(.) | Block, abline = 0)

## -------------------------------------------------------------------------------------------------
lattice::qqmath(mod_lmm_lme4, id = 0.05) ## id allows to see outliers

## ---- fig.width = 4, fig.height = 4---------------------------------------------------------------
lattice::qqmath(lme4::ranef(mod_lmm_lme4, condVar = TRUE))

## ---- fig.width = 8, fig.height = 4---------------------------------------------------------------
library(DHARMa)
r <- simulateResiduals(mod_lmm_lme4, n = 1000)  ## resimulate BLUPs
plot(r)

## -------------------------------------------------------------------------------------------------
testTemporalAutocorrelation(r, time = 1:nrow(Oats2), plot = FALSE)
r2 <- simulateResiduals(mod_lmm_lme4, re.form = NULL, n = 1000)  ## conditional to fitted BLUPs
testTemporalAutocorrelation(r2, time = 1:nrow(Oats2), plot = FALSE)

## -------------------------------------------------------------------------------------------------
str(Penicillin)
table(Penicillin$sample, Penicillin$plate)

## -------------------------------------------------------------------------------------------------
mod <- fitme(diameter ~ 1 + (1|plate) + (1|sample), data = Penicillin)
mod$lambda

## -------------------------------------------------------------------------------------------------
head(mod$ZAlist[[1]])

## -------------------------------------------------------------------------------------------------
head(mod$ZAlist[[2]], 10)

## -------------------------------------------------------------------------------------------------
head(cake)
str(cake)

## -------------------------------------------------------------------------------------------------
table(cake$recipe, cake$replicate, cake$temperature)

## -------------------------------------------------------------------------------------------------
mod <- fitme(angle ~ recipe + temperature + (1|recipe:replicate), data = cake)
mod$lambda

## -------------------------------------------------------------------------------------------------
cake$replicate_tot <- factor(paste(cake$recipe, cake$replicate, sep = "_"))
levels(cake$replicate_tot)
mod <- fitme(angle ~ recipe + temperature + (1|replicate_tot), data = cake)
mod$lambda

## -------------------------------------------------------------------------------------------------
data("carnivora", package = "ape") 
carnivora$log_brain <- log(carnivora$SB)
carnivora$log_body <- log(carnivora$SW)
str(carnivora)

## -------------------------------------------------------------------------------------------------
tapply(carnivora$Genus, carnivora$Family, function(x) length(unique(x)))

## -------------------------------------------------------------------------------------------------
coplot(log_brain ~ log_body | Family, data = carnivora)

## -------------------------------------------------------------------------------------------------
mod1 <- fitme(log_brain ~ log_body + (1|Family/Genus), data = carnivora)
mod1

## -------------------------------------------------------------------------------------------------
mod1bis <- fitme(log_brain ~ log_body + (1|Family) + (1|Family:Genus), data = carnivora)
mod1bis

## -------------------------------------------------------------------------------------------------
mod1ter <- fitme(log_brain ~ log_body + (1|Family) + (1|Genus), data = carnivora)
mod1ter

## -------------------------------------------------------------------------------------------------
crossprod(as.matrix(mod1$ZAlist[[1]]))

## -------------------------------------------------------------------------------------------------
crossprod(as.matrix(mod1$ZAlist[[2]]))

## -------------------------------------------------------------------------------------------------
lF <- lFormula(log_brain ~ log_body + (1|Family) + (1|Genus), data = carnivora)
lF$reTrms$flist  ## list of grouping factors used in the random-effects terms; see ?mkReTrms

## -------------------------------------------------------------------------------------------------
lapply(ranef(mod1), head, n = 20)

## -------------------------------------------------------------------------------------------------
mod3 <- HLfit(log_brain ~ log_body + (log_body|Family) + (1|Genus), data = carnivora, HLmethod = "ML")
mod3

## -------------------------------------------------------------------------------------------------
ranef(mod3)$`( log_body | Family )`

## ---- fig.width = 4, fig.height = 4---------------------------------------------------------------
plot(log_brain ~ log_body, data = subset(carnivora, Family == "Canidae"), col = "red",
     ylim = range(carnivora$log_brain))
points(log_brain ~ log_body, data = subset(carnivora, Family == "Mustelidae"), col = "blue")
points(log_brain ~ log_body, data = subset(carnivora, Family == "Viverridae"), col = "orange")
abline(mod3$fixef + ranef(mod3)$`( log_body | Family )`["Canidae", ], col = "red", lwd = 2, lty = 2)
abline(mod3$fixef + ranef(mod3)$`( log_body | Family )`["Mustelidae", ], col = "blue", lwd = 2, lty = 2)
abline(mod3$fixef + ranef(mod3)$`( log_body | Family )`["Viverridae", ], col = "orange", lwd = 2, lty = 2)

## -------------------------------------------------------------------------------------------------
mod4 <- lmer(log_brain ~ log_body + (log_body|Family) + (1|Genus), data = carnivora, REML = FALSE)
mod4noRS <- lmer(log_brain ~ log_body + (1|Family) + (1|Genus), data = carnivora, REML = FALSE)
anova(mod4, mod4noRS)

## -------------------------------------------------------------------------------------------------
mod5 <- lm(log_brain ~ log_body * Family, data = carnivora)
mod5noIS <- lm(log_brain ~ log_body + Family, data = carnivora)
anova(mod5, mod5noIS)

## -------------------------------------------------------------------------------------------------
mod4 <- lmer(log_brain ~ log_body + (log_body|Family) + (1|Genus), data = carnivora, REML = FALSE)
mod4noRS <- lmer(log_brain ~ log_body + (1|Family) + (1|Genus), data = carnivora, REML = FALSE)
anova(mod4, mod4noRS)

## -------------------------------------------------------------------------------------------------
mod6 <- lm(log_brain ~ log_body * Family + Genus, data = carnivora)
mod6noIS <- lm(log_brain ~ log_body + Family + Genus, data = carnivora)
anova(mod6, mod6noIS)

## -------------------------------------------------------------------------------------------------
set.seed(1)
Aliens <- SimulateMix(intercept = 50, slope = 1.5, n = 30, group_nb = 10, var.rand = 2, var.error = 0.5)

## ---- message = FALSE-----------------------------------------------------------------------------
library(lme4)
(mod <- lmer(y ~ x + (1|group), data = Aliens))

## ---- message = FALSE-----------------------------------------------------------------------------
library(spaMM)
(mod2 <- fitme(y ~ x + (1|group), data = Aliens, method = "REML"))

## -------------------------------------------------------------------------------------------------
mod2_H0 <- fitme(y ~ x + (1|group), data = Aliens, method = "REML", fixed = list(lambda = 2))
1 - pchisq(2*(logLik(mod2) - logLik(mod2_H0)), df = 1)

## ----test spaMM-----------------------------------------------------------------------------------
test <- replicate(1000, {
  d <-  SimulateMix(intercept = 50, slope = 1.5, n = 30, group_nb = 10, var.rand = 2, var.error = 0.5)
  mod <- fitme(y ~ x + (1|group), data = d, method = "REML")
  mod0 <- fitme(y ~ x + (1|group), data = d, method = "REML",
                fixed = list(lambda = 2))
  1 - pchisq(2*(logLik(mod) - logLik(mod0)), df = 1)
})

## -------------------------------------------------------------------------------------------------
plot(ecdf(test))
abline(0, 1, col = "red")

## ----test spaMM 2---------------------------------------------------------------------------------
test2 <- replicate(1000, {
  d <-  SimulateMix(intercept = 50, slope = 1.5, n = 30, group_nb = 10, var.rand = 0.1, var.error = 0.5)
  mod <- fitme(y ~ x + (1|group), data = d, method = "REML")
  mod0 <- fitme(y ~ x + (1|group), data = d, method = "REML",
                fixed = list(lambda = 0.1))
  1 - pchisq(2*(logLik(mod) - logLik(mod0)), df = 1)
})

## -------------------------------------------------------------------------------------------------
plot(ecdf(test2))
abline(0, 1, col = "red")

## ----distrib lambda large-------------------------------------------------------------------------
lambdas_large <- replicate(1000, {
  d <- SimulateMix(intercept = 50, slope = 1.5, n = 30, group_nb = 10, var.rand = 10, var.error = 0.5)
  mod <- fitme(y ~ x + (1|group), data = d, method = "REML")
  as.numeric(mod$lambda)
})

## ----distrib lambda-------------------------------------------------------------------------------
lambdas <- replicate(1000, {
  d <- SimulateMix(intercept = 50, slope = 1.5, n = 30, group_nb = 10, var.rand = 2, var.error = 0.5)
  mod <- fitme(y ~ x + (1|group), data = d, method = "REML")
  as.numeric(mod$lambda)
})

## ----distrib lambda small-------------------------------------------------------------------------
lambdas_small <- replicate(1000, {
  d <- SimulateMix(intercept = 50, slope = 1.5, n = 30, group_nb = 10, var.rand = 0.1, var.error = 0.5)
  mod <- fitme(y ~ x + (1|group), data = d, method = "REML")
  as.numeric(mod$lambda)
})

## ---- fig.width = 4, fig.height = 4---------------------------------------------------------------
var.between.group <- 10
hist(lambdas_large, nclass = 50, probability = TRUE)
shape <- (10 - 1)/2 ## with 10 being the number of levels
scale <- (2*var.between.group)/(10 - 1)
curve(dgamma(x, shape = shape, scale = scale), from = 0, to = 30, add = TRUE, lwd = 2, col = "red")

## ---- fig.width = 4, fig.height = 4---------------------------------------------------------------
var.between.group <- 2
hist(lambdas, nclass = 50, probability = TRUE)
shape <- (10 - 1)/2 ## with 10 being the number of levels
scale <- (2*var.between.group)/(10 - 1)
curve(dgamma(x, shape = shape, scale = scale), from = 0, to = 7, add = TRUE, lwd = 2, col = "red")

## ---- fig.width = 4, fig.height = 4---------------------------------------------------------------
var.between.group2 <- 0.1
hist(lambdas_small, nclass = 50, probability = TRUE)
shape <- (10 - 1)/2 ## with 10 being the number of levels
scale <- (2*var.between.group2)/(10 - 1)
curve(dgamma(x, shape = shape, scale = scale), from = 0, to = 7, add = TRUE, lwd = 2, col = "red")

## ----CI lambda lme4-------------------------------------------------------------------------------
mod_lmer <- lmer(y ~ x + (1|group), data = Aliens, REML = TRUE)
round(confint(mod_lmer, method = "profile")[1, ]^2, 2)
round(confint(mod_lmer, method = "boot", nsim = 1000)[1, ]^2, 2)

## -------------------------------------------------------------------------------------------------
carnivora$Canidae  <- as.numeric(carnivora$Family == "Canidae")
carnivora$Others   <- as.numeric(carnivora$Family != "Canidae")

mod2 <- fitme(log_brain ~ log_body + (0 + Canidae|Genus) + (0 + Others|Genus), data = carnivora)

## -------------------------------------------------------------------------------------------------
mod2

## -------------------------------------------------------------------------------------------------
as.data.frame(ranef(mod2))

## -------------------------------------------------------------------------------------------------
tail(Gryphon$pedigree)
library(nadiv)
A <- as(makeA(Gryphon$pedigree), "matrix")
colnames(A) <- rownames(A) <- Gryphon$pedigree$ID
A[1305:1309, 1296:1309]

## ----animal model---------------------------------------------------------------------------------
library(spaMM)
system.time(mod1 <- fitme(BWT ~ 1 + corrMatrix(1|ID), corrMatrix = A, data = Gryphon$data, method = "REML"))
(h2 <- as.numeric(mod1$lambda / (mod1$lambda + mod1$phi)))

## ----animal model 2-------------------------------------------------------------------------------
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

## ---- fig.height=3.5, fig.width=4-----------------------------------------------------------------
curve(dnorm(x, sd = sqrt(as.numeric(mod2$lambda["ID"]))), from = -3, to = 3, las = 1,
      ylab = "pdf", xlab = "predicted breeding value")
BLUPs <- ranef(mod2)$"corrMatrix(1 | ID)"
points(dnorm(BLUPs, sd = sqrt(as.numeric(mod2$lambda["ID"]))) ~ BLUPs, col = "blue", type = "h", lwd = 0.1)

## -------------------------------------------------------------------------------------------------
plot(BLUPs ~ scale(mod2$data$BWT), ylab = "Predicted breeding values", xlab = "Scaled phenotypic values")

## -------------------------------------------------------------------------------------------------
library(ade4)
data(carni70)
carni70$tab

## -------------------------------------------------------------------------------------------------
library(ape)
tree <- read.tree(text = carni70$tre)
plot(tree, cex = 0.3)

## -------------------------------------------------------------------------------------------------
(corrM <- vcv(tree, model = "Brownian", corr = TRUE))

## -------------------------------------------------------------------------------------------------
carni70$tab$sp <- factor(rownames(corrM))
(mod_carni <- fitme(range ~ size + corrMatrix(1|sp), corrMatrix = corrM, data = carni70$tab))

## -------------------------------------------------------------------------------------------------
mod_carni_no_size <- fitme(range ~ 1 + corrMatrix(1|sp), corrMatrix = corrM, data = carni70$tab)
anova(mod_carni, mod_carni_no_size)

## ---- message = FALSE-----------------------------------------------------------------------------
library(nlme)
rownames(carni70$tab) <- rownames(corrM)
(mod_carni2 <- gls(range ~ size, correlation = corBrownian(1, tree), method = "ML", data = carni70$tab))

## -------------------------------------------------------------------------------------------------
mod_carni2_no_size <- gls(range ~ 1, correlation = corBrownian(1, tree), method = "ML", data = carni70$tab)
anova(mod_carni2, mod_carni2_no_size)

## ---- message = FALSE-----------------------------------------------------------------------------
library(metafor)
dat.bcg

## -------------------------------------------------------------------------------------------------
dat.bcg$RR  <- with(dat.bcg, log((tpos/(tpos + tneg)) / (cpos/(cpos + cneg))))
dat.bcg$sampling.var <- with(dat.bcg, 1/tpos - 1/tneg + 1/cpos - 1/cneg)
dat.bcg

## -------------------------------------------------------------------------------------------------
(mod <- rma(yi = RR ~ factor(alloc) + year + ablat, vi = sampling.var, data = dat.bcg, method = "REML"))

## -------------------------------------------------------------------------------------------------
dat.bcg$study <- factor(dat.bcg$trial)
(mod_spaMM <- fitme(RR ~ factor(alloc) + year + ablat + (1|study), data = dat.bcg,
                    fixed = list(phi = dat.bcg$sampling.var), method = "REML"))

## -------------------------------------------------------------------------------------------------
round(c(tau2_metafor = mod$tau2, lambda_spaMM = mod_spaMM$lambda[[1]]), 4)  ## heterogeneity between studies

mod_spaMMRE <- fitme(RR ~ 1 + (1|study), data = dat.bcg,
                     fixed = list(phi = dat.bcg$sampling.var), method = "REML")

R2_spaMM <- 100 * (mod_spaMMRE$lambda[[1]] - mod_spaMM$lambda[[1]]) / mod_spaMMRE$lambda[[1]]
round(c(R2_metafor = mod$R2, R2_spaMM = R2_spaMM), 4)  ## amount of heterogeneity accounted for by fixed effects

## ---- cache = FALSE-------------------------------------------------------------------------------
c(mod$I2, mod$H2, mod$QE) ## from metafor

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

## ---- fig.width = 6, fig.height = 5.5, cache = FALSE----------------------------------------------
metafor::plot.rma.uni(mod)  ## or just plot(mod)

## -------------------------------------------------------------------------------------------------
mod_spaMM2    <- fitme(RR ~ factor(alloc) + year + ablat + (1|study), data = dat.bcg,
                       fixed = list(phi = dat.bcg$sampling.var))
mod_spaMM2_H0 <- fitme(RR ~ 1 + (1|study), data = dat.bcg, fixed = list(phi = dat.bcg$sampling.var))
anova(mod_spaMM2, mod_spaMM2_H0)

## ----test for metafor-----------------------------------------------------------------------------
res.boot <- anova(mod_spaMM2, mod_spaMM2_H0, boot.repl = 1000)

## -------------------------------------------------------------------------------------------------
res.boot

## -------------------------------------------------------------------------------------------------
(mod2 <- rma(yi = RR ~ factor(alloc) + year + ablat, vi = sampling.var, data = dat.bcg, method = "ML"))

## -------------------------------------------------------------------------------------------------
(mod3 <- rma(yi = RR ~ factor(alloc) + year + ablat, vi = sampling.var, data = dat.bcg, method = "ML",
             test = "knha"))

## -------------------------------------------------------------------------------------------------
mod_spaMM3    <- fitme(RR ~ 1 + (1|study), data = dat.bcg, fixed = list(phi = dat.bcg$sampling.var))
mod_spaMM3_H0 <- fitme(RR ~ 0 + (1|study), data = dat.bcg, fixed = list(phi = dat.bcg$sampling.var))
anova(mod_spaMM3, mod_spaMM3_H0)

## ----test for metafor 2---------------------------------------------------------------------------
res.boot2 <- anova(mod_spaMM3, mod_spaMM3_H0, boot.repl = 1000)

## -------------------------------------------------------------------------------------------------
res.boot2

## -------------------------------------------------------------------------------------------------
mod_spaMM3$fixef
get_intervals(mod_spaMM3, re.form = NA, intervals = "predVar")[1, ]

## -------------------------------------------------------------------------------------------------
(mod4 <- rma(yi = RR ~ 1, vi = sampling.var, data = dat.bcg, method = "ML"))

## -------------------------------------------------------------------------------------------------
forest(mod4)

