## ----setup, include=FALSE-------------------------------------------------------------------------
library(LM2GLMM)
library(spaMM)
library(lme4)
options(width = 100)
knitr::opts_chunk$set(cache = TRUE, cache.path = "./cache_knitr/LMM_more/", fig.path = "./fig_knitr/LMM_more/", fig.width = 5, fig.height = 5, fig.align = "center", error = FALSE)

## -------------------------------------------------------------------------------------------------
AirPassengers

## -------------------------------------------------------------------------------------------------
plot(AirPassengers)

## -------------------------------------------------------------------------------------------------
air <- data.frame(passengers = as.numeric(AirPassengers),
                  year = rep(1949:1960, each = 12),
                  month = factor(rep(1:12, 12)))
air

## -------------------------------------------------------------------------------------------------
plot(with(air, tapply(passengers, year, mean)) ~ I(1949:1960),
     ylab = "Mean number of passengers", xlab = "Year", type = "b")

## -------------------------------------------------------------------------------------------------
plot(with(air, tapply(passengers, month, mean)) ~ I(1:12),
     ylab = "Mean number of passengers", xlab = "Month", type = "b")

## -------------------------------------------------------------------------------------------------
(mod_air <- lm(passengers ~ year * month, data = air))

## -------------------------------------------------------------------------------------------------
plot(residuals(mod_air), type = "b")
abline(h = 0, lty = 2, col = "red")

## -------------------------------------------------------------------------------------------------
lmtest::dwtest(mod_air)

## -------------------------------------------------------------------------------------------------
acf(residuals(mod_air))

## ---- message = FALSE-----------------------------------------------------------------------------
library(nlme)
MAR1 <- corAR1(value = 0.5, form = ~ 1|year, fixed = FALSE)
MAR1 <- Initialize(MAR1, data = air)
round(corMatrix(MAR1)[["1950"]], 2)

## -------------------------------------------------------------------------------------------------
(mod_air2 <- lme(passengers ~ month * year, random = ~ 1 | year, data = air,
                 correlation = MAR1, method = "REML"))

## -------------------------------------------------------------------------------------------------
(mod_air2b <- lme(passengers ~ month * year, random = ~ 1 | year, data = air,
                 correlation = corAR1(form = ~ 1|year), method = "REML"))

## -------------------------------------------------------------------------------------------------
mod_air3 <- lme(passengers ~ month * year, random = ~ 1 | year, data = air, method = "REML")
anova(mod_air2, mod_air3)

## -------------------------------------------------------------------------------------------------
mod_airARMA1 <- update(mod_air2, correlation = corARMA(form = ~ 1 | year, p = 1, q = 0))
mod_airARMA2 <- update(mod_air2, correlation = corARMA(form = ~ 1 | year, p = 4, q = 0))
mod_airARMA3 <- update(mod_air2, correlation = corARMA(form = ~ 1 | year, p = 2, q = 2))

rbind(mod_air2 = AIC(mod_air2),
      mod_airARMA1 = AIC(mod_airARMA1),
      mod_airARMA2 = AIC(mod_airARMA2),
      mod_airARMA3 = AIC(mod_airARMA3))

## -------------------------------------------------------------------------------------------------
mod_air4 <- update(mod_air2, correlation = corARMA(form = ~ 1 | year, p = 4, q = 0), method = "ML")
data.for.plot <- expand.grid(month = factor(1:12), year = 1949:1960)
data.for.plot$obs <- air$passengers
data.for.plot$time <- seq(1949, 1960, length = (1960 - 1949 + 1) * 12)
data.for.plot$fit_lm <- predict(mod_air)
data.for.plot$fit_lme <- predict(mod_air4)

## -------------------------------------------------------------------------------------------------
plot(obs ~ time, data = data.for.plot, type = "l", ylim = c(0, 700), ylab = "Passengers")
points(fit_lm ~ time, data = data.for.plot, type = "l", col = "red")
points(fit_lme ~ time, data = data.for.plot, type = "l", col = "blue")

## -------------------------------------------------------------------------------------------------
plot(residuals(mod_air4), type = "l")
abline(h = 0, lty = 2, col = "red")

## ---- fig.width = 10------------------------------------------------------------------------------
data("BodyWeight", package = "nlme")
plot(BodyWeight)

## -------------------------------------------------------------------------------------------------
body <- as.data.frame(BodyWeight)
body$Rat <- factor(body$Rat, levels = 1:16, order = FALSE)
str(body)
unique(body$Time)

## -------------------------------------------------------------------------------------------------
(mod_rat1 <- lme(weight ~ Diet * Time, random = ~ Time|Rat, data = body))

## -------------------------------------------------------------------------------------------------
plot(mod_rat1) ## there is some homoscedasticity but we will ignore it for now

## -------------------------------------------------------------------------------------------------
plot(residuals(mod_rat1), type = "b")

## -------------------------------------------------------------------------------------------------
(mod_rat2 <- lme(weight ~ Diet * Time, random = ~ Time|Rat, correlation = corExp(form = ~ Time), data = body))

## -------------------------------------------------------------------------------------------------
anova(mod_rat1, mod_rat2)

## -------------------------------------------------------------------------------------------------
mod_rat3 <- update(mod_rat1, corr = corExp(form = ~ Time, nugget = TRUE))
mod_rat4 <- update(mod_rat1, corr = corRatio(form = ~ Time))
mod_rat5 <- update(mod_rat1, corr = corSpher(form = ~ Time))
mod_rat6 <- update(mod_rat1, corr = corLin(form = ~ Time))
mod_rat7 <- update(mod_rat1, corr = corGaus(form = ~ Time))

rbind(mod_rat2 = AIC(mod_rat2),
      mod_rat3 = AIC(mod_rat3),
      mod_rat4 = AIC(mod_rat4),
      mod_rat5 = AIC(mod_rat5),
      mod_rat6 = AIC(mod_rat6),
      mod_rat7 = AIC(mod_rat7))

## -------------------------------------------------------------------------------------------------
mod_rat3

## ----corrHLfit spaMM------------------------------------------------------------------------------
(mod_rat_spaMM <- corrHLfit(weight ~ Diet * Time + (Time|Rat) + Matern(1|Time), data = body,
                            HLmethod = "REML", init.corrHLfit = list(Nugget = 0.1), ranFix = list(nu = 0.5)))

## -------------------------------------------------------------------------------------------------
plot(predict(mod_rat_spaMM), predict(mod_rat3))
abline(0, 1, col = "red")

## ----corrHLfit spaMM2-----------------------------------------------------------------------------
mod_rat_spaMM2 <- corrHLfit(weight ~ Diet * Time + (Time|Rat) + Matern(1|Time), data = body,
                            HLmethod = "REML", init.corrHLfit = list(Nugget = 0))

mod_rat_spaMM3 <- corrHLfit(weight ~ Diet * Time + (Time|Rat) + Matern(1|Time), data = body,
                            HLmethod = "REML", ranFix = list(Nugget = 0, nu = 0.5))

## -------------------------------------------------------------------------------------------------
print(AIC(mod_rat_spaMM))
print(AIC(mod_rat_spaMM2))
print(AIC(mod_rat_spaMM3))

## -------------------------------------------------------------------------------------------------
time.pred <- seq(0, 64, 0.1)
corr <- MaternCorr(time.pred, rho = mod_rat_spaMM$corrPars$rho, nu = 0.5, Nugget = mod_rat_spaMM$corrPars$Nugget)
plot(corr ~ time.pred, xlab = "Time (days)", ylab = "Correlation", type = "l")

## ----spaMM rat------------------------------------------------------------------------------------
mod_rat_spaMM3ML <- corrHLfit(weight ~ Diet * Time + (Time|Rat) + Matern(1|Time), data = body,
                            HLmethod = "ML", ranFix = list(Nugget = 0, nu = 0.5))
mod_rat_no_diet <- corrHLfit(weight ~ 1 + Time + (Time|Rat) + Matern(1|Time), data = body,
                            HLmethod = "ML", ranFix = list(Nugget = 0, nu = 0.5))

## -------------------------------------------------------------------------------------------------
anova(mod_rat_spaMM3ML, mod_rat_no_diet)

c(logLik(mod_rat_spaMM3ML), logLik(mod_rat_no_diet))

## ----nlme rat-------------------------------------------------------------------------------------
mod_rat3ML <- lme(weight ~ Diet * Time, random = ~ Time|Rat,
                  correlation = corExp(form = ~ Time, nugget = TRUE), data = body, method = "ML")

mod_rat_no_diet2 <- lme(weight ~ 1 + Time, random = ~ Time|Rat,
                        correlation = corExp(form = ~ Time, nugget = TRUE), data = body, method = "ML")

## -------------------------------------------------------------------------------------------------
anova(mod_rat3ML, mod_rat_no_diet2)

## ----spaMM air------------------------------------------------------------------------------------
air$time <- seq(1949, 1960, length = (1960 - 1949 + 1) * 12)

mod_air_spaMM1 <- fitme(passengers ~ month * year + Matern(1|time), data = air, method = "REML")

mod_air_spaMM2 <- fitme(passengers ~ month * year + Matern(1|time), data = air, method = "REML",
                        init = list(Nugget = 0))

mod_air_spaMM3 <- fitme(passengers ~ month * year + Matern(1|time), data = air, method = "REML",
                        init = list(Nugget = 0), fixed = list(nu = 0.5))

mod_air_spaMM4 <- fitme(passengers ~ month * year + Matern(1|time), data = air, method = "REML",
                        fixed = list(nu = 0.5, Nugget = 0))

## -------------------------------------------------------------------------------------------------
print(AIC(mod_air_spaMM1))
print(AIC(mod_air_spaMM2))
print(AIC(mod_air_spaMM3))
print(AIC(mod_air_spaMM4))

## -------------------------------------------------------------------------------------------------
mod_air_spaMM2

## ---- fig.height = 4, fig.width = 4---------------------------------------------------------------
data.for.plot$pred_spaMM <- predict(mod_air_spaMM2)
plot(obs ~ time, data = data.for.plot, type = "l", lwd = 3, ylim = c(0, 700), ylab = "Passengers")
points(pred_spaMM ~ time, data = data.for.plot, type = "l", col = "green")

## ----spaMM air 2----------------------------------------------------------------------------------
mod_air_spaMM2ML <- fitme(passengers ~ month + year + Matern(1|time), data = air, method = "ML",
                        init = list(Nugget = 0))

mod_air_no_year <- fitme(passengers ~ month + Matern(1|time), data = air, method = "ML",
                        init = list(Nugget = 0))

## -------------------------------------------------------------------------------------------------
anova(mod_air_spaMM2ML, mod_air_no_year)

c(logLik(mod_air_spaMM2ML), logLik(mod_air_no_year))

## -------------------------------------------------------------------------------------------------
mod_air3ML <- lme(passengers ~ month + year, random = ~ 1 | year, data = air,
                 correlation = corARMA(p = 6, q = 0), method = "ML")

mod_air_no_year2 <- lme(passengers ~ month, random = ~ 1 | year, data = air,
                 correlation = corARMA(p = 6, q = 0), method = "ML")

## -------------------------------------------------------------------------------------------------
anova(mod_air3ML, mod_air_no_year2)

## -------------------------------------------------------------------------------------------------
data("Loaloa")
ndvi <- Loaloa[, c("maxNDVI", "latitude", "longitude")]
head(ndvi)

## ---- fig.width = 9-------------------------------------------------------------------------------
library(maps)
spaMMplot2D(x = ndvi$longitude, y = ndvi$latitude, z = ndvi$maxNDVI, add.map = TRUE,
            xlab = "Longitude", ylab = "Latitude", plot.title = title(main = "max NDVI"))

## -------------------------------------------------------------------------------------------------
pairs(ndvi)

## ----mod ndvi-------------------------------------------------------------------------------------
(mod_ndvi1 <- fitme(maxNDVI ~ 1 + Matern(1|longitude + latitude), data = ndvi, method = "REML"))

## ----mapMM 1, fig.width = 9-----------------------------------------------------------------------
mapMM(mod_ndvi1, add.map = TRUE, plot.title = title(xlab = "Longitude", ylab = "Latitude"))

## ----mapMM 2, fig.width = 9-----------------------------------------------------------------------
filled.mapMM(mod_ndvi1, add.map = TRUE, plot.title = title(xlab = "Longitude", ylab = "Latitude"))

## -------------------------------------------------------------------------------------------------
x.for.pred <- seq(min(ndvi$longitude), max(ndvi$longitude), length.out = 100)
y.for.pred <- seq(min(ndvi$latitude), max(ndvi$latitude), length.out = 50)
data.for.pred <- expand.grid(longitude = x.for.pred, latitude = y.for.pred)
gridpred <- predict(mod_ndvi1, newdata = data.for.pred, variances = list(predVar = TRUE))
data.for.pred$predVar <- attr(gridpred, "predVar")
m <- matrix(data.for.pred$predVar, ncol = length(y.for.pred))

## ---- fig.width = 9-------------------------------------------------------------------------------
spaMM.filled.contour(x = x.for.pred, y = y.for.pred, z = m, plot.axes = {
  points(ndvi[, c("longitude", "latitude")])}, col = spaMM.colors(redshift = 3))

## -------------------------------------------------------------------------------------------------
Flatwork

## -------------------------------------------------------------------------------------------------
str(Flatwork)

## -------------------------------------------------------------------------------------------------
(mod_glmm_lme4 <- glmer(shopping ~ gender + (1|individual) + (1|month), family = poisson(),
                        data = Flatwork))

## -------------------------------------------------------------------------------------------------
(mod_glmm_spaMM <- fitme(shopping ~ gender + (1|individual) + (1|month), family = poisson(),
                        data = Flatwork, method = "ML"))

## ---- fig.width = 9-------------------------------------------------------------------------------
library(DHARMa)
r <- simulateResiduals(mod_glmm_lme4)
plot(r)

## -------------------------------------------------------------------------------------------------
barplot(table(Flatwork$shopping))

## -------------------------------------------------------------------------------------------------
testZeroInflation(r)

## -------------------------------------------------------------------------------------------------
Flatwork$shopping_bin <- Flatwork$shopping > 0
(mod_glmm_lme4bin <- glmer(shopping_bin ~ gender + (1|individual) + (1|month), family = binomial(),
                        data = Flatwork))

## ---- fig.width = 9-------------------------------------------------------------------------------
r_bin <- simulateResiduals(mod_glmm_lme4bin)
plot(r_bin)

## ----overdisp, warning = FALSE--------------------------------------------------------------------
r_bin2 <- simulateResiduals(mod_glmm_lme4bin, refit = TRUE)  ## slow and convergence issues...
testOverdispersion(r_bin2)

## -------------------------------------------------------------------------------------------------
mod_glmm_lme4bin0 <- glmer(shopping_bin ~ 1 + (1|individual) + (1|month), family = binomial(),
                        data = Flatwork)

anova(mod_glmm_lme4bin, mod_glmm_lme4bin0)

## -------------------------------------------------------------------------------------------------
mod_glmm_spaMMbin <- fitme(shopping_bin ~ gender + (1|individual) + (1|month), family = binomial(),
                        data = Flatwork)

mod_glmm_spaMMbin0 <- fitme(shopping_bin ~ 1 + (1|individual) + (1|month), family = binomial(),
                        data = Flatwork)

anova(mod_glmm_spaMMbin, mod_glmm_spaMMbin0)

## -------------------------------------------------------------------------------------------------
Flatwork_pos <- subset(Flatwork, Flatwork$shopping_bin)
barplot(table(Flatwork_pos$shopping))

## ----truncated fit--------------------------------------------------------------------------------
mod_glmm_lme4pos1 <- glmer(shopping ~ gender + (1|individual) + (1|month), family = poisson(),
                        data = Flatwork_pos)
mod_glmm_lme4pos2 <- glmer.nb(shopping ~ gender + (1|individual) + (1|month), data = Flatwork_pos)
mod_glmm_spaMMpos1 <- fitme(shopping ~ gender + (1|individual) + (1|month), family = poisson(),
                        data = Flatwork_pos)
mod_glmm_spaMMpos2 <- fitme(shopping ~ gender + (1|individual) + (1|month), family = spaMM::negbin(),
                        data = Flatwork_pos)
mod_glmm_spaMMpos3 <- fitme(shopping ~ gender + (1|individual) + (1|month), family = COMPoisson(),
                        data = Flatwork_pos)

c(AIC(mod_glmm_lme4pos1), AIC(mod_glmm_lme4pos2))
print(c(AIC(mod_glmm_spaMMpos1), AIC(mod_glmm_spaMMpos2), AIC(mod_glmm_spaMMpos3)))

## ---- fig.width = 9-------------------------------------------------------------------------------
r_pos <- simulateResiduals(mod_glmm_lme4pos2)
plot(r_pos)

## -------------------------------------------------------------------------------------------------
mod_glmm_spaMMpos20 <- fitme(shopping ~ 1 + (1|individual) + (1|month), family = spaMM::negbin(),
                        data = Flatwork_pos)

anova(mod_glmm_spaMMpos20, mod_glmm_spaMMpos2)

## -------------------------------------------------------------------------------------------------
library(MASS)

mod_negbin <- glm.nb(Days ~ Sex/Age, data = quine)

quine$index <- factor(1:nrow(quine))

mod_poiss_gamma <- fitme(Days ~ Sex/Age + (1|index), data = quine,
                         family = poisson(), rand.family = Gamma("log"))

rbind(mod_negbin$coefficients, mod_poiss_gamma$fixef)

## -------------------------------------------------------------------------------------------------
data(seeds)
seeds

## -------------------------------------------------------------------------------------------------
coplot(r/n ~ plate | seed + extract, data = seeds)

## -------------------------------------------------------------------------------------------------
(mod_germ1 <- fitme(cbind(r, n - r) ~ seed * extract + (1|plate), family = binomial(), rand.family = Beta(),
                   data = seeds, method = "REML"))

## -------------------------------------------------------------------------------------------------
mod_germ2 <- fitme(cbind(r, n - r) ~ seed * extract + (1|plate), family = binomial(),
                   data = seeds, method = "REML")

print(rbind(AIC(mod_germ1), AIC(mod_germ2)))

## -------------------------------------------------------------------------------------------------
mod_rat_spaMM <- corrHLfit(weight ~ Diet * Time + (Time|Rat) + Matern(1|Time), data = body,
                            HLmethod = "REML", init.corrHLfit = list(Nugget = 0.1), ranFix = list(nu = 0.5))

## -------------------------------------------------------------------------------------------------
coplot(residuals(mod_rat_spaMM) ~ I(1:nrow(body)) | body$Diet, show.given = FALSE)

## ----rat hetero-----------------------------------------------------------------------------------
mod_rat_hetero <- corrHLfit(weight ~ Diet * Time + (Time|Rat) + Matern(1|Time), data = body,
                           HLmethod = "REML", init.corrHLfit = list(Nugget = 0.1), ranFix = list(nu = 0.5),
                           resid.formula = ~ Diet)

## ---- results="hide"------------------------------------------------------------------------------
summary.tables <- summary(mod_rat_hetero)

## -------------------------------------------------------------------------------------------------
summary.tables$phi_table

## -------------------------------------------------------------------------------------------------
print(rbind(AIC(mod_rat_spaMM),
            AIC(mod_rat_hetero)))

## ----hetero2--------------------------------------------------------------------------------------
mod_rat_hetero <- corrHLfit(weight ~ Diet * Time + (Time|Rat) + Matern(1|Time), data = body,
                           HLmethod = "ML", init.corrHLfit = list(Nugget = 0.1), ranFix = list(nu = 0.5),
                           resid.formula = ~ Diet)

mod_rat_hetero0 <- corrHLfit(weight ~ Time + (Time|Rat) + Matern(1|Time), data = body,
                           HLmethod = "ML", init.corrHLfit = list(Nugget = 0.1), ranFix = list(nu = 0.5),
                           resid.formula = ~ Diet)

anova(mod_rat_hetero, mod_rat_hetero0)

## -------------------------------------------------------------------------------------------------
set.seed(1L)
d <- data.frame(y = c(rnorm(100, mean = 10, sd = sqrt(10)),
                      rnorm(100, mean = 20, sd = sqrt(20))),
                group = factor(c(rep("A", 100), rep("B", 100))))

m <- fitme(y ~ group, resid.model = ~ group, data = d, method = "REML")
unique(m$phi)

## -------------------------------------------------------------------------------------------------
library(IsoriX)

## -------------------------------------------------------------------------------------------------
dim(GNIPDataDE)
head(GNIPDataDE)

## -------------------------------------------------------------------------------------------------
GNIPDataDE_agg <- prepdata(data = GNIPDataDE)
dim(GNIPDataDE_agg)
head(GNIPDataDE_agg)

## ---- warning=FALSE-------------------------------------------------------------------------------
Europefit <- isofit(iso.data = GNIPDataDE_agg, mean.model.fix = list(elev = TRUE, lat.abs = TRUE))

## ---- message = FALSE-----------------------------------------------------------------------------
library(rasterVis)
plot(ElevRasterDE)

## ----isorix, warning = FALSE----------------------------------------------------------------------
isoscape <- isoscape(elevation.raster = ElevRasterDE, isofit = Europefit)

plot.mean <- plot(x = isoscape, which = "mean", plot = FALSE)

plot.disp <- plot(x = isoscape, which = "disp", plot = FALSE)

## -------------------------------------------------------------------------------------------------
plot.mean

## -------------------------------------------------------------------------------------------------
plot.disp

## -------------------------------------------------------------------------------------------------
disp.fit <- fitme(var.isoscape.value ~ 1 + Matern(1 | long + lat), family = Gamma(log),
                  prior.weights = n.isoscape.value - 1, method = "REML", fixed = list(phi = 2),
                  control.dist = list(dist.method = "Earth"), data = GNIPDataDE_agg)

## -------------------------------------------------------------------------------------------------
GNIPDataDE_agg$pred.disp <- predict(disp.fit)[, 1]

## -------------------------------------------------------------------------------------------------
mean.fit <- fitme(isoscape.value ~ lat + elev + Matern(1 | long + lat), family = gaussian(), 
                  prior.weights = n.isoscape.value, method = "REML",
                  resid.model = list(formula = ~ 0 + offset(pred.disp), family = Gamma(identity)),
                  control.dist = list(dist.method = "Earth"), data = GNIPDataDE_agg)

## -------------------------------------------------------------------------------------------------
Europefit2 <- list(mean.fit = mean.fit, disp.fit = disp.fit)

## ---- warning=FALSE, message=FALSE----------------------------------------------------------------
isoscape2 <- isoscape(elevation.raster = ElevRasterDE, isofit = Europefit2)
plot(x = isoscape2, which = "mean", plot = TRUE)

## ----DHGLM, warning = FALSE, message = FALSE, eval = FALSE----------------------------------------
#  system.time(
#    dhglm <- corrHLfit(isoscape.value ~ lat + elev + Matern(1 | long + lat), family = gaussian(),
#                HLmethod = "REML", data = GNIPDataDE, control.dist = list(dist.method = "Earth"),
#                resid.model = list(formula = ~ 1 + Matern(1 | long + lat),
#                                   control.dist = list(dist.method = "Earth"),
#                                   family = Gamma(log), fixed = list(phi = 2)),
#                                   verbose = c(TRACE = TRUE))
#  )

