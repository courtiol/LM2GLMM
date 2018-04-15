## ----setup, include=FALSE-------------------------------------------------------------------------
library(LM2GLMM)
library(spaMM)
library(lme4)
options(width = 100)
knitr::opts_chunk$set(cache = TRUE, cache.path = "./cache_knitr/LMM_more/", fig.path = "./fig_knitr/LMM_more/", fig.width = 5, fig.height = 5, fig.align = "center", error = FALSE)

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

