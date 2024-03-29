% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datasets.R
\docType{data}
\name{Bats}
\alias{Bats}
\title{The Bats dataset}
\format{
A data frame with 10 variables:
 \describe{
   \item{log_weight}{The log body weight in grams}
   \item{Spp.y}{An identifier referring to the species}
   \item{Site.y}{A factor indicating the type of habitats in which individuals have been captured}
   \item{FA.y}{The forearm length in mm}
   \item{log_FAz}{The log forearm length turned into z-score by centering and scalling the measurement within species}
   \item{Fed}{A binary factor indicating if the individuals have recently been feeding (Y = Yes, N = No)}
   \item{REP}{A factor indicating the reproductions status (NR = non-reproductive, L = lactating or M = male)}
   \item{Year}{A factor indicating the year of sampling}
   \item{Season}{A factor indicating the season of sampling}
   \item{Site.x}{A factor indicating the type of forest patch in which individuals have been captured}
 }
}
\source{
Anne Seltmann, Gábor Á. Czirják, Alexandre Courtiol, Henry Bernard,
  Matthew J. Struebig, Christian C. Voigt; Habitat disturbance results in
  chronic stress and impaired health status in forest-dwelling paleotropical
  bats. Conserv Physiol 2017; 5 (1): cox020. doi: 10.1093/conphys/cox020
}
\usage{
Bats
}
\description{
This dataset has been collected during field studies with the goal of studying the impact of disturbance on the health of forest-dwelling paleotropical bats.
This dataset has been used to produce the results concerning the body mass of bats in the paper by Seltmann et al. (see source for details).
}
\keyword{datasets}
