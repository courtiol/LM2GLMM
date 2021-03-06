% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulate.R
\name{simulate_Mix}
\alias{simulate_Mix}
\title{Function simulating the observations under a simple LMM}
\usage{
simulate_Mix(
  intercept = 50,
  slope = 1.5,
  n = 30,
  group_nb = 10,
  var.rand = 2,
  var.error = 0.5
)
}
\arguments{
\item{intercept}{the value for the intercept}

\item{slope}{the value for the slope}

\item{n}{the sample size}

\item{group_nb}{the number of levels for the random effect}

\item{var.rand}{the variance of the random effect}

\item{var.error}{the residual error variance}
}
\value{
a \var{data.frame} containing the simulated data
}
\description{
This function creates observations under LMM.
}
\examples{
simulate_Mix()

}
