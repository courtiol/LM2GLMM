#' Function simulating the predictors
#'
#' @name simulate_predictors
#' @aliases simulate_predictors_1C1G
#'
#' @param n the number of datapoints to simulate per group
#' @param k the number of group to simulate
#'
#' @return a \var{data.frame} containing the simulated data
#' @export
#'
#' @examples
#' simulate_predictors_1C1G(n = 3, k = 4)
#'
simulate_predictors_1C1G <- function(n, k){
  result <- data.frame(
    var1 = stats::rnorm(n = n*k),
    group1 = factor(paste0("g_", rep(1:k, each = n)))
  )
  ordered_result <- result[order(result$group1, result$var1), ]
  rownames(ordered_result) <- NULL
  return(ordered_result)
}

#' Function simulating the Alien datasets
#'
#' The Aliens considered here have a size that is varies linearly according to
#' the number of humans they have eaten.
#'
#' @param N the number of Aliens (default = 12)
#' @param intercept the value for the intercep (default = 50)
#' @param slope the value for the slope (default = 1.5)
#' @param sigma2 the value for the error variance (default = 25)
#'
#' @return a \var{data.frame} containing the simulated data
#' @export
#'
#' @examples
#' simulate_Aliens()
#'
simulate_Aliens <- function(N = 12, intercept = 50, slope = 1.5, sigma2 = 25) {
  Aliens <- data.frame(humans_eaten = sample(1:N))
  Aliens$size <- stats::rnorm(n = N, mean = intercept + slope*Aliens$humans_eaten, sd = sqrt(sigma2))
  return(Aliens)
}

#' Function simulating the Alien datasets for GLM
#'
#' This function creates different response variables to be used for GLM.
#'
#' @param N the number of Aliens (default = 12)
#'
#' @return a \var{data.frame} containing the simulated data
#' @export
#'
#' @examples
#' simulate_Aliens_GLM()
#'
simulate_Aliens_GLM <- function(N = 100) {
  Aliens <- data.frame(humans_eaten = round(stats::runif(n = N, min = 0, max = 15)))
  Aliens$size  <- stats::rnorm( n = N, mean = 50 + 1.5 * Aliens$humans_eaten, sd = 5)
  Aliens$eggs  <- stats::rpois( n = N, lambda = exp(-1 + 0.1 * Aliens$humans_eaten))
  Aliens$happy <- stats::rbinom(n = N, size = 1, prob = plogis(-3 + 0.3 * Aliens$humans_eaten))
  Aliens$all_eyes  <- round(stats::runif(nrow(Aliens), min = 1, max = 12))
  Aliens$blue_eyes <- rbinom(n = nrow(Aliens), size = Aliens$all_eyes, prob = stats::plogis(-2 + 0.5 * Aliens$humans_eaten))
  Aliens$pink_eyes <- Aliens$all_eyes - Aliens$blue_eyes
  Aliens$all_eyes <- NULL
  attr(Aliens, "param.eta") <- list("size" = c(intercept = 50, slope = 1.5),
                                "eggs" = c(intercept = -1, slope = 0.1),
                                "happy" = c(intercept = -3, slope = 0.3),
                                "blue_eyes" = c(intercept = -2, slope = 0.5))
  return(Aliens)
}
