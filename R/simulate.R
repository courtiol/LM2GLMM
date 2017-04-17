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
  Alien <- data.frame(humans_eaten = sample(1:N))
  Alien$size <- rnorm(n = N, mean = intercept + slope*Alien$humans_eaten, sd = sqrt(sigma2))
  return(Alien)
}
