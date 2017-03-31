#' Function simulating the predictors
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
    var1 = rnorm(n = n*k),
    group1 = factor(paste0("g_", rep(1:k, each = n)))
  )
  ordered_result <- result[order(result$group1, result$var1), ]
  rownames(ordered_result) <- NULL
  return(ordered_result)
}
