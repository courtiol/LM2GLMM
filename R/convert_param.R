#' Convert parameters between equivalent design matrices
#'
#' This function allows for the conversion of parameters between design matrices
#' that are equivalent (same data, same rank) but that have been parameterized
#' differently. This is usefull to find conversion of parameters for different
#' contrasts matrices (e.g. contrast treament vs contrast sum), including
#' conversion between \code{poly(..., raw = FALSE)} and
#' \code{poly(..., raw = TRUE)}.
#'
#' @param XA The orignal design matrix
#' @param XB A design matrix equivalent to the original design matrix but with different parameterization
#' @param betaXA The vector of coefficients for the original design matrix
#'
#' @return The vector of coefficients for the design matrix XB
#' @export
#'
#' @examples
#' xa <- model.matrix(object = ~ x, data = data.frame(x = factor(c("A", "B", "C"))),
#'                    contrasts.arg = list(x = "contr.treatment"))
#' xb <- model.matrix(object = ~ x, data = data.frame(x = factor(c("A", "B", "C"))),
#'                    contrasts.arg = list(x = "contr.sum"))
#' beta_xa <- c(0.5, 2, 1)
#' beta_xb <- convert_betaXA_to_betaXB(XA = xa, XB = xb, betaXA = beta_xa)
#' ## test that we get same predictions as we should:
#' xa %*% beta_xa
#' xb %*% beta_xb
#'
convert_betaXA_to_betaXB <- function(XA, XB, betaXA) {
  ## Test that inputs are OK:
  if (!any(dim(XA) == dim(XB)))   stop("design matrices differ in size")
  if (ncol(XA) != length(betaXA)) stop("betaXA of wrong length")
  ## Identify parameters that need to be converted
  id_col_keep <- which(apply(XA != XB, 2, sum) != 0) ## index columns to keep
  if (all(XA[, 1] == 1)) id_col_keep <- c(1, id_col_keep) ## add intercept
  id_col_drop <- setdiff(1:ncol(XA), id_col_keep) ## index columns to discard
  ## Conversion per se:
  betaXB_temp <- solve(coef(stats::lm.fit(XA[, id_col_keep], XB[, id_col_keep])), betaXA[id_col_keep])
  ## Put back non converted parameters into output
  betaXB <- numeric(length(betaXA))
  betaXB[id_col_keep] <- betaXB_temp
  betaXB[id_col_drop] <- betaXA[id_col_drop]
  ## Output
  return(betaXB)
}
