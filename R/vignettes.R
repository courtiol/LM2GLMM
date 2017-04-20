#' Access the vignettes
#'
#' This function allows to access to the vignettes depsite them not being
#' installed properly. I had to do that as I want students to be able to install
#' the package quickly from github. They thus won't use a package bundle, nor I
#' want them to have to build the vignettes on their computers.
#'
#' @export
#'
#' @examples
#' get_vignettes()
#'
get_vignettes <- function() {
  browseURL(paste0(find.package("LM2GLMM"), "/doc/index.html"))
  return(invisible(NULL))
}
