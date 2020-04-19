.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "\n #############################################",
    "\n #                                           #",
    "\n #    This is the package for the course     #",
    "\n #                                           #",
    "\n #        Data science for biologists:       #",
    "\n #    generalized linear modelling with R    #",
    "\n #                                           #",
    "\n #       Version ", utils::packageDescription("LM2GLMM")$Version," installed!       #",
    "\n #                                           #",
    "\n #    To access the slides, type either      #",
    "\n #   browseVignettes(package = 'LM2GLMM')    #",
    "\n #                    or                     #",
    "\n #             get_vignettes()               #",
    "\n #                                           #",
    "\n #############################################", "\n")
}


#' @export
.build_vignettes <- function(quiet = TRUE) {
  devtools::build_vignettes(pkg = ".", dependencies = "VignetteBuilder", clean = FALSE,
                            upgrade = "never", quiet = quiet, install = TRUE,
                            keep_md = TRUE)
  system("mkdir -p ./inst/doc; cp ./doc/*.html ./inst/doc")
  message("To view the vignette, Install & Rebuild, then\n browseVignettes('LM2GLMM')")
  invisible(TRUE)
}


#' @export
.update_drat <- function() {
  path <- devtools::build(binary = FALSE, vignettes = FALSE)
  drat::insertPackage(path, repodir = "../../drat", commit = TRUE)
  drat::pruneRepo(repopath = "../../drat", remove = TRUE)
  message("About to upload the package on GitHub, be patient...")
  system("cd ../../drat; git add .; git commit -m 'Pruning';  git push")
}
