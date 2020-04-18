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


.build_vignettes <- function() {
  #devtools::build_vignettes(pkg = ".", dependencies = "VignetteBuilder", clean = FALSE,
  #                          upgrade = "never", quiet = FALSE, install = TRUE,
  #                          keep_md = TRUE)
  pkg <- devtools::as.package(".")
  remotes::dev_package_deps(pkg$path, dependencies = "VignetteBuilder")
  message("Building ", pkg$package, " vignettes")
  tools::buildVignettes(dir = pkg$path, tangle = TRUE, clean = FALSE)
  devtools:::copy_vignettes(pkg, keep_md = FALSE)
  invisible(TRUE)
}


.update_drat <- function() {
  path <- devtools::build(binary = FALSE, vignettes = TRUE)
  drat::insertPackage(path, repodir = "~/Boulot/Mes_projets_de_recherche/R_packages/drat", commit = TRUE)
  drat::pruneRepo(repopath = "~/Boulot/Mes_projets_de_recherche/R_packages/drat", remove = TRUE)
  system("cd ~/Boulot/Mes_projets_de_recherche/R_packages/drat; git add .; git commit -m 'Pruning';  git push") ## note: takes a while
}
