.build_vignettes <- function(){
  pkg <- devtools::as.package(".")
  devtools::install_deps(pkg, "VignetteBuilder", upgrade = TRUE)
  message("Building ", pkg$package, " vignettes")
  tools::buildVignettes(dir = pkg$path, tangle = TRUE, clean = FALSE)
  devtools:::copy_vignettes(pkg)
  invisible(TRUE)
}

.update_drat <- function() {
  path <- devtools::build(binary = FALSE, vignettes = FALSE)
  drat::insertPackage(path, "~/Boulot/Mes_projets_de_recherche/R_packages/drat", commit = TRUE)
  system("cd ~/Boulot/Mes_projets_de_recherche/R_packages/drat; git push") ## note: takes a while
}
