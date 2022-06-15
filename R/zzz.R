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
.build_vignettes <- function(quiet = FALSE) {
  devtools::build_vignettes(pkg = ".", dependencies = "VignetteBuilder", clean = FALSE,
                            upgrade = "never", quiet = quiet, install = TRUE,
                            keep_md = TRUE)
  system("mkdir -p ./inst/doc; cp ./doc/*.html ./inst/doc")
  message("To view the vignette, Install & Rebuild, then\n browseVignettes('LM2GLMM')")
  invisible(TRUE)
}


#' @export
.update_drat <- function() {
  message("Building package (without rebuilding vignettes)...")
  path <- devtools::build(binary = FALSE, vignettes = FALSE)
  message("Adding [package]kg to drat repo...")
  drat::insertPackage(path, repodir = "../../drat", commit = TRUE)
  message("Removing old pkg version from drat repo...")
  drat::pruneRepo(repopath = "../../drat", remove = TRUE)
  message("About to upload the package on GitHub, be patient...")
  system("cd ../../drat; git add .; git commit -m 'Pruning';  git push")
}

#' @export
.emo <- function(x = c("info", "practice", "proof", "goal", "nerd", "party", "broken", "slow", "warn", "alien", "recap")) {
  x <- match.arg(x)
  if (!requireNamespace("emojifont")) stop("You need to install the package emojifont to use this function.")
  switch(x,
    info = emojifont::emoji("mortar_board"),
    practice = emojifont::emoji("wrench"),
    proof = knitr::asis_output("\U1F9EA")[[1]],
    goal = emojifont::emoji("dart"),
    nerd = knitr::asis_output("\U1F913")[[1]],
    party = emojifont::emoji("tada"),
    broken = emojifont::emoji("angry"),
    slow = emojifont::emoji("fire"),
    warn = emojifont::emoji("warning"),
    alien = emojifont::emoji("alien"),
    recap = emojifont::emoji("rewind")
  )
}
