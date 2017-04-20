.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "\n #############################################",
    "\n #                                           #",
    "\n #     This is the package for the course    #",
    "\n #                                           #",
    "\n #    Advanced Statistical Applications:     #",
    "\n #          from LM to GLMM using R          #",
    "\n #                                           #",
    "\n #   You have the version ", utils::packageDescription("LM2GLMM")$Version," installed!   #",
    "\n #                                           #",
    "\n # Type get_vignettes() to access the slides #",
    "\n #                                           #",
    "\n #############################################", "\n")
}
