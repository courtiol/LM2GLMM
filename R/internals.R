.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "\n #############################################",
    "\n #                                           #",
    "\n #    This is the package for the course     #",
    "\n #                                           #",
    "\n #    Advanced Statistical Applications:     #",
    "\n #          from LM to GLMM using R          #",
    "\n #                                           #",
    "\n #   You have the version ", utils::packageDescription("LM2GLMM")$Version," installed!   #",
    "\n #                                           #",
    "\n #    To access the slides, type either      #",
    "\n #   browseVignettes(package = 'LM2GLMM')    #",
    "\n #                    or                     #",
    "\n #             get_vignettes()               #",
    "\n #                                           #",
    "\n #############################################", "\n")
}
