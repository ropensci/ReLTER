.onAttach <- function(libname, pkgname) {
  # to show a startup message
  startMessage <- c(
    "\n\nReLTER is specially drafted for the LTER community.\n\n",
    "To contribute to the improvement of this package, join the group of developers
    (https://github.com/oggioniale/ReLTER).\n\n",
    "If you use this package, please cite as:\n",
    "Alessandro Oggioni. (2021). oggioniale/ReLTER: v0.3.0 (0.3.0). Zenodo. https://doi.org/10.5281/zenodo.5576813\n",
    "'citation('ReLTER')'.\n"
  )
  packageStartupMessage(startMessage)
}
