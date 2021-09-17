.onAttach <- function(libname, pkgname) {
  # to show a startup message
  startMessage <- c(
    "ReLTER is specially drafted for the LTER community.\n\n",
    "Contribute to the improvement of it, join the group of developers
    (https://github.com/oggioniale/ReLTER).\n\n",
    "If you use this package, please cite it (citation(\'ReLTER\')).\n\n"
  )
  packageStartupMessage(startMessage)
}
