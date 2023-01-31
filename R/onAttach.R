.onAttach <- function(libname, pkgname) {
  deimsbaseurl <- get_deims_base_url()
  # to show a startup message
  startMessage <- c(
    "\n\nReLTER is specially drafted for the LTER community.\n\n",
    "To contribute to the improvement of this package, join the group of
    developers (https://github.com/ropensci/ReLTER).\n\n",
    "If you use this package, please cite as:\n\n",
    "Alessandro Oggioni, Micha Silver, Luigi Ranghetti & Paolo Tagliolato.
    (2023) ReLTER: An Interface for the eLTER Community (v2.1). Zenodo.
    https://doi.org/10.5281/zenodo.5576813\n\n",
    "Type 'citation(package = 'ReLTER')' on how to cite R packages in
    publications.\n\n",
    "Currently the DEIMS-SDR base URL is set as: ", deimsbaseurl,
    ".\nTo change the URL please use set_deims_base_url()."
  )
  packageStartupMessage(startMessage)
}
