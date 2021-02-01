# start with R package
# 1. https://support.rstudio.com/hc/en-us/articles/200486488-Developing-Packages-with-RStudio

# documentation with roxygen2
roxygen2::roxygenise()

# Prerequisites pkgdown
require(devtools)
use_readme_rmd()
use_news_md()
use_vignette("ReLTER")  #substitute with the name of your package
use_github_links()
# creating a bare-bones website with pkgdown
# devtools::install_github("hadley/pkgdown")
pkgdown::build_site()
