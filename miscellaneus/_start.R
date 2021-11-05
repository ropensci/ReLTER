# start with R package
# follow https://www.r-bloggers.com/2017/08/building-a-website-with-pkgdown-a-short-guide/
require(devtools)
library(dplyr)

# execute this only if you want to restructure the Readme.rmd
# use_readme_rmd()
# the Readme.md
# use_readme_md()

usethis::use_news_md()
usethis::use_vignette("ReLTER")  #substitute with the name of your package

# use_github_links()

# use_travis()
# use_cran_badge() 
# use_badge()

# my_package.Rmd
devtools::build_vignettes()

#usethis::use_readme_md()
usethis::use_package("dplyr")
usethis::use_package("dtplyr")

# bare_bones website
# devtools::install_github("hadley/pkgdown")
pkgload::unload("ReLTER")
# devtools::load_all(".")
# man subfolder:
devtools::document() # per creare file Rd
pkgdown::build_reference()
# prima kintr di README.Rmd, viene prodotto README.md quindi eseguire per ottenere home della documentazione
pkgdown::build_home()
# il comando seguente per ricostruire l'intero sito della documentazione
pkgdown::build_site(examples = T)

# Automated checking https://r-pkgs.org/r-cmd-check.html
devtools::check()

# following this guide https://r-pkgs.org
# after creating the package documentation, test it: https://r-pkgs.org/tests.html
usethis::use_testthat()

# per listare le funzioni dei pacchetti importati dalle funzioni scritte
NCmisc::list.functions.in.file("R/nomefile.R")
