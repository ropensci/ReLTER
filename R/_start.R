# start with R package
# follow https://www.r-bloggers.com/2017/08/building-a-website-with-pkgdown-a-short-guide/
require(devtools)
library(dplyr)

# execute this only if you want to restructure the Readme.rmd
# use_readme_rmd()
# the Readme.md
# use_readme_md()

use_news_md()
use_vignette("ReLTER")  #substitute with the name of your package

# use_github_links()

# use_travis()
# use_cran_badge() 
# use_badge()

# man subfolder:
devtools::document()

# my_package.Rmd
devtools::build_vignettes()

#usethis::use_readme_md()
usethis::use_package("dplyr")

# bare_bones website
# devtools::install_githb("hadley/pkgdown")
pkgdown::build_site(examples = F)
