# start with R package
# follow https://www.r-bloggers.com/2017/08/building-a-website-with-pkgdown-a-short-guide/
require(devtools)
use_readme_rmd()
use_news_md()
use_vignette("ReLTER")  #substitute with the name of your package

use_github_links()

# use_travis()
use_cran_badge() 

# man subfolder:
devtools::document()

# my_package.Rmd:
devtools::build_vignettes()

# bare_bones website
devtools::install_githb("hadley/pkgdown")
library(pkgdown)
build_site()
