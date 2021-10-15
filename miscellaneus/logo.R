# create logo ----
# from: https://github.com/GuangchuangYu/hexSticker
# remotes::install_github("GuangchuangYu/hexSticker")
# other references:
# https://codein.withgoogle.com/archive/2019/organization/5963835628847104/task/4960363093688320/
# http://hexb.in/
# https://github.com/maxogden/hexbin#submit-a-hexagon

## Loading Google fonts (http://www.google.com/fonts)
sysfonts::font_add_google("Balsamiq Sans", "balsamiq", bold.wt = 700)
## Automatically use showtext to render text for future devices
showtext::showtext_auto()
p <- ggplot2::ggplot(ggplot2::aes(x = mpg, y = wt), data = mtcars) + ggplot2::geom_point()
p <- p + ggplot2::theme_void() + hexSticker::theme_transparent()
## use the ggplot2 example
hexSticker::sticker(
  # subplot in logo
  subplot = "miscellaneus/elterLogo_leaf.png",
  s_x = 1, s_y = .8, s_width = .6, s_height = 1,
  # hexagon characteristics
  h_size = 1.2, h_fill = "#FFFFFF", h_color = "#EF9530",
  # name of package
  package="ReLTER",
  p_size = 23, p_family = "balsamiq", p_fontface = "bold", p_color = "#2C77B1", p_x = 1, p_y = 1.4,
  # output
  # filename="miscellaneus/logo.svg", 
  filename="miscellaneus/logo.png", 
  # URL
  url = 'https://www.lter-europe.net/', 
  u_angle = "30", u_size = 5
)

# create favicon 
# https://pkgdown.r-lib.org/reference/build_favicon.html ----
# This function auto-detects the location of your package logo (with the name logo.svg 
# (recommended format) or logo.png) and runs it through the https://realfavicongenerator.net 
# API to build a complete set of favicons with different sizes, as needed for modern web usage.
pkgdown::build_favicons(pkg = ".", overwrite = TRUE)

# use logo ----
# This function helps you use a logo in your package:
# Enforces a specific size
# Stores logo image file at man/figures/logo.png
# Produces the markdown text you need in README to include the logo
usethis::use_logo("./man/figures/logo.png", geometry = "240x278", retina = TRUE)
