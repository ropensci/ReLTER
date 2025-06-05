#' Provide a map object of a sites LTER.
#' @description `r lifecycle::badge("stable")`
#' This function produces a `map` of the site boundaries
#' as provided by the \href{https://deims.org/}{DEIMS-SDR catalogue}, within
#' a given country and network.
#' @param deimsid A `character`. The DEIMS ID of network from
#' DEIMS-SDR website. DEIMS ID information
#' \href{https://deims.org/docs/deimsid.html}{here}.
#' @param scale_location A `character`. Position of the map scale (e.g. "bl", "br").
#' Options: `"tl"` = top-left, `"tr"` = top-right, `"bl"` = bottom-left, `"br"` = bottom-right.
#' Default is `"bl"`.
#' @param arrow_location A `character`. Position of the north arrow (e.g. "tl", "tr").
#' Options: `"tl"` = top-left, `"tr"` = top-right, `"bl"` = bottom-left, `"br"` = bottom-right.
#' Default is `"tl"`.
#' @param inset_position A `character`. Position of the country overview inset map.
#' Options: `"tl"` = top-left, `"tr"` = top-right, `"bl"` = bottom-left, `"br"` = bottom-right.
#' Default is `"br"`.
#' @return The output of the function is an `image` of the boundary of the
#' site, OSM as base map and all country sites map.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @importFrom sf st_transform st_as_sfc st_sf st_bbox st_simplify
#' @importFrom tibble tribble
#' @importFrom ggplot2 ggplot geom_sf theme element_blank labs 
#' @importFrom ggplot2 theme_minimal element_text coord_sf element_rect
#' @seealso [ggspatial::annotation_map_tile()]
#' @seealso [ggspatial::annotation_scale()]
#' @seealso [ggspatial::annotation_north_arrow()]
#' @seealso [cowplot::ggdraw()]
#' @seealso [cowplot::draw_plot()]
#' @seealso [geodata::gadm()]
#' @references
#'   \insertRef{sfR}{ReLTER}
#'
#'   \insertRef{tibbleR}{ReLTER}
#'   
#'   \insertRef{ggplot2R}{ReLTER}
#'   
#'   \insertRef{ggspatialR}{ReLTER}
#'   
#'   \insertRef{cowplotR}{ReLTER}
#'   
#'   \insertRef{geodataR}{ReLTER}
#' @export
#' @examples
#' \dontrun{
#' # Example of Lange Bramke site
#' siteMap <- produce_site_map(
#'   deimsid = "https://deims.org/8e24d4f8-d6f6-4463-83e9-73cac2fd3f38"
#' )
#' 
#' # Example of Eisenwurzen site
#' siteMap <- produce_site_map(
#'   deimsid = "https://deims.org/d0a8da18-0881-4ebe-bccf-bc4cb4e25701",
#'   inset_position = "bl"
#' )
#' 
#' # Example of Lake Maggiore site
#' siteMap <- produce_site_map(
#'   deimsid = "https://deims.org/f30007c4-8a6e-4f11-ab87-569db54638fe",
#'   scale_location = "bl",
#'   arrow_location = "tl",
#'   inset_position = "br"
#' )
#' }
#'
#' @section The function output:
#' \figure{produce_site_map_fig.png}{Lake Maggiore site map}
#'
### function produce_site_map
produce_site_map <- function(deimsid, scale_location = "bl", arrow_location = "tl", inset_position = "br") {
  # Check if required packages are installed
  if (!requireNamespace("ggspatial", quietly = TRUE)) {
    stop(
      "\n----\nThe function 'produce_site_map()' requires the optional package 'ggspatial'.\n",
      "Please install it with: install.packages(\"ggspatial\")\n----\n"
    )
  }
  if (!requireNamespace("cowplot", quietly = TRUE)) {
    stop(
      "\n----\nThe function 'produce_site_map()' requires the optional package 'cowplot'.\n",
      "Please install it with: install.packages(\"cowplot\")\n----\n"
    )
  }
  if (!requireNamespace("geodata", quietly = TRUE)) {
    stop(
      "\n----\nThe function 'produce_site_map()' requires the optional package 'geodata'.\n",
      "Please install it with: install.packages(\"geodata\")\n----\n"
    )
  }
  # Load required packages
  siteInfo <- get_site_info(
    deimsid = deimsid,
    categories = c("EnvCharacts", "Affiliations"),
    show_map = TRUE,
    with_locations = FALSE
  )
  # Load Biome information
  biomeColor <- tibble::tribble(
    ~ geoBonBiome, ~ fill, ~ border,
    "Marine", "#055ca8", "#057ae1",
    "Coastal", "#43903f", "#5ecc58",
    "Fresh water lakes", "#03a3b8", "#04d0eb",
    "Fresh water rivers", "#03a3b8", "#04d0eb",
    "Terrestrial", "#b07c03", "#e8a303"
  )
  geoBonBiome <- siteInfo$data$geoBonBiome[[1]]
  if (length(geoBonBiome) > 1) {
    geoBonBiome = "Terrestrial"
  } else {
    geoBonBiome <- geoBonBiome
  }
  color <- biomeColor$fill[biomeColor$geoBonBiome == geoBonBiome]
  borderColor <-
    biomeColor$border[biomeColor$geoBonBiome == geoBonBiome]
  # Set the color and border color
  networkID <- tail(siteInfo$data$networks[[1]]$uri, 1)
  countryCode <- isoCodes$Alpha_3[isoCodes$Name == siteInfo$data$country]
  countryPlot <- produce_network_points_map(
    networkDEIMSID = networkID,
    countryCode = countryCode
  ) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank()
    ) +
    ggplot2::labs(title = NULL, subtitle = NULL) +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "white", color = "black", size = 1)
    )
  # Add the point of the site
  gadm_fx <- getExportedValue("geodata", "gadm")
  point_sf <- sf::st_as_sfc(siteInfo$data$geoCoord, crs = 4326) 
  point_sf <- sf::st_sf(geometry = point_sf)
  country <- gadm_fx(country = countryCode, level = 0, path = tempdir())
  countryMap <- sf::st_as_sf(country) %>%
    sf::st_simplify(dTolerance = 1000)
  countryBbox <- sf::st_bbox(countryMap)
  countryPlot_withSite <- countryPlot +
    ggplot2::geom_sf(data = point_sf, fill = color, color = borderColor, size = 1) +
    ggplot2::coord_sf(
      xlim = c(countryBbox["xmin"], countryBbox["xmax"]),
      ylim = c(countryBbox["ymin"], countryBbox["ymax"]),
      expand = FALSE
    )
  
  # Create the site map with expanded bounding box
  siteInfo_3857 <- sf::st_transform(siteInfo$data, crs = 3857)
  bbox <- sf::st_bbox(siteInfo_3857)
  xrange <- bbox["xmax"] - bbox["xmin"]
  yrange <- bbox["ymax"] - bbox["ymin"]
  bbox_expanded <- bbox
  bbox_expanded["xmin"] <- bbox["xmin"] - 0.05 * xrange
  bbox_expanded["xmax"] <- bbox["xmax"] + 0.05 * xrange
  bbox_expanded["ymin"] <- bbox["ymin"] - 0.05 * yrange
  bbox_expanded["ymax"] <- bbox["ymax"] + 0.05 * yrange
  # Load required functions from packages
  annotation_map_tile_fx <- getExportedValue("ggspatial", "annotation_map_tile")
  annotation_scale_fx <- getExportedValue("ggspatial", "annotation_scale")
  annotation_north_arrow_fx <- getExportedValue("ggspatial", "annotation_north_arrow")
  sitePlot <- ggplot2::ggplot() +
    annotation_map_tile_fx(zoomin = 0) +
    ggplot2::geom_sf(data = siteInfo_3857, fill = color, color = borderColor) +
    annotation_scale_fx(location = scale_location) +
    annotation_north_arrow_fx(location = arrow_location) +
    ggplot2::coord_sf(
      xlim = c(bbox_expanded["xmin"], bbox_expanded["xmax"]),
      ylim = c(bbox_expanded["ymin"], bbox_expanded["ymax"]),
      expand = FALSE
    ) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = siteInfo$data$title.x,
      subtitle = siteInfo$data$uri
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      plot.subtitle = ggplot2::element_text(hjust = 0.5)
    )
  # Define inset position
  inset_coords <- switch(
    inset_position,
    "tl" = list(x = 0.05, y = 0.65),
    "tr" = list(x = 0.65, y = 0.65),
    "bl" = list(x = 0.05, y = 0.05),
    "br" = list(x = 0.65, y = 0.05),
    list(x = 0.65, y = 0.05)  # default to "br"
  )
  
  # Combine the two maps using cowplot
  ggdraw_fx <- getExportedValue("cowplot", "ggdraw")
  draw_plot_fx <- getExportedValue("cowplot", "draw_plot")
  combinedPlot <- ggdraw_fx() +
    draw_plot_fx(
      sitePlot
    ) +
    draw_plot_fx(
      countryPlot_withSite,
      x = inset_coords$x, y = inset_coords$y,
      width = 0.3, height = 0.3
    )
  # Output
  print(combinedPlot)
}
