# Retrieve species occurrences within an eLTER site boundary

**\[stable\]** This function downloads species occurrence records from
[GBIF](https://www.gbif.org),
[iNaturalist](https://www.inaturalist.org/) and
[OBIS](https://obis.org/) via the
[spocc](https://docs.ropensci.org/spocc/) package (SPecies OCCurrence
data from across the web), and intersects them with the boundary of an
eLTER site retrieved from the [DEIMS-SDR API](https://deims.org/). Only
occurrences falling within the site polygon are returned, enriched with
eLTER site metadata.

iNaturalist observations are filtered to **research grade** only via the
`quality` parameter of
[`rinat::get_inat_obs()`](https://docs.ropensci.org/rinat/reference/get_inat_obs.html),
which is the quality level required by `ReLTER.inatEnrich` enrichment
functions. Note that the iNaturalist API has a hard limit of 10,000
observations per query. For sites with more than 10,000 research grade
observations in the bounding box, consider using GBIF with the
iNaturalist dataset key (`"50c9509d-22c7-4a22-a47d-8c48425ef4a7"`) as an
alternative, which indexes all iNaturalist research grade observations
and has no such limit (though with a synchronisation lag of a few days).

## Usage

``` r
get_site_speciesOccurrences(
  deimsid,
  list_DS,
  show_map = TRUE,
  limit = 500,
  exclude_inat_from_gbif = TRUE
)
```

## Arguments

- deimsid:

  A `character`. The DEIMS ID of the site from DEIMS-SDR website. DEIMS
  ID information [here](https://deims.org/docs/deimsid.html).

- list_DS:

  A `character` vector. Data sources to query; any combination of
  `"gbif"`, `"inat"`, and/or `"obis"`.

- show_map:

  A `boolean`. If `TRUE` the `leaflet` map is both printed and returned
  in the output list as `$map`. If `FALSE` the map is not printed but is
  still built and returned in `$map` for later use. Default `FALSE`.

- limit:

  A `numeric`. Maximum number of records to download per source. Default
  `500`. Note that when querying for many species the total number of
  records can be large and slow to download; start with a small value
  (e.g. `10`) to verify results before increasing. For iNaturalist, the
  maximum allowed by the API is `10000`.

- exclude_inat_from_gbif:

  A `boolean`. If `TRUE`, and both `"gbif"` and `"inat"` are in
  `list_DS`, records originating from iNaturalist are removed from the
  GBIF results to avoid duplicates. Default `TRUE`.

## Value

A `list` with one `sf` element per data source (named `gbif`, `inat`,
`obis`) containing only occurrences that fall within the site boundary,
and a `map` element with a `leaflet` object. Each `sf` element contains
the occurrence geometry and the following eLTER site metadata fields
(prefixed with `eLTER_`): `title`, `uri`, `created`, `changed`,
`geoCoord`, `country`, `geoElev.avg`, `geoElev.min`, `geoElev.max`,
`biogeographicalRegion`, `biome`, `ecosystemType`, `eunisHabitat`,
`landforms`, `geoBonBiome`, `geology`, `hydrology`, `soils`,
`vegetation`, `size.value`. If no occurrences are found within the
boundary for a given source, that source is omitted from the list and an
informative message is printed. Returns `invisible(NULL)` if the site
has no boundary or if no occurrences are found for any source.

## Note

iNaturalist observations are downloaded as research grade only. This is
consistent with the quality level required by the `ReLTER.inatEnrich`
enrichment pipeline (`add_iucn_to_occ()`, `add_nativeness_to_occ()`,
`add_eunis_legal_to_occ()`).

The iNaturalist API enforces a hard limit of 10,000 observations per
query. Spatial filtering is performed on a bounding box (not the exact
site polygon); the exact polygon intersection is applied afterwards via
[`sf::st_intersection()`](https://r-spatial.github.io/sf/reference/geos_binary_ops.html),
so the number of records returned may be slightly higher than those
visible in the corresponding iNaturalist project for the site.

## The function output

![Map of occurrences acquired from OBIS in the marine site Gulf of
Venice](figures/get_site_speciesOccurrences_fig.png)

## See also

[`spocc::occ()`](https://docs.ropensci.org/spocc/reference/occ.html)

[`spocc::obis_search()`](https://docs.ropensci.org/spocc/reference/obis_search.html)

[`RColorBrewer::brewer.pal()`](https://rdrr.io/pkg/RColorBrewer/man/ColorBrewer.html)

[`get_site_info()`](https://docs.ropensci.org/ReLTER/reference/get_site_info.md)

## Author

Alessandro Oggioni, PhD (2020) <oggioni.a@irea.cnr.it>

Paolo Tagliolato, PhD (2020) <tagliolato.p@irea.cnr.it>

Martina Zilioli <zilioli.m@irea.cnr.it>

## Examples

``` r
if (FALSE) { # \dontrun{
# Terrestrial site: Saldur River Catchment (GBIF and iNaturalist,
# excluding records sourced from iNaturalist in GBIF)
occ_SRC <- get_site_speciesOccurrences(
  deimsid = "https://deims.org/97ff6180-e5d1-45f2-a559-8a7872eb26b1",
  list_DS = c("gbif", "inat"),
  show_map = TRUE,
  limit = 50,
  exclude_inat_from_gbif = TRUE
)
occ_SRC

# Terrestrial site: Gran Paradiso National Park (only GBIF,
# excluding records sourced from iNaturalist)
occ_GPNP <- get_site_speciesOccurrences(
  deimsid = "https://deims.org/15c3e841-8494-42d2-a44e-c49a0ff25946",
  list_DS = "gbif",
  show_map = TRUE,
  limit = 50,
  exclude_inat_from_gbif = TRUE
)
occ_GPNP

# Terrestrial site: Tenuta di Castelporziano (iNaturalist only,
# research grade, up to 10000 records -- the iNaturalist API hard limit)
occ_CP <- get_site_speciesOccurrences(
  deimsid = "https://deims.org/0d2269d3-5423-4939-a30d-077c8bc38b03",
  list_DS = "inat",
  show_map = TRUE,
  limit = 10000
)
occ_CP$inat

# Marine site: Gulf of Venice (OBIS only)
occ_GoV <- get_site_speciesOccurrences(
  deimsid = "https://deims.org/758087d7-231f-4f07-bd7e-6922e0c283fd",
  list_DS = "obis",
  show_map = FALSE,
  limit = 10
)
occ_GoV

# Marine site: Gulf of Venice (all sources excluding records of GBIF
# sourced from iNaturalist)
occ_GoV_all <- get_site_speciesOccurrences(
  deimsid = "https://deims.org/758087d7-231f-4f07-bd7e-6922e0c283fd",
  list_DS = c("gbif", "inat", "obis"),
  show_map = TRUE,
  limit = 10,
  exclude_inat_from_gbif = TRUE
)
occ_GoV_all$obis
occ_GoV_all$map
} # }
```
