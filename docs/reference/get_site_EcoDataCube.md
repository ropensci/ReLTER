# Acquire various raster layers from [EcoDataCube EU](https://ecodatacube.eu/) and crop to an eLTER site boundary.

**\[stable\]** Download and return a SpatRaster object containing the
requested dataset from [EcoDataCube EU](https://ecodatacube.eu/),
cropped to an eLTER site boundary, which is obtained from the DEIMS-SDR
API.

## Usage

``` r
get_site_EcoDataCube(
  deimsid,
  dataset = "",
  dataset_year = NA,
  dataset_month = NA,
  show_map = TRUE,
  output_dir = tempdir()
)
```

## Arguments

- deimsid:

  `string`. The DEIMS ID of the site from DEIMS-SDR website. DEIMS ID
  information [here](https://deims.org/docs/deimsid.html).

- dataset:

  `string` The requested dataset. One of: "CHELSA_precip", "clc_2017",
  "clc_2020", "crop_map", "DTM_30m", "MODIS_LST_day", "MODIS_LST_night",
  "NDVI_bimonthly", "NDVI_yearly", "NDWI_monthly", "soil_type" If
  dataset is NA or an empty string, then a list of the available
  EcoDataCube layers is printed. Default is "". (See Details for
  explanation of each dataset)

- dataset_year:

  `string` indicating which year to choose (for multi-year datasets)
  Entered as four digits (i.e. '2020')

- dataset_month:

  `string` indicating which month (for multi-month datasets). Entered as
  two digits. i.e. '02', '03', or '11' etc.

- show_map:

  `Bool` whether to show `leaflet` map of downloaded dataset. Default
  TRUE

- output_dir:

  `string` where to save the EcoDataCube dataset and style file. The
  \*.tif, \*.sld and \*.qml files will be saved to the specified
  directory, with filename as \<chosen_dataset\>\_.tif Default tempdir()

## Value

The function returns a SpatRaster object (from the `terra` package) of
the requested dataset, cropped to the site boundaries. The SpatRaster is
also saved as GeoTiff, along with the style files, in the chosen
`output_dir`.

## Details

Supported datasets from the EcoDataCube repository include:

|  |  |  |  |
|----|----|----|----|
| dataset | full name | date required | temporal extent |
| ————- | —————————————- | ————— | ——————————- |
| CHELSA_precip | CHELSA Monthly accumulated precipitation | yes | 2000-01-01 00:00:00 UTC–2019-06-30 00:00:00 UTC |
| clc_2017 | Corine Landcover (CLC+) 2017-2019 | no | 2017-01-01 00:00:00 UTC–2022-12-31 00:00:00 UTC |
| clc_2020 | Corine Landcover (CLC+) 2020-2022 | no | 2017-01-01 00:00:00 UTC–2022-12-31 00:00:00 UTC |
| crop_map | EUCROPMAP Pan-EU year 2022 | no | 2022-01-01 00:00:00 UTC–2022-12-31 00:00:00 UTC |
| DTM_30m | OpenLandMap Ensemble Digital Terrain Model | no | 2006-01-01 00:00:00 UTC–2015-12-31 00:00:00 UTC |
| MODIS_LST_day | MOD11A2 monthly land surface temp. (day) | yes | 2000-01-01 00:00:00 UTC–2021-12-31 00:00:00 UTC |
| MODIS_LST_night | MOD11A2 monthly land surface temp. (night) | yes | 2000-01-01 00:00:00 UTC–2021-12-31 00:00:00 UTC |
| NDVI_bimonthly | Cloud-free reconstructed Landsat bimonthly NDVI | yes | 2000-01-01 00:00:00 UTC–2022-12-31 00:00:00 UTC |
| NDVI_yearly | Cloud free reconstructed yearly Landsat NDVI | yes | 2000-01-01 00:00:00 UTC–2022-12-31 00:00:00 UTC |
| NDWI_bimonthly | Cloud free reconstructed bi-monthly NDWI (Gao) | yes | 2000-01-01 00:00:00 UTC–2022-12-31 00:00:00 UTC |
| soil_type | AI4SoilHealth: Soil type dominant class | no | 2000-01-01 00:00:00 UTC–2022-12-31 00:00:00 UTC |

All datasets are georeferenced to the EPSG:3035 coordinate reference
system.

## Author

Micha Silver, phD (2020) <silverm@post.bgu.ac.il>

Alessandro Oggioni, phD (2020) <oggioni.a@irea.cnr.it>

## Examples

``` r
# Example of TERENO Harsleben
deimsid = "https://deims.org/c945abe4-3d40-46d1-b5d0-33127c35c6ab"
# NDVI
harsleben_ndvi <- get_site_EcoDataCube(
  deimsid = deimsid,
  dataset = "NDVI_bimonthly",
  dataset_year = "2021",
  dataset_month = "06"
)

# EUCrop map
harsleben_crop <- get_site_EcoDataCube(
  deimsid = deimsid,
  dataset = "crop_map",
)
# MODIS Land Surface Temperature, 
# Kalkalpen National Park - Austria
if (FALSE) { # \dontrun{
deimsid = "https://deims.org/49515dda-1198-4013-8f43-c33e107af081"
kalkalpen_LST_day <- get_site_EcoDataCube(
  deimsid = deimsid,
  dataset = "MODIS_LST_day",
  dataset_year = "2010",
  dataset_month = "01"
)
} # }
```
