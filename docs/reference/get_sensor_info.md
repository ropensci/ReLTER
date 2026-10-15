# Obtain the information about of an eLTER sensor.

**\[deprecated\]** This function obtains the information about of an
eLTER sensor (e.g.
<https://deims.org/sensors/3845475c-4aec-4dd7-83b4-0ab6ba95db35>)
provided in [DEIMS-SDR catalogue](https://deims.org/).

## Usage

``` r
get_sensor_info(sensorid, show_map = FALSE)
```

## Arguments

- sensorid:

  A `character`. It is the DEIMS ID of sensor make from DEIMS-SDR
  website. DEIMS ID information
  [here](https://deims.org/docs/deimsid.html). The DEIMS.iD of sensor is
  the URL for the sensor page.

- show_map:

  A `boolean`. If TRUE a Leaflet map with occurrences is shown. Default
  FALSE.

## Value

The output of the function is a `list` with two elements:

- `map` A Leaflet map with the sensor location, if requested with
  `show_map`.

- `data` A `data.frame` with the information about the sensor.

## The function output

![Map of position of the "Climate Station B3"
sensor](figures/get_sensor_info_fig.png)

## Author

Alessandro Oggioni, phD (2020) <oggioni.a@irea.cnr.it>

## Examples

``` r
if (FALSE) { # \dontrun{
# print the map of the sensor
sensor_B3 <- get_sensor_info(
  sensorid =
    "https://deims.org/sensors/3845475c-4aec-4dd7-83b4-0ab6ba95db35",
  show_map = TRUE
)
sensor_B3

# only table of sensor information
Licor <- get_sensor_info(
  sensorid =
    "https://deims.org/sensors/4a7ad644-f2e7-4224-965b-ec5ef5365655",
  show_map = FALSE
)
Licor

# Moldaenke FluoroProbe sensor
sensor_FP <- get_sensor_info(
  sensorid = "https://deims.org/sensors/82635223-a4f4-498c-b283-9c95999d9d2f",
  show_map = FALSE
)
sensor_FP
} # }
```
