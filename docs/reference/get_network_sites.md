# Retrieve a list of sites in an eLTER Network.

**\[stable\]** This function return a spatial point vector object
including title, date late updated, URI, and coordinates, stored in
[DEIMS-SDR catalogue](https://deims.org/), of all the eLTER sites
belonging to an eLTER Network (e.g. [LTER- Italy
network](https://deims.org/networks/7fef6b73-e5cb-4cd2-b438-ed32eb1504b3)).

## Usage

``` r
get_network_sites(networkDEIMSID)
```

## Arguments

- networkDEIMSID:

  A `character`. The DEIMS ID of the network from DEIMS-SDR website.
  DEIMS ID information [here](https://deims.org/docs/deimsid.html) and
  Complete list of networks [here](https://deims.org/networks). The
  DEIMS ID of network is the URL for the network page.

## Value

The output of the function is a point vector of `sf` class (package sf)
of the network's sites.

## Author

Alessandro Oggioni, phD (2020) <oggioni.a@irea.cnr.it>

## Examples

``` r
if (FALSE) { # \dontrun{
# The sites of LTER-Italy network
listSites <- get_network_sites(
  networkDEIMSID =
    "https://deims.org/network/7fef6b73-e5cb-4cd2-b438-ed32eb1504b3"
)
listSites

# The sites of LTER Europe network
euSites <- get_network_sites(
  networkDEIMSID =
    "https://deims.org/networks/4742ffca-65ac-4aae-815f-83738500a1fc"
)
euSites
} # }
```
