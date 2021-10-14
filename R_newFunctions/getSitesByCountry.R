getSitesByCountry <- function(country_name) {
  #' @title eLTER getSitesByCountry function
  #' @description This function allows the user to retrieve a list of sites
  #' associated with a country name. The user supplied country_name
  #' must appear in the 'title' string of the site metadata
  #' @param country_name A `character`. Partial names are also matched.
  #' However the partial string should be unique.
  #' i.e. "United" can match "United States" and "United Kingdom"
  #' @return A `data.frame` of the sites within the given country
  #' @author Micha Silver, phD (2020) \email{silverm@post.bgu.ac.il}
  #' @import jsonlite httr tibble sf leaflet
  #' @export
  #' @examples
  #' sites_filtered <- getSitesByCountry("Austri")
  #' (Austria, not Australia)
  #' sites_filtered 

  deims_url = "https://deims.org/api/sites"
  sites_list <-  rawToChar(GET(url = deims_url)$content)
  sites_df <- as.data.frame(fromJSON(sites_list))
  sites_filtered <- sites_df[grep(x = sites_df$title,
                                  pattern = country_name,
                                  ignore.case = TRUE),]
  #print(sites_filtered$title)
  return(sites_filtered)
}
