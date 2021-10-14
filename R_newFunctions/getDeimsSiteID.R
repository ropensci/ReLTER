getDeimsSiteID <- function(sites_filtered, idx = 1) {
  #' @title eLTER getDeimsSiteID function
  #' @description This function retrieves the DEIMS site ID
  #' from a filtered list (data.frame) of sites (i.e. using getSitesByCountry)
  #' where the user supplies an index number to choose which site from the list
  #' @param sites_filtered A `data.frame`. 
  #' @param idx An `integer` index in the data.frame for the desired site id
  #' @return the deims site id (url)
  #' @author Micha Silver, \email{silverm@post.bgu.ac.il}
  #' @examples
  #' deimsid <- getDeimsSitesID(sites_filtered, 3)
  #' deimsid

  if (length(sites_filtered$title) < idx) {
    print("Index is larger than filtered sites list.")
    return(NA)
  }
  site <- sites_filtered[idx,]
  deimsid = paste0(site$id$prefix, site$id$suffix)
  return(deimsid)
}
