#' @title Return a QR code image of any provided DEIMS.iD (e.g. dataset,
#' site, activity).
#' @description This function obtain the QRCode of any DEIMS-SDR
#' entities of the eLTER.
#' @param deimsid a `character`. The DEIMS ID of entities from
#' DEIMS-SDR website. More information about DEIMS iD in this
#' \href{https://deims.org/docs/deimsid.html}{page}.
#' @param do_plot a `boolean`. Plot the computed QRCode. Default FALSE.
#' @return The QR code as a logical matrix with "qr_code" class.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @importFrom qrcode qr_code
#' @export
#' @examples
#' qrcode <- produce_site_qrcode(
#'   deimsid = "https://deims.org/f30007c4-8a6e-4f11-ab87-569db54638fe"
#' )
#' plot(qrcode)
#'
#' a <- produce_site_qrcode(
#'   deimsid = "https://deims.org/f30007c4-8a6e-4f11-ab87-569db54638fe",
#'   do_plot = TRUE
#' )
#'
### function produce_site_qrcode
produce_site_qrcode <- function(deimsid, do_plot = FALSE) {
  res <- qrcode::qr_code(deimsid, ecl = "L")
  if (do_plot) {
    a <- res %>%
      plot(col = c("White", "#1479BC"))
    a
  }
  return(res)
}
