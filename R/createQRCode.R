#' @title eLTER createQRCode function
#' @description This function allows to obtain the QRCode of any DEIMS-SDR
#' entities of the eLTER.
#' @param deimsid A `character`. It is the DEIMS iD of site make from
#' DEIMS-SDR website. More information about DEIMS iD in this
#' \href{https://deims.org/docs/deimsid.html}{page}.
#' @return The output of the function is a QRcode `image`.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @importFrom qrcode qrcode_gen
#' @export
#' @examples
#' createQRCode(deimsid = "https://deims.org/f30007c4-8a6e-4f11-ab87-569db54638fe")
#'
### function createQRCode
createQRCode <- function(deimsid) {
  qrcode::qrcode_gen(
    deimsid,
    ErrorCorrectionLevel = "L",
    dataOutput = FALSE,
    plotQRcode = TRUE,
    wColor = "White",
    bColor = "#1479BC",
    mask = 1,
    softLimitFlag = TRUE
  )
}
