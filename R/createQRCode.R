#' @title eLTER createQRCode function
#' @description This function allows to obtain the QRCode of any DEIMS-SDR
#' entities of the eLTER.
#' @param deimsid A `character`. It is the DEIMS iD of site make from
#' DEIMS-SDR website. More information about DEIMS iD in this
#' \href{https://deims.org/docs/deimsid.html}{page}.
#' @return The output of the function is a QRcode `image` in a png
#' (saved in the workdir).
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @export
#' @examples
#' createQRCode(deimsid = "https://deims.org/f30007c4-8a6e-4f11-ab87-569db54638fie")
#'
### function createQRCode
createQRCode <- function(deimsid) {
  siteTitle <- ReLTER::getSiteGeneral(deimsid = deimsid) %>%
    dplyr::select(title) %>%
    as.character()
  outputImageQRCode <- paste0(
    "eLTER_qrCode_",
    gsub(" ", "_", siteTitle),
    ".png"
  )
  par(mar = c(0, 0, 0, 0))
  png(filename = paste0(outputImageQRCode))
  image(
    qrencoder::qrencode_raster(as.character(deimsid)),
    asp = 1,
    col = c("white", "#1479BC"),
    axes = FALSE,
    xlab = "",
    ylab = ""
  )
  dev.off()
}
