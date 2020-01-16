#' gisco.
#'
#' @import assertthat
#' @importFrom data.table data.table
#' @importFrom magrittr %>%
#' @name gisco
#' @docType package
NULL




utils::globalVariables(c("lon", "lat", "lab"))


.onLoad <- function(...) {
  options(qmap.providers = c(
    "CartoDB.Positron",
    "OpenStreetMap",
    "Esri.WorldImagery",
    "OpenTopoMap"
  ))
}



EPSG_WGS84 <- 4326L
