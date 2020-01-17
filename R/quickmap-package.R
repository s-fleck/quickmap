#' @keywords internal
#' @importFrom magrittr %>%
#' @importFrom sf st_as_sf st_as_sfc
#' @export st_as_sf st_as_sfc
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL




.onLoad <- function(...) {
  options(qmap.providers = c(
    "CartoDB.Positron",
    "OpenStreetMap",
    "Esri.WorldImagery",
    "OpenTopoMap"
  ))
}




EPSG_WGS84 <- 4326L
