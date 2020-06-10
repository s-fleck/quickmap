#' @keywords internal
#' @importFrom magrittr %>%
"_PACKAGE"




# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL




.onLoad <- function(...) {
  options(smap.providers = c(
    "CartoDB.Positron",
    "OpenStreetMap",
    "Esri.WorldImagery",
    "OpenTopoMap"
  ))
}




EPSG_WGS84 <- 4326L
