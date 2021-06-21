backtick <- function(x){
  paste0("`", x, "`")
}




#' Convert sf objects to normal data.frames with longitude and latitude colu,ns
#'
#' @param x an `sf::sf` object
#' @param coord_names `character` vector of length `2`. The names of the new
#'   that should contain the coordinates from the geometry column of `x`.
#'
#' @return a `data.frame`
#' @export
unsf <- function(
  x,
  coord_names = c("lon", "lat")
){
  assert(all(sf::st_geometry_type(x) == "POINT"))
  coords <- sf::st_coordinates(x)
  colnames(coords) <- coord_names
  res <- cbind(x, coords)
  sf::st_geometry(res) <- NULL
  res
}
