backtick <- function(x){
  paste0("`", x, "`")
}




#' Title
#'
#' @param x
#' @param coord_names
#'
#' @return
#' @export
#'
#' @examples
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
