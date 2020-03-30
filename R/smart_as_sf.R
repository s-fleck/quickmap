#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
smart_as_sf <- function(x, ...){
  UseMethod("smart_as_sf")
}



#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
smart_as_sf.default <- function(
  x,
  ...
){
  sf::st_as_sf(x, ...)
}





#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
smart_as_sf.data.frame <- function(
  x,
  ...
){
  coords <- sf::st_as_sf(as_coord_matrix(x))
  sf::st_geometry(x) <- sf::st_geometry(coords)
  x
}
