#' Smartly convert an object to a simple feautres data frame
#'
#' @param x any \R object
#' @param ... ignored
#'
#' @return an [sf::sf] data.frame
#' @export
#'
#' @examples
#' smart_as_sf(data.frame(lat = c(1,2,3), longitude = c(3,4,5)))
smart_as_sf <- function(x, ...){
  UseMethod("smart_as_sf")
}




#' @rdname smart_as_sf
#' @export
smart_as_sf.default <- function(
  x,
  ...
){
  tryCatch(
    sf::st_as_sf(x, ...),
    error = function(e)
      sf::st_as_sf(sf::st_as_sfc(x))  # for example for bbox
  )
}




#' @rdname smart_as_sf
#' @export
smart_as_sf.data.frame <- function(
  x,
  ...
){
  coords <- sf::st_as_sf(as_coord_matrix(x))
  sf::st_geometry(x) <- sf::st_geometry(coords)
  x
}
