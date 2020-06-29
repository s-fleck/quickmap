#' Smartly convert an object to a simple features data frame
#'
#' Converts \R objects to [sf::sf] objects, but supports a wider
#' range of input data than [sf::st_as_sf].
#'
#' @param x any \R object
#' @param ... ignored
#'
#' @return an [sf::sf] data.frame
#' @export
#'
#' @examples
#' smart_as_sf(data.frame(lat = c(1,2,3), longitude = c(3,4,5)))
#' smart_as_sf(c(1, 2))
smart_as_sf <- function(x, ...){
  UseMethod("smart_as_sf")
}




#' @rdname smart_as_sf
#' @export
smart_as_sf.default <- function(
  x,
  ...
){
  res <- try(sf::st_as_sf(x, ...), silent = TRUE)

  if (inherits(res, "sf"))
    return(res)

  res <- try(sf::st_as_sf(sf::st_as_sfc(x)), silent = TRUE) # for example for bbox

  if (inherits(res, "sf"))
    return(res)

  res <- try(sf::st_as_sf(as_coord_matrix(x)), silent = TRUE)

  if (inherits(res, "sf"))
    return(res)

  stop(errorCondition(sprintf("cannot convert %s to sf", class_fmt(x)), class = "value_error"))
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
