#' @export
waypoints <- function(x){
  stopifnot(
    is.matrix(x),
    identical(ncol(x), 2L)
  )

  if (is.null(colnames(x)))
      colnames(x) <- c("lat", "lon")
  else
    assert(identical(colnames(x), c("lat", "lon")))

  class(x) <- union("waypoints", class(x))
  x
}




#' Coerce R Object to Waypoints for gisco::gc_route
#'
#' Create valid inputs for [gc_route()]
#'
#' @param x a `matrix`, a `data.frame`, a `sf::sfc_POINT` object, or a
#' `character` string in the form `"lon1,lat1;lon2,lat2;..."`.
#'
#' If `x` is a `matrix` the columns are assumed to be in the order `lat, lon`,
#' except if the column names `lat, lon` are already present.
#'
#' If `x` is a `data.frame` it must contain a column
#' named either `"lat"` or `"latitude"` and a column named either `"lon"`,
#' `"lng"` or `"longitude"` (all case insenstive)
#'
#' @return `as_waypoints()` returns a `waypoints` object: A numeric `matrix`
#'   with the columns `lat`and `lon` (in that order)
#'
#' @seealso [waypoints_to_string]
#' @aliases waypoints
#' @export
#'
#' @examples
#' as_waypoints("16.373831,48.208171;14.437802,50.07554;11.581979,48.135129")
as_waypoints <- function(x){
  UseMethod("as_waypoints")
}




#' @export
as_waypoints.sf <- function(x){
  assert_namespace("sf")
  res <- sf::st_coordinates(x)
  colnames(res)[1:2] <- c("lon", "lat")
  waypoints(res[, c("lat", "lon"), drop = FALSE])
}




#' @export
as_waypoints.sfc_POINT <- function(x){
  assert_namespace("sf")

  empty <- matrix(c(NA_real_, NA_real_), ncol = 2)

  res <- vapply(
    x,
    FUN.VALUE = empty,
    function(.x)  if (is.null(.x)) empty else sf::st_coordinates(.x)
  )
  res <- matrix(res, ncol = 2, byrow = TRUE)
  colnames(res) <- c("lon", "lat")

  waypoints(res[, c("lat", "lon"), drop = FALSE])
}




#' @export
as_waypoints.matrix <- function(x){
  assert(identical(ncol(x), 2L))

  if (identical(colnames(x), c("lon", "lat"))){
    x <- x[, c("lat", "lon")]
  }

  waypoints(x)
}




#' @export
as_waypoints.data.frame <- function(
  x
){
  if (any(vapply(x, inherits, logical(1), "sfc", USE.NAMES = FALSE))){
    return(as_waypoints(sf::st_as_sf(x)))
  }

  col_names <- tolower(names(x))

  lat_col <- which(col_names %in% c("lat", "latitude"))
  lon_col <- which(col_names %in% c("lon", "lng", "longitude"))


  if (length(lat_col) < 1 || length(lon_col) < 1){
    stop(
      "If `x` is a data.frame, it must contain appropriately named columns ",
      "for latitude (lat, latitude) and longitude (lon, lng, longitude)",
      call. = FALSE
    )

  } else if (length(lat_col) > 1){
    stop(
      "Multiple possible latitude columns identified: ",
      paste(names(x)[lat_col], collapse = ", "), call. = FALSE
    )

  } else if (length(lon_col) > 1) {
    stop(
      "Multiple possible longitude columns identified: ",
      paste(names(x)[lon_col], collapse = ", "), call. = FALSE
    )
  }


  lat_name <- names(x)[[lat_col]]
  lon_name <- names(x)[[lon_col]]

  dd <- data.frame(
    lat = x[[lat_name]],
    lon = x[[lon_name]]
  )

  assert(identical(ncol(dd), 2L))
  waypoints(as.matrix(dd))
}




# as_sfc ------------------------------------------------------------------

#' @rdname as_waypoints
#' @return The `st_as_sf()` and `st_as_sf()` methwods for `waypoints`
#'   [sf::st_point()] objects.
#' @export
st_as_sfc.waypoints <- function(
  x
){
  points <- lapply(seq_len(nrow(x)), function(i) sf::st_point(x[i, c("lon", "lat")]))
  sf::st_sfc(points, crs = EPSG_WGS84)
}



#' @rdname as_waypoints
#' @export
st_as_sf.waypoints <- function(
  x
){
  sf::st_sf(geometry = st_as_sfc.waypoints(x), crs = EPSG_WGS84)
}
