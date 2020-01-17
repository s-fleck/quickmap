#' @export
coord_matrix <- function(x){
  stopifnot(
    is.matrix(x),
    identical(ncol(x), 2L)
  )

  if (is.null(colnames(x)))
      colnames(x) <- c("lon", "lat")
  else
    assert(identical(colnames(x), c("lon", "lat")))

  class(x) <- union("coord_matrix", class(x))
  x
}





# as_coord_matrix ---------------------------------------------------------

#' Coerce R Object to Waypoints
#'
#'
#' @param x a `matrix`, a `data.frame`, a `sf::sfc_POINT` object
#'
#' If `x` is a `matrix` the columns are assumed to be in the order `lat, lon`,
#' except if the column names `lat, lon` are already present.
#'
#' If `x` is a `data.frame` it must contain a column
#' named either `"lat"` or `"latitude"` and a column named either `"lon"`,
#' `"lng"` or `"longitude"` (all case insenstive)
#'
#' @return `as_coord_matrix()` returns a `coord_matrix` object: A numeric `matrix`
#'   with the columns `lat`and `lon` (in that order)
#'
#' @aliases coord_matrix
#' @export
as_coord_matrix <- function(
  x,
  ...
){
  UseMethod("as_coord_matrix")
}




#' @export
as_coord_matrix.sf <- function(
  x,
  ...
){
  res <- sf::st_coordinates(x)
  colnames(res)[1:2] <- c("lon", "lat")
  coord_matrix(res[, c("lon", "lat"), drop = FALSE])
}




#' @export
as_coord_matrix.sfc_POINT <- function(x){
  assert_namespace("sf")

  empty <- matrix(c(NA_real_, NA_real_), ncol = 2)

  res <- vapply(
    x,
    FUN.VALUE = empty,
    function(.x) if (is.null(.x)) empty else sf::st_coordinates(.x)
  )

  res <- matrix(res, ncol = 2, byrow = TRUE)
  colnames(res) <- c("lon", "lat")
  coord_matrix(res)
}




#' @export
as_coord_matrix.matrix <- function(
  x,
  ...,
  loncol = guess_loncol(x),
  latcol = guess_latcol(x)
){
  assert(identical(ncol(x), 2L))
  colnames(x) <- c("lon", "lat")

  x <- x[, c("lon", "lat")]
  coord_matrix(x)
}




#' @export
as_coord_matrix.data.frame <- function(
  x,
  ...,
  loncol = guess_loncol(x),
  latcol = guess_latcol(x)
){
  if (any(vapply(x, inherits, logical(1), "sfc", USE.NAMES = FALSE))){
    return(as_coord_matrix(st_as_sf(x)))
  }

  coord_matrix(matrix(
    c(x[[loncol]], x[[latcol]]),
    ncol = 2,
    dimnames = list(NULL, c("lon", "lat"))
  ))
}




# as_sfc ------------------------------------------------------------------

#' @rdname as_coord_matrix
#' @return The `st_as_sf()` and `st_as_sf()` methwods for `coord_matrix`
#'   [sf::st_point()] objects.
#' @export
st_as_sfc.coord_matrix <- function(
  x
){
  points <- lapply(seq_len(nrow(x)), function(i) sf::st_point(x[i, c("lon", "lat")]))
  sf::st_sfc(points, crs = EPSG_WGS84)
}




#' @rdname as_coord_matrix
#' @export
st_as_sf.coord_matrix <- function(
  x
){
  sf::st_sf(geometry = st_as_sfc.coord_matrix(x), crs = EPSG_WGS84)
}




# utils -------------------------------------------------------------------

guess_loncol <- function(x){
  assert(ncol(x) >= 1, "Input must be an R object with columns (such as a data.frame or matrix)")
  cols <- colnames(x)

  if (!length(cols))
    return(1L)

  res <- which(tolower(cols) %in%  c("lon", "lng", "long", "longitude"))

  standardize_colpos(res, x, "longitude")
}




guess_latcol <- function(x){
  assert(ncol(x) >= 1, "Input must be an R object with columns (such as a data.frame or matrix)")
  cols <- colnames(x)

  if (!length(cols))
    return(2L)

  res <- which(tolower(cols) %in% c("lat", "latitude"))
  standardize_colpos(res, x, "latitude")
}



standardize_colpos <- function(
  pos,
  obj,
  coltype
){
  if (length(pos) < 1L){
    stop(cannotGuessColumnError(obj, coltype))

  } else if (length(pos) > 1L){
    warning(multipleCandidateColumnsFoundWarning(obj, pos, coltype))

  }

  pos[[1]]
}





# conditions --------------------------------------------------------------

cannotGuessColumnError <- function(obj, coltype){
  errorCondition(
    paste(
      "Input must contain appropriately named columns for", coltype,
      "and and longitude"
    ),
    class = "cannotGuessColumnError"
  )
}




multipleCandidateColumnsFoundWarning <- function(obj, pos, coltype){
  columns <- names(obj)[pos]
  warningCondition(
    paste(
      "Multiple possible latitude columns found: ",
      commalist(columns), ". Using ", backtick(columns[[1]]), "`."
    ),
    class = "multipleCandidateColumnsFoundWarning"
  )
}
