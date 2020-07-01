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

#' Coerce an R object to a matrix of coordinates
#'
#' @description
#' A `coord_matrix` is a `matrix` with two columns named `"lon"` and `"lat"` to
#' represent spatial point data. They are used as an intermediary when
#' converting some \R objects to [sf::sf()] objects.
#'
#' `as_coord_matrix()` can smartly convert a range of \R objects to
#' `coord_matrix`. If you are a package developer and want to add support
#' for smartmap to your package without having to depend on the heavy \pkg{sf}
#' package, it is enough to provide an `as_coord_matrix()` method.
#'
#' @inheritParams smart_as_sf
#'
#' @return `as_coord_matrix()` returns a `coord_matrix` object: A numeric
#'   `matrix` with the columns `"lon"`and `"lat"` (in that order)
#'
#' @param ... passed on to methods
#'
#' @aliases coord_matrix
#' @seealso \url{https://stackoverflow.com/questions/7309121/preferred-order-of-writing-latitude-longitude-tuples}
#' @export
as_coord_matrix <- function(
  x,
  ...
){
  UseMethod("as_coord_matrix")
}




#' @rdname as_coord_matrix
#' @export
as_coord_matrix.default <- function(
  x,
  ...
){
  tryCatch(
    as_coord_matrix(smart_as_sf(x)),
    error = function(e) stop(objectNotSupportedError(
        message = paste("don't know how convert objects of type", class_fmt(x), "to coord_matrix")
    ))
  )
}




#' @rdname as_coord_matrix
#' @export
as_coord_matrix.numeric <- function(
  x,
  ...
){
  assert(
    identical(length(x), 2L),
    objectNotSupportedError("only numeric vectors of length 2 can be coerced to coordinate matrices")
  )
  as_coord_matrix(matrix(x, ncol = 2, dimnames = list(NULL, names(x))))
}




#' @rdname as_coord_matrix
#' @export
as_coord_matrix.sf <- function(
  x,
  ...
){
  res <- sf::st_coordinates(x)
  colnames(res)[1:2] <- c("lon", "lat")
  coord_matrix(res[, c("lon", "lat"), drop = FALSE])
}




#' @rdname as_coord_matrix
#' @export
as_coord_matrix.sfc_POINT <- function(x, ...){
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




#' @rdname as_coord_matrix
#' @param loncol,latcol `character` scalars. Names of the columns of
#'   `x` containing longitude and latitude. The default trying guessing the
#'   columns.
#' @export
as_coord_matrix.matrix <- function(
  x,
  ...,
  loncol = guess_loncol(x),
  latcol = guess_latcol(x)
){
  assert(identical(ncol(x), 2L) || length(names(x)) > 0)
  force(loncol)
  force(latcol)

  if (!length(colnames(x))){
    colnames(x) <- paste0("V", seq_len(ncol(x)))
  }

  colnames(x)[[loncol]] <- "lon"
  colnames(x)[[latcol]] <- "lat"

  x <- x[, c("lon", "lat"), drop = FALSE]
  coord_matrix(x)
}




#' @rdname as_coord_matrix
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




# as_sf ------------------------------------------------------------------

#' Convert coordinate matrices to sf objects
#'
#' @seealso [sf::st_as_sf()]
#' @name st_as_sf
#' @importFrom sf st_as_sf
#' @export st_as_sf
NULL




#' @rdname st_as_sf
#' @param x a [coord_matrix]
#' @param ... ignored
#' @return an [sf::sf()] object with an `sfc_POINT`-geometry column
#' @export
st_as_sf.coord_matrix <- function(
  x,
  ...
){
  sf::st_sf(geometry = st_as_sfc.coord_matrix(x), crs = EPSG_WGS84)
}



#' Convert coordinate matrices to sfc objects
#'
#' @seealso [sf::st_as_sfc()]
#' @name st_as_sfc
#' @importFrom sf st_as_sfc
#' @export st_as_sfc
NULL




#' @rdname st_as_sfc
#' @param x a [coord_matrix]
#' @param ... ignored
#' @return an [sf::sfc()] object of subclass `sfc_POINT`
#' @export
st_as_sfc.coord_matrix <- function(
  x,
  ...
){
  points <- lapply(seq_len(nrow(x)), function(i) sf::st_point(x[i, c("lon", "lat")]))
  sf::st_sfc(points, crs = EPSG_WGS84)
}




# utils -------------------------------------------------------------------

guess_loncol <- function(x){
  assert(ncol(x) >= 1, "Input must be an R object with columns (such as a data.frame or matrix)")
  cols <- colnames(x)

  if (!length(cols))
    return(1L)

  res <- which(tolower(cols) %in%  c("lon", "lng", "long", "longitude", "x"))

  standardize_colpos(res, x, "longitude")
}




guess_latcol <- function(x){
  assert(ncol(x) >= 1, "Input must be an R object with columns (such as a data.frame or matrix)")
  cols <- colnames(x)

  if (!length(cols))
    return(2L)

  res <- which(tolower(cols) %in% c("lat", "latitude", "y"))
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

objectNotSupportedError <- function(
  obj,
  message = paste("object of type", class_fmt(obj), "is not supported")
){
  errorCondition(
    message = message,
    class = "objectNotSupportedError"
  )
}




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
