#' Smartly convert an object to a simple features data frame
#'
#' Converts \R objects to [sf::sf] objects, but supports a wider
#' range of input data than [sf::st_as_sf].
#'
#' @param x any of the following:
#'   * a `matrix`: Either a matrix with named `longitude` and `latitude` columns
#'     or an unnamed two column matrix containing longitude and latitude
#'     (in that order)
#'   * a `data.frame` with named `longitude` and `latitude` columns
#'   * an [sf::sfc_POINT][sf::sfc] object
#'   * a named or unnamed `numeric` vector of length 2 containing a single
#'     longitude-latitude coordinate pair
#'   * a `character` scalar path or URL to a shapefile or zipped shapefile
#'
#' @note `smart_as_sf.default()` looks if an [sf::st_as_sf()],
#'   [sf::st_as_sfc()] or [smartmap::as_coord_matrix()] method exists for `x` (in
#'   that order). If you are a package developer and want to support smartmap
#'   for a custom S3 class in your package, it is enough to provide one of these
#'   methods.
#'
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





#' @rdname smart_as_sf
#' @export
smart_as_sf.character <- function(
  x,
  ...
){
  assert(
    is_scalar(x),
    errorCondition(
      "If `x` is a character vector, it must be a single file path or URL.",
      class = "value_error"
    )
  )

  infile <- x

  if (is_url(infile)){
    tf <- paste0(tempfile(), basename(x))
    on.exit(unlink(tf))
    utils::download.file(x, destfile = tf)
    infile <- tf
  }

  if (is_zipfile(infile)){
    unzipped <- handle_zipfile(infile)
    on.exit(unlink(unzipped, recursive = TRUE), add = TRUE)
    sel <- grep("\\.shp$", unzipped)

    if (identical(length(sel), 1L)){
      infile <- unzipped[[sel]]

    } else if (length(sel) < 1) {
      stop(errorCondition(
        sprintf("'%s' does not contain a `.shp` file.", infile),
        class = "value_error"
      ))
    } else {
      stop(errorCondition(
        sprintf("'%s' contains more than one `.shp` file.", infile),
        class = "value_error"
      ))
    }
  }

  if (is_shpfile(infile)){
    sf::read_sf(infile)

  } else {
    stop(errorCondition(
      "If `x` is a character vector, it must be a single file path or URL.",
      class = "value_error"
    ))
  }
}








handle_zipfile <- function(x){
  tdir <- tempfile()
  dir.create(tdir)
  res <- utils::unzip(x, exdir = tdir)
  on.exit({
    unlink(res,  recursive = TRUE)
    unlink(tdir, recursive = TRUE)
  }, add = TRUE)

  on.exit(NULL)
  c(tdir, res)
}




is_url <- function(x){
  is_scalar_character(x) && grepl("^https{0,1}://", x)
}




is_zipfile <- function(x){
  is_scalar_character(x) && grepl("\\.zip$", x, ignore.case = TRUE)
}




is_shpfile <- function(x){
  is_scalar_character(x) && grepl("\\.shp$", x, ignore.case = TRUE)
}


