#' View spatial objects as interactive leaflet maps
#'
#' Can be used to preview spatial \R objects
#'
#' @param x any input supported by [smart_as_sf()] or a [leaflet][leaflet::leaflet()] map
#'   * a `matrix`: Either a matrix with named `longitude` and `latitude` columns
#'     or an unnamed two column matrix containing longitude and latitude
#'     (in that order)
#'   * a `data.frame` with named `longitude` and `latitude` columns
#'   * an [sf::sfc_POINT][sf::sfc] object
#'   * a named or unnamed `numeric` vector of length 2 containing a single
#'     longitude-latitude coordinate pair
#'   * a `character` scalar path or URL to a shapefile or zipped shapefile
#'   * a `leaflet` map
#'
#' @param tools `logical` scalar. If `TRUE` show additional tools on the
#'   resulting map (such as a ruler and the ability to switch between several
#'   background tiles)
#'
#' @param provider `character` vector. Name of one or several valid providers
#'   for [leaflet::addProviderTiles()]. If `tools == TRUE` you will be able
#'   to switch interactively between all supplied providers on the returned
#'   leaflet map, if `tools == FALSE` only the first provider will be used.
#'
#' @param ... passed on to methods.
#'
#' @return a [leaflet::leaflet] object
#' @export
#'
#' @examples
#' wp <- matrix(
#'   c(16.419684, 48.186065,
#'     16.373894, 48.207853,
#'     16.285887, 48.083053),
#'   byrow = TRUE,
#'   ncol = 2
#' )
#'
#' \donttest{
#' smap(wp)
#' smap(c(16.419684, 48.186065))
#' }
smap <- function(
  x,
  ...,
  tools = TRUE,
  provider = getOption("smap.providers", "OpenStreetMap")
){
  assert(is_scalar_bool(tools))
  assert(is.character(provider))
  UseMethod("smap")
}




#' @rdname smap
#' @export
smap.leaflet <- function(
  x,
  ...,
  tools = TRUE,
  provider = getOption("smap.providers", "OpenStreetMap")
){
  if (isTRUE(tools)){
    x <-
      leaflet::addMeasure(
        x,
        primaryLengthUnit = "kilometers",
        secondaryLengthUnit = "meters",
        primaryAreaUnit = "sqmeters",
        secondaryAreaUnit = NULL
      )

    for (p in provider){
      x <- leaflet::addProviderTiles(
        x,
        provider = p,
        group = p
      )
    }

    x <- leaflet::addLayersControl(
      x,
      baseGroups = provider
    )

  } else {
    x <- leaflet::addProviderTiles(x, provider[[1]])
  }

  x
}




#' @rdname smap
#' @export
smap.sf <- function(
  x,
  ...,
  tools = TRUE,
  provider = getOption("smap.providers", "OpenStreetMap")
){

  # prep input
    x <- sf::st_zm(x)
    rem <- vapply(sf::st_geometry(x), sf::st_is_empty, logical(1))

    if (any(rem)){
      warning("removed ", sum(rem), " rows with empty geometry", call. = FALSE)
      x <- x[!rem, ]
    }

    if (is.na(sf::st_crs(x))){
      warning("no CRS set for `", preview_object(x), "`: trying WGS84 (EPSG:4326)", call. = FALSE)
      sf::st_crs(x) <- EPSG_WGS84
    }

  # build map
    gt <- unique(as.character(sf::st_geometry_type(x)))

    addFun <- switch(  # nolint
      gt,
     "MULTILINESTRING" = leaflet::addPolylines,
     "LINESTRING"      = leaflet::addPolylines,
     "POINT"           = leaflet::addCircleMarkers,
     "MULTIPOINT"      = leaflet::addCircleMarkers,
     "MULTIPOLYGON"    = leaflet::addPolygons,
     "POLYGON"         = leaflet::addPolygons,
     stop("'", gt, "' is not a geometry type supported by smartmap", call. = FALSE)
    )

  leaflet::leaflet() %>%
    addFun(data = sf::st_transform(sf::st_zm(x), EPSG_WGS84)) %>%
    smap(tools = tools, provider = provider)
}




#' @rdname smap
#' @param labels an optional `character` vector of popup labels
#' @export
smap.default <- function(
  x,
  labels = NULL,
  ...,
  tools = TRUE,
  provider = getOption("smap.providers", "OpenStreetMap")
){
  dd <- try(smart_as_sf(x), silent = TRUE)

  if (is_try_error(dd))
    dd <- try(st_as_sf(as_coord_matrix(x)), silent = TRUE)

  if (is_try_error(dd)){
    stop(objectNotSupportedError(
      message = paste("don't know how to generate a map for object of class", class_fmt(x))
    ))
  }

  smap(dd, tools = tools, provider = provider)
}




#' @export
smap.data.frame <- function(
  x,
  labels = NULL,
  ...,
  tools = TRUE,
  provider = getOption("smap.providers", "OpenStreetMap")
){
  if (any(vapply(x, inherits, logical(1), "sfc", USE.NAMES = FALSE))){
    smap(st_as_sf(x), tools = tools, provider = provider)

  } else {
    NextMethod()
  }
}




#' @rdname smap
#' @export
smap.sfc <- smap.sf




#' @rdname smap
#' @export
smap.sfg <- function(
  x,
  ...,
  tools = TRUE,
  provider = getOption("smap.providers", "OpenStreetMap")
){
  smap(
    sf::st_sf(geometry = sf::st_sfc(x)),
    tools = tools, provider = provider
  )
}




#' @rdname smap
#' @export
smap.matrix <- function(
  x,
  labels = NULL,
  ...,
  tools = TRUE,
  provider = getOption("smap.providers", "OpenStreetMap")
){
  x   <- as_coord_matrix(x)
  res <- st_as_sfc(x)

  if (!is.null(labels)){
    smap(res, tools = tools, provider = provider) %>%
      leaflet::addPopups(lat = x[, "lat"], lng = x[, "lon"], popup = as.character(labels))

  } else {
    smap(res, tools = tools, provider = provider)
  }
}
