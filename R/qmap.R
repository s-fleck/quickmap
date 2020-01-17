#' View spatial objects as interactive leaflet maps
#'
#' Can be used to preview spatial \R objects
#'
#' @note `quickmap.default()` looks if an [sf::st_as_sf()],
#'   [sf::st_as_sfc()] or [qmap::as_coord_matrix()] method exists for `x` (in
#'   that order). If you are a package developer and want to support quickmap
#'   for a custom S3 class in your package, it is enough to provide one of these
#'   methods.
#'
#' @param x any \R object. Currently [sf::sf] (and sfg, sfc),
#'   and `numeric matrices` are supported.
#'
#'   If `x` is a `numeric matrix` with two
#'   columns, the first two columns will be interpreted as latitude and
#'   longitude, and its rows as separate points. Further columns are ignored
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
#'   c(48.186065, 16.419684,
#'     48.207853, 16.373894,
#'     48.083053, 16.285887),
#'   byrow = TRUE,
#'   ncol = 2
#' )
#'
#' \donttest{
#'   qmap(wp)
#' }
#'
#'
qmap <- function(
  x,
  ...,
  tools = TRUE,
  provider = getOption("qmap.providers", "OpenStreetMap")
){
  assert(is_scalar_bool(tools))
  assert(is.character(provider))
  UseMethod("qmap")
}




#' @rdname qmap
#' @export
qmap.leaflet <- function(
  x,
  ...,
  tools = TRUE,
  provider = getOption("qmap.providers", "OpenStreetMap")
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




#' @rdname qmap
#' @export
qmap.sf <- function(
  x,
  ...,
  tools = TRUE,
  provider = getOption("qmap.providers", "OpenStreetMap")
){
  gt <- unique(as.character(sf::st_geometry_type(x)))

  addFun <- switch(  # nolint
    gt,
   "MULTILINESTRING" = leaflet::addPolylines,
   "LINESTRING"      = leaflet::addPolylines,
   "POINT"           = leaflet::addCircleMarkers,
   "MULTIPOINT"      = leaflet::addCircleMarkers,
   "MULTIPOLYGON"    = leaflet::addPolygons,
   "POLYGON"         = leaflet::addPolygons,
   stop("'", gt, "' is not a valid geometry type")
  )

  if (is.na(sf::st_crs(x))){
    warning("no CRS set for `", deparse(substitute(x)), "`: trying WGS84 (EPSG:4326).", call. = FALSE)
    sf::st_crs(x) <- EPSG_WGS84
  }

  leaflet::leaflet() %>%
    addFun(data = sf::st_transform(x, EPSG_WGS84)) %>%
    qmap(tools = tools, provider = provider)
}




#' @rdname qmap
#' @param labels an optional `character` vector of popup labels
#' @export
qmap.default <- function(
  x,
  labels = NULL,
  ...,
  tools = TRUE,
  provider = getOption("qmap.providers", "OpenStreetMap")
){
  dd <- try(sf::st_as_sf(x), silent = TRUE)

  if (is_try_error(dd))
    dd <- try(st_as_sfc(x), silent = TRUE)

  if (is_try_error(dd))
    dd <- try(st_as_sf(as_coord_matrix(x)), silent = TRUE)

  if (is_try_error(dd)){
    stop(errorCondition(
      message = paste("cannot generate quickmap for object of class", class_fmt(x)),
      class = "objectNotSupportedError"
    ))
  }

  qmap(dd, tools = tools, provider = provider)
}




#' @export
qmap.data.frame <- function(
  x,
  labels = NULL,
  ...,
  tools = TRUE,
  provider = getOption("qmap.providers", "OpenStreetMap")
){
  if (any(vapply(x, inherits, logical(1), "sfc", USE.NAMES = FALSE))){
    qmap(st_as_sf(x), tools = tools, provider = provider)

  } else {
    NextMethod()
  }
}




#' @rdname qmap
#' @export
qmap.sfc <- qmap.sf





#' @rdname qmap
#' @export
qmap.sfg <- function(
  x,
  ...,
  tools = TRUE,
  provider = getOption("qmap.providers", "OpenStreetMap")
){
  qmap(
    sf::st_sf(geometry = sf::st_sfc(x)),
    tools = tools, provider = provider
  )
}




#' @rdname qmap
#' @export
qmap.matrix <- function(
  x,
  labels = NULL,
  ...,
  tools = TRUE,
  provider = getOption("qmap.providers", "OpenStreetMap")
){
  x   <- as_coord_matrix(x)
  res <- st_as_sfc(x)

  if (!is.null(labels)){
    qmap(res, tools = tools, provider = provider) %>%
      leaflet::addPopups(lat = x[, "lat"], lng = x[, "lon"], popup = as.character(labels))

  } else {
    qmap(res, tools = tools, provider = provider)
  }
}
