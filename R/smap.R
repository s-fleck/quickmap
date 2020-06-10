#' View spatial objects as interactive leaflet maps
#'
#' Can be used to preview spatial \R objects
#'
#' @note `smartmap.default()` looks if an [sf::st_as_sf()],
#'   [sf::st_as_sfc()] or [smartmap::as_coord_matrix()] method exists for `x` (in
#'   that order). If you are a package developer and want to support smartmap
#'   for a custom S3 class in your package, it is enough to provide one of these
#'   methods.
#'
#' @inheritParams as_coord_matrix
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
#'   smap(wp)
#'   smap(c(16.419684, 48.186065))
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
  dd <- try(sf::st_as_sf(x), silent = TRUE)

  if (is_try_error(dd))
    dd <- try(st_as_sfc(x), silent = TRUE)

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




#' @rdname smap
#' @export
smap.character <- function(
  x,
  labels = NULL,
  ...,
  tools = TRUE,
  provider = getOption("smap.providers", "OpenStreetMap")
){
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
    infile <- grep("\\.shp$", unzipped, value = TRUE)
  }

  if (is_shpfile(infile))
    smap(sf::read_sf(infile))
  else
    stop()
}




handle_zipfile <- function(x){
  tdir <- tempfile()
  dir.create(tdir)
  res <- utils::unzip(x, exdir = tdir)
  on.exit({
    unlink(res,  recursive = TRUE)
    unlink(tdir, recursive = TRUE)
  }, add = TRUE)

  if (sum(grepl("\\.shp$", res)) > 1){
    stop("More than one shapefile found inside ", x)
  }

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
