
test_that("smap uses as_coord_matrix/st_as_sfc/st_as_sf in the correct order if available", {
  tdat <-
    list(matrix(
      c(48.186065, 16.419684,
        48.207853, 16.373894,
        48.207853, 16.373894,
        48.083053, 16.285887),
      byrow = TRUE,
      ncol = 2
    ))
  class(tdat) <- c("testclass")
  expect_error(smap(tdat), class = "objectNotSupportedError")

  as_coord_matrix.testclass <- function(x){
    message("coercing to coord_matrix")
    as_coord_matrix(x[[1]])
  }
  registerS3method("as_coord_matrix", "testclass", as_coord_matrix.testclass)
  expect_message(expect_s3_class(smap(tdat), "leaflet"), "coercing to coord_matrix")

  st_as_sfc.testclass <- function(x){
    message("coercing to sfc")
    st_as_sfc(as_coord_matrix(x)[[1]])
  }
  registerS3method("st_as_sfc", "testclass", st_as_sfc.testclass)
  expect_message(expect_s3_class(smap(tdat), "leaflet"), "coercing to sfc")

  st_as_sf.testclass <- function(x){
    class(x) <- "matrix"
    message("coercing to sf")
    st_as_sf(as_coord_matrix(x))
  }
  registerS3method("st_as_sf", "testclass", st_as_sf.testclass)
  expect_message(expect_s3_class(smap(tdat), "leaflet"), "coercing to sf")
})





test_that("map_route creates a leaflet preview", {
  tdat <-
    matrix(
      c(48.186065, 16.419684,
        48.207853, 16.373894,
        48.207853, 16.373894,
        48.083053, 16.285887),
      byrow = TRUE,
      ncol = 2
    )

  expect_s3_class(smap(tdat), "leaflet")
  colnames(tdat) <- c("lon", "lat")
  expect_s3_class(smap(tdat), "leaflet")
})



test_that("smap.bbox", {
  tdat <- structure(
    c(xmin = 13.093937, ymin = 48.206416, xmax = 16.373189, ymax = 48.247874),
    class = "bbox",
    crs = structure(
      list(
        epsg = 4326L,
        proj4string = "+proj=longlat +datum=WGS84 +no_defs"),
      class = "crs")
    )

  expect_s3_class(smap(tdat), "leaflet")
})





test_that("smap.sfg", {
  tdat <-
    matrix(
      c(48.186065, 16.419684,
        48.207853, 16.373894,
        48.207853, 16.373894,
        48.083053, 16.285887),
      byrow = TRUE,
      ncol = 2
    )

  colnames(tdat) <- c("lat", "lon")
  tdat <- st_as_sf(as_coord_matrix(tdat))$geometry[[1]]
  expect_warning(expect_s3_class(smap(tdat), "leaflet"))
})




test_that("smap.character", {
  skip_on_cran()

  expect_s3_class(
    smap("https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip"),
    "leaflet"
  )
})

