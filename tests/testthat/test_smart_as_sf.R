context("smart_as_sf")


test_that("smart_as_sf works as expected for bbox objects", {

  x <- structure(
    c(xmin = 12.7, ymin = 48.0, xmax = 17.5, ymax = 48.5),
    class = "bbox",
    crs = structure(
      list(
        epsg = 4326L,
        proj4string = "+proj=longlat +datum=WGS84 +no_defs"),
      class = "crs")
    )

  expect_s3_class(smart_as_sf(x), "sf")
  expect_s3_class(smap(x), "leaflet")
})




test_that("smart_as_sf.character works", {
  skip_on_cran()
  tf  <- tempfile(fileext = ".zip")
  tf2 <- tempfile()
  on.exit(unlink(c(tf, tf2)))
  download.file(
    "https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip",
    destfile = tf
  )

  expect_s3_class(as_coord_matrix(tf), "coord_matrix")
  expect_s3_class(smart_as_sf(tf), "sf")
  expect_error(smart_as_sf(tf2), class = "value_error")
})
