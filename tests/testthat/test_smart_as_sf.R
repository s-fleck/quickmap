context("smart_as_sf")


test_that("smart_as_sf works as expected for bbox objects", {

  x <- structure(
    c(xmin = 12.7, ymin = 48.0, xmax = 17.5, ymax = 48.7),
    class = "bbox",
    crs = structure(
      list(
        epsg = 4326L,
        proj4string = "+proj=longlat +datum=WGS84 +no_defs"),
      class = "crs")
    )

  expect_s3_class(smart_as_sf(x), "sf")
  expect_s3_class(qmap(x), "leaflet")
})
