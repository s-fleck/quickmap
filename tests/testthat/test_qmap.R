
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

  expect_s3_class(qmap(tdat), "leaflet")
  colnames(tdat) <- c("lon", "lat")
  expect_s3_class(qmap(tdat), "leaflet")
})



test_that("qmap.bbox", {
  tdat <- structure(
    c(xmin = 13.093937, ymin = 48.206416, xmax = 16.373189, ymax = 48.247874),
    class = "bbox",
    crs = structure(
      list(
        epsg = 4326L,
        proj4string = "+proj=longlat +datum=WGS84 +no_defs"),
      class = "crs")
    )

  expect_s3_class(qmap(tdat), "leaflet")
})





test_that("qmap.sfg", {
  tdat <-
    matrix(
      c(48.186065, 16.419684,
        48.207853, 16.373894,
        48.207853, 16.373894,
        48.083053, 16.285887),
      byrow = TRUE,
      ncol = 2
    )

  tdat <- sf::st_as_sf(as_waypoints(tdat))$geometry[[1]]

  expect_s3_class(qmap(tdat), "leaflet")
})
