
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

  print(qmap(tdat))
  qmap(tdat)
  colnames(tdat) <- c("lon", "lat")
  qmap(tdat)
})



test_that("qmap.bbox", {
  x <- structure(
    c(xmin = 13.093937, ymin = 48.206416, xmax = 16.373189, ymax = 48.247874),
    class = "bbox",
    crs = structure(
      list(
        epsg = 4326L,
        proj4string = "+proj=longlat +datum=WGS84 +no_defs"),
      class = "crs")
    )

  qmap(x)
})
