context("coord_matrix")


test_that("guess_loncol/guess_latcol work as expected", {
  coord_cols <- c("lon", "lat")
  x <- matrix(c(1, 2, 3, 4, 5, 6), ncol = 2)


  # matrix without colnames
  expect_identical(guess_loncol(x), 1L)
  expect_identical(guess_latcol(x), 2L)

  # matrix with colnames
  colnames(x) <- c("lat", "longitude")
  expect_identical(guess_loncol(x), 2L)
  expect_identical(guess_latcol(x), 1L)

  # matrix with two longitude columns
  colnames(x) <- c("lon", "long")
  expect_warning(guess_loncol(x), class = "multipleCandidateColumnsFoundWarning")
  expect_error(guess_latcol(x), class = "cannotGuessColumnError")

  # data.frame with appropriate colnames
  colnames(x) <- c("lat", "lon")
  expect_identical(guess_loncol(as.data.frame(x)), 2L)
  expect_identical(guess_latcol(as.data.frame(x)), 1L)
})




test_that("as_coord_matrix.numeric works as expected", {
  expect_identical(
    as_coord_matrix(c(16.422524, 48.185686)),
    as_coord_matrix(c(lon = 16.422524, lat =  48.185686))
  )

  expect_identical(
    as_coord_matrix(c(16.422524, 48.185686)),
    as_coord_matrix(c(LAT = 48.185686, LoNgItuDE = 16.422524))
  )
})




test_that("as_coord_matrix.data.frame works as expected", {
  x <- matrix(c(1, 2, 3, 4, 5, 6), ncol = 2)
  y <- as.data.frame(x)
  expect_error(as_coord_matrix(y), class = "cannotGuessColumnError")

  # switch lat and lon for the sake of this test
  colnames(x) <- c("lon", "lat")
  colnames(y) <- c("lat", "lon")
  y <- as_coord_matrix(y)

  expect_identical(colnames(y), c("lon", "lat"))

  expect_identical(x[, "lat"], y[, "lon"])
  expect_identical(x[, "lon"], y[, "lat"])
  expect_identical(colnames(x), colnames(y))
})




test_that("as_coord_matrix.default fails nicely on unspported objects", {

  expect_error(as_coord_matrix("blubb"), class = "objectNotSupportedError")

})

