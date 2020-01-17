
<!-- README.md is generated from README.Rmd. Please edit that file -->

# quickmap

<!-- badges: start -->

<!-- badges: end -->

**quickmap** is a package for previewing spatial data as interactive
leaflet maps in R. It distinquishes itself from other similar R packages
by beeing able to automatically coerce a wide variety of inputs to
leaflet maps

## Installation

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("s-fleck/quickmap")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(quickmap)

# coordinate pairs without names are interpreted as longitude/latitude
quickmap::qmap(c(16.422524, 48.185686))

# quickmap tries its best to guess the correct order from names
quickmap::qmap(c(LAT = 48.185686, LoNgItuDE = 16.422524))
```
