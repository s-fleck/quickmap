
<!-- README.md is generated from README.Rmd. Please edit that file -->

# quickmap

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/s-fleck/quickmap.svg?branch=master)](https://travis-ci.org/s-fleck/quickmap)
[![Codecov test
coverage](https://codecov.io/gh/s-fleck/quickmap/branch/master/graph/badge.svg)](https://codecov.io/gh/s-fleck/quickmap?branch=master)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
<!-- badges: end -->

**quickmap** is a package for previewing spatial data as leaflet maps in
R. It is designed primarily for interactive use and produces nice maps
with minimal setup required. In contrast to other similar packages,
quickmap does not require an `sf` or `sp` object as input, but just
works with normal `data.frames` or `matrices` that have latitude and
longitude columns. See [the function
reference](https://s-fleck.github.io/quickmap/) for more infos.

## Installation

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("s-fleck/quickmap")
```

## CRAN release

A CRAN release is currently not planned. quickmap is pretty stable but
it currently has few features to distinquish itself from
[mapview](https://r-spatial.github.io/mapview/). If you like quickmap’s
simple API and automatic input coercion and would like to see it on
CRAN, please file an issue on the issue tracker.

## Example

``` r
library(quickmap)

# coordinate pairs without names are interpreted as longitude/latitude
qmap(c(16.422524, 48.185686))

# if you supply column names, qmap uses those to identify the correct columns
qmap(c(LAT = 48.185686, LoNgItuDE = 16.422524))

# when supplied with an url or file system path, qmap tries its best to discover
# supported spatial datasets
qmap("https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip")

# You can call qmap() on existing leaflet objects to add background tiles and
# a ruler for measuring distance
library(leaflet)
lf <- leaflet::leaflet() %>% 
  leaflet::addCircleMarkers(lng = 16.422524, lat = 48.185686)

lf
qmap(lf)
```
