
<!-- README.md is generated from README.Rmd. Please edit that file -->

# smartmap

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/s-fleck/smartmap.svg?branch=master)](https://travis-ci.com/s-fleck/smartmap)
[![Codecov test
coverage](https://codecov.io/gh/s-fleck/smartmap/branch/master/graph/badge.svg)](https://codecov.io/gh/s-fleck/smartmap?branch=master)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
<!-- badges: end -->

**smartmap** is a package for previewing spatial data as leaflet maps in
R. It is designed primarily for interactive use and produces nice maps
with minimal setup required. In contrast to other similar packages,
smartmap does not require an `sf` or `sp` object as input, but *just
works* with normal `data.frames` or `matrices` that have longitude and
latitude columns. See [the function
reference](https://s-fleck.github.io/smartmap/) for more info.

## Installation

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("s-fleck/smartmap")
```

## Example

``` r
library(smartmap)

# coordinate pairs without names are interpreted as longitude/latitude
smap(c(16.422524, 48.185686))

# if you supply column names, smap uses those to identify the correct columns
smap(c(LAT = 48.185686, LoNgItuDE = 16.422524))

# when supplied with an url or file system path, smap tries its best to discover
# supported spatial datasets
smap("https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip")

# You can call smap() on existing leaflet objects to add background tiles and
# a ruler for measuring distance
library(leaflet)
lf <- leaflet::leaflet() %>% 
  leaflet::addCircleMarkers(lng = 16.422524, lat = 48.185686)

lf
smap(lf)
```
