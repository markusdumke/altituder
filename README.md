
<!-- README.md is generated from README.Rmd. Please edit that file -->
altituder
=========

[![Travis build status](https://travis-ci.org/markusdumke/altituder.svg?branch=master)](https://travis-ci.org/markusdumke/altituder)

An R package to get altitude (elevation) data from several APIs.

Installation
------------

You can install the development version of `altituder` from Github via:

``` r
# install.packages("devtools")
devtools::install_github("markusdumke/altituder")
```

get\_altitude
-------------

Currently there is only one function `get_altitude`, which queries altitude data for given locations.

``` r
library(altituder)
get_altitude(.longitude = 11, .latitude = 48)
#> [1] 647

# Input is a data.frame
get_altitude(.Data = data.frame(longitude = 10:12, latitude = 48:50))
#> [1] 650 528 777
```

Which API can be specified via the `.src` argument. Currently supported are:

-   `geonames`: Uses the geonames webservice. The number of free requests is limited, for a higher rate, register and pass your username to this function. Check usage terms at \[<http://www.geonames.org/export/web-services.html>\].
-   `google`: Uses the Google Elevation API. You need to pass your API key. Costs can apply. Check usage terms at \[<https://developers.google.com/maps/documentation/elevation/usage-and-billing>\].
-   `openelevation`: Uses free Open-Elevation API. See \[<https://github.com/Jorl17/open-elevation>\].
-   `racemap`: Uses free Racemap API. Based on mapzen terrain data. See \[<https://github.com/racemap/elevation-service>\].

``` r
# Set API key via environment variable
# Warning: Costs may apply!
get_altitude(.longitude = 11, .latitude = 48, 
             .src = "google", .google.api.key = Sys.getenv("GOOGLE_API_KEY"))
#> [1] 648
```
