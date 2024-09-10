<meta charset="UTF-8">

## **panelPomp**

### an *R* package for inference on panel partially observed Markov processes

[![Project Status: Moved to http://example.com – The project has been moved to a new location, and the version at that location should be considered authoritative.](https://www.repostatus.org/badges/latest/moved.svg)](https://www.repostatus.org/#moved) to [https://github.com/panelPomp-org/panelPomp](https://github.com/panelPomp-org/panelPomp)
[![CRAN Status](https://www.r-pkg.org/badges/version/panelPomp)](https://cran.r-project.org/package=panelPomp)
[![Last CRAN release date](https://www.r-pkg.org/badges/last-release/panelPomp)](https://cran.r-project.org/package=panelPomp)
[![R-CMD-check](https://github.com/cbreto/panelPomp/actions/workflows/r-cmd-check.yml/badge.svg)](https://github.com/cbreto/panelPomp/actions/workflows/r-cmd-check.yml) [![binary-build](https://github.com/cbreto/panelPomp/actions/workflows/binary-build.yml/badge.svg)](https://github.com/cbreto/panelPomp/actions/workflows/binary-build.yml) [![test-coverage](https://github.com/cbreto/panelPomp/actions/workflows/test-coverage.yml/badge.svg)](https://github.com/cbreto/panelPomp/actions/workflows/test-coverage.yml) [![codecov](https://codecov.io/gh/cbreto/panelPomp/branch/master/graph/badge.svg?token=1vT9TJfHGP)](https://codecov.io/gh/cbreto/panelPomp)

This package allows performing data analysis based on panel partially-observed Markov process (PanelPOMP) models. To implement such models, simulate them and fit them to panel data, 'panelPomp' extends some of the facilities provided for time series data by the 'pomp' package. Implemented methods include filtering (panel particle filtering) and maximum likelihood estimation (Panel Iterated Filtering) as proposed in Bretó, Ionides and King (2020) "Panel Data Analysis via Mechanistic Models" [\<doi:10.1080/01621459.2019.1604367\>](https://doi.org/10.1080/01621459.2019.1604367).

The latest version of the package can be installed from this GitHub source using `devtools::install_github('cbreto/panelPomp')`

Installing the current CRAN version is also possible using `install.packages("panelPomp")`

Related packages:

-   [**pomp**](https://github.com/kingaa/pomp/)
-   [**spatPomp**](https://github.com/kidusasfaw/spatPomp)
-   [**phylopomp**](https://github.com/kingaa/phylopomp/)
