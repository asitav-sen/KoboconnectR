
<!-- README.md is generated from README.Rmd. Please edit that file -->

# KoboconnectR <img src='man/figures/logo.png' align="right" height="139" />

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/KoboconnectR)](https://CRAN.R-project.org/package=KoboconnectR)
[![CRAN
checks](https://badges.cranchecks.info/summary/KoboconnectR.svg)](https://cran.r-project.org/web/checks/check_results_KoboconnectR.html)
[![License:
GPL-3.0](https://img.shields.io/badge/license-GPL--3.0-blue.svg)](https://cran.r-project.org/web/licenses/GPL-3.0)
[![](https://cranlogs.r-pkg.org/badges/KoboconnectR)](https://cran.r-project.org/package=KoboconnectR)
[![](https://cranlogs.r-pkg.org/badges/grand-total/KoboconnectR?color=blue)](https://r-pkg.org/pkg/KoboconnectR)

<!-- badges: end -->

The goal of KoboconnectR is to facilitate data extraction from
Kobotoolbox projects/assets into R, instead of downloading the
individual spreadsheet files.

## Installation

CRAN version can be downloaded with:

``` r
install.packages("KoboconnectR") 
```

The effort is still in its early stage. The development version can be
installed from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("asitav-sen/KoboconnectR")
```

## Getting started

Before being able to use the kobotoolbox API, one needs to set up API
token. The [kobotoolbox
documentation](https://support.kobotoolbox.org/api.html) lists multiple
ways for one to extract the token value. If there is a token, the
easiest way to retrieve it is by using `get_kobo_token()` function.

``` r
kobotools_kpi_data(assetid = "assetid", url = "eu.kobotoolbox.org", uname = "username", pwd = "password")
```

To find the asset id, one can use the `kobotools_api()` function that
returns a list of assets the user has access to.

``` r
kobotools_api(url = "eu.kobotoolbox.org", simplified = T, uname = "userid", pwd = "password")
```

## Downloading data

``` r
KoboconnectR::kobo_df_download(
  uname = "username",
  pwd = "password", assetid = "assetid",
  all = "false", lang = "_default",
  hierarchy = "false", include_grp = "true", grp_sep = "/", fsep = ";",
  multi_sel = "both", media_url = "true", fields = NULL, sub_ids = NULL, sleep = 2
)
```

**IF THE DOWNLOAD FAILS, TRY INCREASING THE `SLEEP` PARAMETER (use the
latest version please, dev version, if required).**

For further details, check the vignette.

## Contribution

The package is stable but still in early stages of development. Any help
to enhance it further will be highly appreciated. Please feel free to
fork, add/modify and create pull requests.

## Issues and Suggestions

Please feel free to add issue of suggestions in
[github](https://github.com/asitav-sen/KoboconnectR/issues)

## Dependencies

The following packages are required to install and run this package.

1.  Package [`httr2`](https://httr2.r-lib.org)
2.  Package [`jsonlite`](https://cran.r-project.org/package=jsonlite)
3.  Package [`curl`](https://cran.r-project.org/package=curl)
4.  Package [`mime`](https://cran.r-project.org/package=mime)
5.  Package [`openssl`](https://cran.r-project.org/package=openssl)
6.  Package [`R6`](https://cran.r-project.org/package=R6)
7.  Package `methods`
8.  Package [`readxl`](https://cran.r-project.org/package=readxl)
9.  Package [`rlang`](https://cran.r-project.org/package=rlang)
10. Package [`purrr`](https://cran.r-project.org/package=purrr)
