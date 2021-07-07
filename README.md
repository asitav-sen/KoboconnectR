
<!-- README.md is generated from README.Rmd. Please edit that file -->

# KoboconnectR <img src='man/figures/logo.png' align="right" height="139" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of Koboconnect is to facilitate data extraction from Kobotools
projects/assets into R.

## Installation

The effort is still in its early stage. The development version can be
installed from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("asitav-sen/KoboconnectR")
```

## Getting started

**These functions have been removed because of their dependency on
Renviron. Use the functions in Extracting data section**

Before being able to use the kobotools API, one needs the API token and
in some cases the user id and password. The [kobotools
documentation](https://support.kobotoolbox.org/api.html) lists multiple
ways for one to extract the token value. This token can be stored in
your Renviron using the `set_kobo_token` function.

``` r
library(KoboconnectR)
set_kobo_token("this is a token value")
```

It is optional. One can provide the token value as string in the
relevant functions to extract info/data. However, this may not always be
a secure option. `set_kobo_token` encrypts the token value and stores in
the Renviron. Hence, it is fairly secure.

The set value can be retrieved using `get_kobo_token` function.

``` r
get_kobo_token()
```

There are multiple variants of APIs available in kobotoolbox. One of
which also uses user id and password of one’s kobotoolbox account for
authentication. These can also be encrypted and saved in Renviron using
`set_kobo_id_pass` function.

``` r
set_kobo_id_pass(id="my_id",password="my_password")
```

The id and passwords can be retrieved using `get_kobo_id` and
`get_kobo_pass` functions.

``` r
get_kobo_id()
get_kobo_pass()
```

## Extracting data

### Using older version of kobotools API

As mentioned earlier, there are multiple APIs of kobotoolbox. The older
version is interesting because it can extract the data in csv format
which may be desirable. Similar procedure to extract data in csv format
using newer API is not documented in kobotoolbox, at least as on July
04, 2021. However, if anyone knows the ways, please feel free to
contribute in the package to make it more comprehensive.

Older version of API requires userid and password for authentication.
The first step is to extract information about all the data/project that
one has access to. This is done by `kobotools_data_list_kc` function.

``` r
kobotools_data_list_kc()
```

This will return a json with some details like this.

\[\[1\]\] \[\[1\]\]$id \[1\] 833396

\[\[1\]\]$id\_string \[1\] “anuZhfzfyfceU6myr52sE3”

\[\[2\]\]$title \[1\] “Test”

\[\[1\]\]$description \[1\] “Test”

\[\[1\]\]$url \[1\]
“<https://kc.humanitarianresponse.info/api/v1/data/833396?format=json>”

The `id` is of our interest. This id will be used later to extract the
data using `kobotools_kc_download` function.

``` r
kobotools_kc_download(assetid = "8332...")
```

### Using the newer version

Newer version is of interest to us because the older version may get
depreciated. One can extract the data as json.A token is required to
access the API.

Like to older version, one needs to get the details of the
assets/projects one has in their kobotoolbox account. This can be done
using `kobotools_api` function.

``` r
kobotools_api()
```

THe asset id is available in `results[[x]]$uid`. Once you have the asset
id, if can be passed in `kobotools_kpi_data` function to extract data.
Please note that the asset ids in old and new APIs are not same.

``` r
kobotools_kpi_data(assetid= "anuZhfzfyfce")
```

This returns a json file.

## Contribution

The package is still in the early stage of development. Any help will be
highly appreciated. Please feel free to fork, add/modify and create pull
requests.

## Issues and Suggestions

Please feel free to add issue of suggestions in
[github](https://github.com/asitav-sen/KoboconnectR/issues)

## Dependencies

The following packages have been used to develop this.

1.  Package
    [`safer`](https://cran.r-project.org/web/packages/safer/index.html)
    to encrypt and decrypt the strings
2.  Package
    [`httr`](https://cran.r-project.org/web/packages/httr/index.html) to
    use the APIs
3.  Package
    [`jsonlite`](https://cran.r-project.org/web/packages/jsonlite/index.html)
    to read extracted contents
4.  Package
    [`hexSticker`](https://cran.r-project.org/web/packages/hexSticker/index.html)
    to create the logo
5.  Package
    [`usethis`](https://cran.r-project.org/web/packages/usethis/index.html)
    to help with setup
6.  Package
    [`devtools`](https://cran.r-project.org/web/packages/devtools/index.html)
    to develop the package
7.  Package
    [`Roxygen2`](https://cran.r-project.org/web/packages/roxygen2/index.html)
    to help with documentation
8.  Package
    [`testthat`](https://cran.r-project.org/web/packages/testthat/index.html)
    for automation of testing
