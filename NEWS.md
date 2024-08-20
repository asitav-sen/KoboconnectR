
#KoboconnectR 2.0.0

All the functions are updated to use `httr2` as `httr` is approaching end of life.
API calls now use version 2, instead of version 1 of Kobotoolbox REST APIs.


#KoboconnectR 1.2.2

Replaced `kobo.humanitarianresponse.info` with `eu.kobotoolbox.org` because of change in API location.



# KoboconnectR 1.2.1

Changed function `kobo_xls_dl()` to include multiple sheets.
Changed test files to rectify errors in CRAN (Bug fixes).

# KoboconnectR 1.2.0

Added new function `kobo_media_downloader` that downloads media from the kobo assets.

# KoboconnectR 1.1.1

Resolved issues of API not reacting to parameters. For example, setting `include_media_url` to "true" did not actually include the medial url. 
Also, resolved the issues which were causing the last version to fail during tests. Specifically, not failing gracefully when the issue was coming from the internet resource.

# KoboconnectR 1.1.0

* New function `kobo_df_download` added, that enables direct download of csv file. A csv export is created and downloaded. Then the export is deleted.

# KoboconnectR 1.0.0

* Ver 1.0 done, with aim to submit the package in CRAN
* Eliminated old functions that used older version of 'Kobotoolbox' APIs
* Simplified authentication. Instead of using API token, now only user id and password is sufficient
* Added a `NEWS.md` file to track changes to the package.
* Supports export creation to enable download of 'csv' and 'xls' files.

