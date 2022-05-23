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

