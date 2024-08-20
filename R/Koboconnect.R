#' Check kobotoolbox API and retrieve overall info about the projects/assets
#'
#' @description
#' `kobotools_api` is a wrapper for kobotoolbox API `https://[kpi-url]/api/v2/assets/`
#'
#' @details
#' The function takes two variables. First one is `url` which is the `[kpi-url]`. For most users it will be "eu.kobotoolbox.org" or
#' "kf.kobotoolbox.org". Former is the default. The second parameter is `simplified` which takes a logical value. If set to true,
#' the function will return selected values from the parsed data and return a data frame. When set to false, a json will be returned with
#' all the details.
#'
#' @param url The `[kpi-url]` of kobotoolbox. Default is "eu.kobotoolbox.org"
#' @param simplified A logical value, default is true
#' @param uname takes the username
#' @param pwd takes the password
#' @param encoding is the encoding to be used. Default is "UTF-8".
#' @param qry to add queries
#'
#' @return The function returns the asset details from the API, inform of a data frame or json.
#'
#'
#'
#' @importFrom httr2 request req_auth_basic req_perform resp_body_json resp_body_string resp_body_html req_method req_timeout req_body_form resp_status resp_body_raw
#' @importFrom jsonlite fromJSON
#' @import R6
#' @import curl
#' @import methods
#' @import mime
#' @import openssl
#' @import dplyr
#'
#' @export
#

kobotools_api <- function(url = "eu.kobotoolbox.org", simplified = TRUE, uname = "", pwd = "", encoding = "UTF-8", qry="") {
  if (!is.character(url)) stop("URL entered is not a string")
  if (!is.character(uname)) stop("uname (username) entered is not a string")
  if (!is.character(pwd)) stop("pwd (password) entered is not a string")
  if (is.null(url) | url == "") stop("URL empty")
  if (is.null(uname) | uname == "") stop("uname (username) empty")
  if (is.null(pwd) | pwd == "") stop("pwd (password) empty")
  if (!is.logical(simplified)) stop("simplified can take only logical value")


  if(qry==""){
    fullurl <- paste0("https://", url, "/api/v2/assets.json")
      }
    else {
    fullurl <- paste0("https://", url, "/api/v2/assets.json/?",qry)
    }


  respon.api <- tryCatch(
    expr = {
      httr2::request(fullurl) |>
        httr2::req_auth_basic(uname, pwd) |>
        httr2::req_perform()
    },
    error = function(x) {
      print("Error. Please try again or check the input parameters.")
      cat("Error: ", e$message, "\n")
      return(NULL)
    }
  )

  if (!is.null(respon.api)) {
    parsed <- httr2::resp_body_json(respon.api, simplifyVector = FALSE, encoding = encoding)

    if (simplified == FALSE) {
      return(parsed)
    } else {
      results <- lapply(parsed$results, function(res) {
        list(
          name = res$name,
          assetid = res$uid,
          active = res$deployment__active,
          submissions = res$deployment__submission_count,
          owner = res$owner__username,
          date_created = res$date_created,
          date_modified = res$date_modified,
          URL = res$url
        )
      })

      simp.parsed <- do.call(dplyr::bind_rows, lapply(results, as.data.frame))
      return(simp.parsed)
    }
  } else {
    return(NULL)
  }
}


#' Extract data from kobotoolbox
#'
#' @description
#' `kobotools_kpi_data` is a wrapper for kobotoolbox API `https://[URL]/api/v2/assets/[assetid]/data/`
#'
#' @details
#' The function takes the url as one of the inputs. And asset id as another. Both are strings. The asset id is found by running
#' the [kobotools_api()] function.Other parameters are username and password.
#'
#' @param url The `[kpi-url]` of kobotoolbox. Default is "eu.kobotoolbox.org"
#' @param assetid is the asset id of the asset for which the data is to be downloaded. The id can be found by running [kobotools_data_list_kc()]
#' @param uname is username of your kobotoolbox account
#' @param pwd is the password of the account
#' @param encoding is the encoding to be used. Default is "UTF-8".
#' @param format lets you define the format of output for e.g. json or xml
#'
#' @return The function returns the data in json format
#'
#'
#' @importFrom httr2 request req_auth_basic req_perform resp_body_json resp_body_string resp_body_html req_method req_timeout  req_body_form resp_status resp_body_raw
#' @importFrom jsonlite fromJSON
#'
#'
#'
#' @export

kobotools_kpi_data <- function(assetid, url = "eu.kobotoolbox.org", uname = "", pwd = "", encoding = "UTF-8", format = "json") {
  if (!is.character(url)) stop("URL entered is not a string")
  if (!is.character(uname)) stop("uname (username) entered is not a string")
  if (!is.character(pwd)) stop("pwd (password) entered is not a string")
  if (!is.character(assetid)) stop("assetid entered is not a string")
  if (is.null(url) | url == "") stop("URL empty")
  if (is.null(uname) | uname == "") stop("uname (username) empty")
  if (is.null(pwd) | pwd == "") stop("pwd (password) empty")
  if (is.null(assetid) | assetid == "") stop("assetid empty")
  if (!format %in% c("json", "xml")) stop("format must be either 'json' or 'xml'")

  fullurl <- paste0("https://", url, "/api/v2/assets/", assetid, "/data.",format)

  respon.kpi <- tryCatch(
    expr = {
      httr2::request(fullurl) |>
        httr2::req_auth_basic(uname, pwd) |>
        httr2::req_perform()
    },
    error = function(x) {
      print("Error. Please try again or check the input parameters.")
      return(NULL)
    }
  )

  if (!is.null(respon.kpi)) {
    content_type <- httr2::resp_content_type(respon.kpi)

    if (grepl("json", content_type)) {
      return(httr2::resp_body_json(respon.kpi, encoding = encoding))
    } else if (grepl("xml", content_type)) {
      return(httr2::resp_body_string(respon.kpi, encoding = encoding))
    }else if(grepl("html", content_type)){
      return(httr2::resp_body_html(respon.kpi, encoding = encoding))
    }else {
      warning("Unexpected content type: ", content_type, ". Unable to parse as JSON/XML/HTML.")
      return(NULL)
    }
  } else {
    return(NULL)
  }
}






#' Know your API token or check
#'
#' @description
#' `get_kobo_token` is a wrapper for kobotoolbox API `https://"[url]"/token/?format=json`
#'
#' @details
#' The function returns the API token.
#'
#' @param url The `[url]` of kobotoolbox. Default is "eu.kobotoolbox.org".
#' @param uname is username of your kobotoolbox account
#' @param pwd is the password of the account
#' @param encoding is the encoding to be used. Default is "UTF-8".
#'
#' @return The function returns the token associated with your id and password in the given url.
#'
#'
#' @importFrom httr2 request req_auth_basic req_perform resp_body_json resp_body_string resp_body_html req_method req_timeout  req_body_form resp_status resp_body_raw
#' @importFrom jsonlite fromJSON
#'
#'
#' @export

# get_kobo_token <- function(url = "eu.kobotoolbox.org", uname = "", pwd = "", encoding = "UTF-8") {
#   if (!is.character(url)) stop("URL entered is not a string")
#   if (!is.character(uname)) stop("uname (username) entered is not a string")
#   if (!is.character(pwd)) stop("pwd (password) entered is not a string")
#   if (is.null(url) | url == "") stop("URL empty")
#   if (is.null(uname) | uname == "") stop("uname (username) empty")
#   if (is.null(pwd) | pwd == "") stop("pwd (password) empty")
#
#   fullurl <- paste0("https://", url, "/token/?format=json")
#   respon.token <- tryCatch(
#     expr = {
#       GET(fullurl, authenticate(uname, pwd), progress())
#     },
#     error = function(x) {
#       print("Error. Please try again or check the input parameters.")
#       return(NULL)
#     }
#   )
#
#   if (!is.null(respon.token)) {
#     # stop_for_status(respon.token,"extract token")
#     tkn <- fromJSON(content(respon.token, "text", encoding = encoding))
#     return(tkn)
#   } else {
#     return(NULL)
#   }
# }

get_kobo_token <- function(url = "eu.kobotoolbox.org", uname = "", pwd = "", encoding = "UTF-8") {
  if (!is.character(url)) stop("URL entered is not a string")
  if (!is.character(uname)) stop("uname (username) entered is not a string")
  if (!is.character(pwd)) stop("pwd (password) entered is not a string")
  if (is.null(url) | url == "") stop("URL empty")
  if (is.null(uname) | uname == "") stop("uname (username) empty")
  if (is.null(pwd) | pwd == "") stop("pwd (password) empty")

  fullurl <- paste0("https://", url, "/token/?format=json")

  respon.token <- tryCatch(
    {
      response <- httr2::request(fullurl) |>
        httr2::req_auth_basic(uname, pwd) |>
        httr2::req_perform()

      httr2::resp_body_json(response, encoding = encoding)
    },
    error = function(e) {
      cat("Error: ", e$message, "\n")
      return(NULL)
    }
  )

  return(respon.token)
}




#' See list of exports created
#'
#' @description
#' `kobo_exports` is a wrapper for kobotoolbox API `https://[kpi-url]/api/v2/assets/[assetid]/exports/`
#'
#' @details
#' The function returns the export views.
#'
#' @param url The `[url]` of kobotoolbox. Default is "eu.kobotoolbox.org".
#' @param uname is username of your kobotoolbox account
#' @param pwd is the password of the account
#' @param encoding is the encoding to be used. Default is "UTF-8"
#' @param assetid the asset id
#' @param simplified if TRUE, then return a simple data frame with selected data, if FALSE returns the full list.
#'
#' @return The function returns a list of exports available for the account id and password entered.
#'
#'
#' @importFrom httr2 request req_auth_basic req_perform resp_body_json resp_body_string resp_body_html req_method req_timeout  req_body_form resp_status resp_body_raw
#' @importFrom jsonlite fromJSON
#'
#' @export

kobo_exports <- function(url = "eu.kobotoolbox.org", uname = "", pwd = "", encoding = "UTF-8", assetid="", simplified=FALSE) {
  # Input validation
  if (!is.character(url) || is.null(url) || url == "") stop("URL must be a non-empty string.")
  if (!is.character(uname) || is.null(uname) || uname == "") stop("Username must be a non-empty string.")
  if (!is.character(pwd) || is.null(pwd) || pwd == "") stop("Password must be a non-empty string.")
  if(!is.logical(simplified) || is.null(simplified) || simplified=="") stop("Simplified must be logical")

  fullurl <- paste0("https://", url,"/api/v2/assets/",assetid,"/exports.json")

  # Attempt to send the request and handle any errors
  tryCatch(
    {
      response <- httr2::request(fullurl) |>
        httr2::req_auth_basic(uname, pwd) |>
        httr2::req_perform()

      # Parse and return the JSON response
      exports <- httr2::resp_body_json(response, encoding = encoding)
      results <- lapply(exports$results, function(res) {
        list(
          url = res$url,
          status = res$status,
          uid = res$uid,
          date_created = res$date_created,
          outputurl = res$result,
          type = res$data$type
        )
      })

      if(simplified==TRUE){
        simp.parsed <- do.call(dplyr::bind_rows, lapply(results, as.data.frame))
        return(simp.parsed)
      }
      if(simplified==FALSE){
        return(exports)
      }


    },
    error = function(e) {
      cat("Error: ", e$message, "\n")
      return(NULL)
    }
  )
}


#' Create an export
#'
#' @description
#' `kobo_export_create` is a wrapper for kobotoolbox API `https://[kpi-URL]/api/v2/assets/[asset-uid]/exports/`
#'
#' @details
#' The function creates an export of survey data. If successful, returns the URL of the data that can be directly downloaded/read/imported in R.
#'
#' @param url The `[url]` of kobotoolbox Default is "eu.kobotoolbox.org".
#' @param uname is username of your kobotoolbox account
#' @param pwd is the password of the account
#' @param assetid is the id of the asset for which the export is to be created
#' @param all takes logical value in string format. Used to specify whether fields from all form versions will be included in the export.Acceptable values are "true" or "false". Default value is "false".
#' @param lang takes the language. For e.g. "English (en)". For "XML Values as headers", use '_xml'.
#' @param hierarchy takes logical value in string format. Used to specify whether the group hierarchy will be displayed in labels. Acceptable values are "true" or "false". Default value is "false".
#' @param grp_sep is the group separator. Default value is "/".
#' @param include_grp defines whether or not to include groups. Default value is "true".
#' @param multi_sel is used to specify the display of multiple_select-type responses. Valid inputs include "both", "summary" or "details". Default is "both".
#' @param fields is an array of column names to be included in the export (including their group hierarchy). Valid inputs include:
#' An array containing any string value that matches the XML column name,
#' An empty array which will result in all columns being included,
#' If "fields" is not included in the "export_settings", all columns will be included in the export
#'
#' @param media_url This will include an additional column for media-type questions ("question_name_URL") with the URL link to the hosted file. Valid inputs are "true" or "false". Default value is true.
#' @param sub_ids is an array of submission ids that will filter exported submissions to only the specified array of ids. Valid inputs include an array containing integer values or an empty array.#'
#' @param type specifies the export format. Valid formats include "csv","xls", "spss_labels" and "geojson"
#' @param qry is a JSON object containing a Mongo filter query for filtering exported submissions. Valid inputs include
#' a JSON object containing a valid Mongo query or An empty JSON object (no filtering)
#' @param flatten is a is a boolean (in form of string) value and only relevant when exporting to "geojson" format. Valid inputs are "true" and "false"
#' @param sleep is the sleep time between API actions. For example, it takes time to download an export. But R does not wait for the download to finish before going to next step. Hence the need to provide a break between consecutive API actions. Default value is 2 (seconds).
#'
#'
#' @return The function creates an export, prints and returns the URL of the export created
#'
#'
#' @importFrom httr2 request req_auth_basic req_perform resp_body_json resp_body_string resp_body_html req_method req_timeout  req_body_form resp_status resp_body_raw
#' @importFrom jsonlite fromJSON
#'
#' @export

kobo_export_create <- function(url = "eu.kobotoolbox.org", uname = "", pwd = "",
                               assetid = "", type = "csv", all = "false", lang = "_default",
                               hierarchy = "false", include_grp = "true", grp_sep = "/",
                               multi_sel = "both", fields = NULL, media_url = "true",
                               sub_ids = NULL, qry = NULL, flatten = "true", sleep = 3) {

  # Call the export_creator function
  export_res <- tryCatch(
    {
      export_creator(
        url = url, uname = uname, pwd = pwd,
        assetid = assetid, type = type, all = all, lang = lang,
        hierarchy = hierarchy, include_grp = include_grp, grp_sep = grp_sep,
        multi_sel = multi_sel, fields = fields, media_url = media_url,
        sub_ids = sub_ids, qry = qry, flatten = flatten, sleep = sleep
      )
    },
    error = function(e) {
      cat("Error: ", e$message, "\n")
      return(NULL)
    }
  )

  # Check if the export creation was successful
  if (is.null(export_res)) {
    cat("Export could not be created\n")
    return(NULL)
  } else {
    cat("Export created with URL:", export_res[[1]], "\n")
    return(unlist(export_res[[1]]))
  }
}


#' Creates a data frame after creating a 'csv' export and downloading it
#'
#' @description
#' `kobo_df_download` is a wrapper for kobotoolbox API
#'
#' @details
#' The function creates an export of survey data in 'csv'. If successful, it attempts to download the data and and return a data frame. Finally, the function attempts to delete the export.
#'
#' @param url The `[url]` of kobotoolbox Default is "eu.kobotoolbox.org".
#' @param uname is username of your kobotoolbox account
#' @param pwd is the password of the account
#' @param assetid is the id of the asset for which the export is to be created
#' @param all takes logical value in string format. Used to specify whether fields from all form versions will be included in the export.Acceptable values are "true" or "false". Default value is "false".
#' @param lang takes the language. For e.g. "English (en)". For "XML Values as headers", use '_xml'.
#' @param hierarchy takes logical value in string format. Used to specify whether the group hierarchy will be displayed in labels. Acceptable values are "true" or "false". Default value is "false".
#' @param grp_sep is the group separator. Default value is "/".
#' @param include_grp defines whether or not to include groups. Default value is "true".
#' @param fsep is the separator of the downloaded csv file. In most of the cases, it is ";", which is the default. However,
#' if you notice that the separator is the downloaded data is "," or something else, you can change it.
#' @param multi_sel is used to specify the display of multiple_select-type responses. Valid inputs include "both", "summary" or "details". Default is "both".
#' @param fields is an array of column names to be included in the export (including their group hierarchy). Valid inputs include:
#' An array containing any string value that matches the XML column name,
#' An empty array which will result in all columns being included,
#' If "fields" is not included in the "export_settings", all columns will be included in the export
#'
#' @param media_url This will include an additional column for media-type questions ("question_name_URL") with the URL link to the hosted file. Valid inputs are "true" or "false". Default value is true.
#' @param sub_ids is an array of submission ids that will filter exported submissions to only the specified array of ids. Valid inputs include an array containing integer values or an empty array.
#' @param sleep is the sleep time between API actions. For example, it takes time to download an export. But R does not wait for the download to finish before going to next step. Hence the need to provide a break between consecutive API actions. Default value is 2 (seconds).
#'
#' @return The function returns a data frame of data downloaded from 'Kobotoolbox'.
#'
#'
#' @importFrom httr2 request req_auth_basic req_perform resp_body_json resp_body_string resp_body_html req_method req_timeout  req_body_form resp_status resp_body_raw
#' @importFrom jsonlite fromJSON
#' @importFrom utils read.csv
#'
#' @export

kobo_df_download <- function(url = "eu.kobotoolbox.org", uname = "", pwd = "",
                             assetid = "", all = "false", lang = "_default",
                             hierarchy = "false", include_grp = "true", grp_sep = "/", fsep = ";",
                             multi_sel = "both", media_url = "true", fields = NULL, sub_ids = NULL, sleep = 2) {

  # Create a new export using the export_creator function
  new_export_details <- tryCatch(
    {
      x<-export_creator(
        url = url, uname = uname, pwd = pwd,
        assetid = assetid, type = "csv", all = all, lang = lang,
        hierarchy = hierarchy, include_grp = include_grp, grp_sep = grp_sep,
        multi_sel = multi_sel, fields = fields, media_url = media_url, sub_ids = sub_ids, sleep = sleep
      )
      Sys.sleep(sleep*2)
      x
    },
    error = function(e) {
      cat("Error during export creation: ", e$message, "\n")
      return(NULL)
    }
  )

  if (is.null(new_export_details)) {
    cat("Export creation was not successful\n")
    return(NULL)
  }


  # Attempt to download the data using the export_downloader function
  dff <- tryCatch(
    {
      export_downloader(new_export_details[[1]], fsep, uname, pwd, sleep)
    },
    error = function(e) {
      cat("Error during export download: ", e$message, "\n")
      return(NULL)
    }
  )

  # Only proceed to delete the export if download was successful
  if (!is.null(dff)) {
    deleteact <- tryCatch(
      {
        httr2::request(paste0("https://", url, "/api/v2/assets/", assetid, "/exports/", new_export_details[[2]], "/")) |>
          httr2::req_auth_basic(uname, pwd) |>
          httr2::req_method("DELETE") |>
          httr2::req_perform()


      },
      error = function(e) {
        cat("Error during deletion: ", e$message, "\n")
        return(NULL)
      }
    )
    Sys.sleep(sleep * 2)
    if (!is.null(deleteact) && httr2::resp_status(deleteact) == 200) {
      cat("Export deleted from server\n")
    } else {
      cat("Failed to delete export. Please delete manually.\n")
    }
  } else {
    cat("Export download was not successful; skipping deletion.\n")
  }

  return(dff)
}


#'  Downloads media data from Kobotoolbox
#'
#' @description
#' `kobo_media_downloader` downloads media from data downloaded using `kobo_df_download`. Loops through media columns and downloads files individually.
#'
#' @details
#' The function creates an export of survey data in 'csv'. If successful, it attempts to download the data and and returns a data frame. From that data frame, the function extracts the URLs of media and attempts to download them.
#'
#' @param url The `[url]` of kobotoolbox Default is "eu.kobotoolbox.org".
#' @param uname is username of your kobotoolbox account
#' @param pwd is the password of the account
#' @param assetid is the id of the asset for which the export is to be created
#' @param fsep is the separator of the downloaded csv file. In most of the cases, it is ";", which is the default. However,
#' @param sleep is the sleep time between API actions. For example, it takes time to download an export. But R does not wait for the download to finish before going to next step. Hence the need to provide a break between consecutive API actions. Default value is 2 (seconds).
#' @param identifier is the key using with the columns with URL is identified. Default value is "URL" because in most of the cases, the columns
#' containing the URL values end with "URL". Please note that any other column name with similar value may cause error.
#' @param timeoutval is the timeout value in seconds to download the media files. Default is 300 seconds.
#' @param destfolder is the folder where the media is to be stored.
#'
#' @return The function returns a data frame of data downloaded from 'Kobotoolbox'.
#'
#'
#' @importFrom httr2 request req_auth_basic req_perform resp_body_json resp_body_string resp_body_html req_method req_timeout  req_body_form resp_status resp_body_raw
#' @importFrom jsonlite fromJSON
#' @importFrom utils read.csv download.file
#'
#' @export


kobo_media_downloader <- function(url = "eu.kobotoolbox.org", uname, pwd, assetid, fsep = ";", sleep = 2, identifier = "URL", timeoutval = 300, destfolder = "media") {

  # Download the data using kobo_df_download
  dat <- tryCatch(
    {
      kobo_df_download(
        url = url, uname = uname,
        pwd = pwd, assetid = assetid,
        lang = "_default", sleep = sleep, fsep = fsep
      )
    },
    error = function(e) {
      cat("Error during data download: ", e$message, "\n")
      return(NULL)
    }
  )

  if (!is.null(dat)) {
    cat("Please note that this function loops over the URLs and downloads the individual files. This process can be slow and some files may fail to download due to timeout issues. Downloading a zipped file is not supported using API yet in Kobotoolbox.\n")

    # Identify columns that contain URLs
    cnamesdat <- colnames(dat)
    urlcols <- cnamesdat[grepl(identifier, cnamesdat, ignore.case = TRUE)]

    # Set timeout option for downloads
    options(timeout = max(timeoutval, getOption("timeout")))

    # Create destination folder if it doesn't exist
    if (!file.exists(destfolder)) {
      dir.create(destfolder)
    }

    # Loop over URL columns and download files
    for (urlcol in urlcols) {
      for (i in seq_along(dat[[urlcol]])) {
        if (!is.na(dat[[urlcol]][i]) && nzchar(dat[[urlcol]][i])) {
          fname <- paste0(destfolder, "/", urlcol, "_", i)
          tryCatch(
            {
              httr2::request(dat[[urlcol]][i]) |>
                httr2::req_auth_basic(uname, pwd) |>
                httr2::req_timeout(timeoutval) |>
                httr2::req_method("GET") |>
                httr2::req_perform(path=fname)
              cat("Downloaded:", fname, "\n")
            },
            error = function(e) {
              cat("Failed to download:", dat[[urlcol]][i], "\nError:", e$message, "\n")
            }
          )
        }
      }
    }

    return(TRUE)
  } else {
    cat("Data could not be downloaded. Please try again or check the parameters.\n")
    return(FALSE)
  }
}


#'  Downloads data (xls type) from Kobotoolbox
#'
#' @description
#' `kobo_xls_dl` is a wrapper for kobotoolbox API that attemps to download data in excel compatible formal.
#'
#' @details
#' The function creates an export of survey data in 'xls'. If successful, it attempts to download the data and and return a data frame (reading using `readxl::read_excel`).
#'
#' @param url The `[url]` of kobotoolbox Default is "eu.kobotoolbox.org".
#' @param uname is username of your kobotoolbox account
#' @param pwd is the password of the account
#' @param assetid is the id of the asset for which the export is to be created
#' @param all takes logical value in string format. Used to specify whether fields from all form versions will be included in the export.Acceptable values are "true" or "false". Default value is "false".
#' @param lang takes the language. For e.g. "English (en)". For "XML Values as headers", use '_xml'.
#' @param hierarchy takes logical value in string format. Used to specify whether the group hierarchy will be displayed in labels. Acceptable values are "true" or "false". Default value is "false".
#' @param grp_sep is the group separator. Default value is "/".
#' @param include_grp defines whether or not to include groups. Default value is "true".
#' @param multi_sel is used to specify the display of multiple_select-type responses. Valid inputs include "both", "summary" or "details". Default is "both".
#' @param fields is an array of column names to be included in the export (including their group hierarchy). Valid inputs include:
#' An array containing any string value that matches the XML column name,
#' An empty array which will result in all columns being included,
#' If "fields" is not included in the "export_settings", all columns will be included in the export
#'
#' @param media_url This will include an additional column for media-type questions ("question_name_URL") with the URL link to the hosted file. Valid inputs are "true" or "false". Default value is true.
#' @param sub_ids is an array of submission ids that will filter exported submissions to only the specified array of ids. Valid inputs include an array containing integer values or an empty array.
#' @param sleep is the sleep time between API actions. For example, it takes time to download an export. But R does not wait for the download to finish before going to next step. Hence the need to provide a break between consecutive API actions. Default value is 2 (seconds).
#'
#' @return The function returns a data frame of data downloaded from 'Kobotoolbox'.
#'
#'
#' @importFrom httr2 request req_auth_basic req_perform resp_body_json resp_body_string resp_body_html req_method req_timeout req_body_form resp_status resp_body_raw resp_check_status
#' @importFrom jsonlite fromJSON
#' @importFrom utils read.csv
#' @importFrom readxl read_excel excel_sheets
#' @importFrom rlang set_names
#' @importFrom purrr map
#'
#' @export
kobo_xls_dl <- function(url = "eu.kobotoolbox.org", uname = "", pwd = "",
                        assetid = "", all = "false", lang = "_default",
                        hierarchy = "false", include_grp = "true", grp_sep = "/",
                        multi_sel = "both", media_url = "true", fields = NULL, sub_ids = NULL, sleep = 2) {
  new_export_details <- export_creator(
    url = url, uname = uname, pwd = pwd,
    assetid = assetid, type = "xls", all = all, lang = lang,
    hierarchy = hierarchy, include_grp = include_grp, grp_sep = grp_sep,
    multi_sel = multi_sel, fields = fields, media_url = media_url, sub_ids = sub_ids, sleep = sleep
  )

  Sys.sleep(sleep*2)

  if (is.null(new_export_details)) {
    cat("Export creation was not successful.\n")
    return(NULL)
  } else {
    # Download the export file using your custom function (adjust parameters as needed)
    dff <- export_downloader(new_export_details[[1]], uname = uname, pwd = pwd, sleep = sleep, type = "xls")

    # Construct the URL for the DELETE request
    delete_url <- paste0("https://", url, "/api/v2/assets/", assetid, "/exports/", new_export_details[[2]], "/")

    # Perform the DELETE request using httr2
    deleteact <- httr2::request(delete_url) |>
      httr2::req_auth_basic(uname, pwd) |>
      httr2::req_method("DELETE") |>
      httr2::req_perform()

    # Check if the delete request was successful
    while (is.na(httr2::resp_status(deleteact)) | is.null(httr2::resp_status(deleteact))) {
      cat("Attempting export deletion...\n")
    }

    # Handle the status of the DELETE request
    if (httr2::resp_status(deleteact) == 200) {
      cat("Export deleted from server.\n")
    } else {
      cat("Failed to delete export. Please delete manually.\n")
      httr2::resp_check_status(deleteact)
    }

    return(dff)
  }

}
