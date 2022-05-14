#' Check kobotoolbox API and retrieve overall info about the projects/assets
#'
#'@description
#' `kobotools_api` is a wrapper for kobotoolbox API `https://[kpi-url]/api/v2/assets.json`
#'
#'@details
#' The function takes two variables. First one is `url` which is the `[kpi-url]`. For most users it will be "kobo.humanitarianresponse.info" or
#' "kf.kobotoolbox.org". Former is the default. The second parameter is `simplified` which takes a logical value. If set to true,
#' the function will return selected values from the parsed data and return a data frame. When set to false, a json will be returned with
#' all the details.
#'
#' @param url The `[kpi-url]` of kobotoolbox. Default is "kobo.humanitarianresponse.info"
#' @param simplified A logical value, default is true
#' @param uname takes the username
#' @param pwd takes the password
#' @param encoding is the encoding to be used. Default is "UTF-8".
#'
#' @return The function returns the asset details from the API, inform of a data frame or json.
#'
#'
#'
#' @importFrom httr GET add_headers content progress stop_for_status warn_for_status
#' @importFrom jsonlite fromJSON
#' @import R6
#' @import curl
#' @import methods
#' @import mime
#' @import openssl
#' @import dplyr
#'
#'
#' @export

kobotools_api<- function(url="kobo.humanitarianresponse.info", simplified=TRUE, uname="", pwd="", encoding = "UTF-8") {


  if(!is.character(url)) stop("URL entered is not a string")
  if(!is.character(uname)) stop("uname (username) entered is not a string")
  if(!is.character(pwd)) stop("pwd (password) entered is not a string")
  if(is.null(url)) stop("URL empty")
  if(is.null(uname)) stop("uname (username) empty")
  if(is.null(pwd)) stop("pwd (password) empty")
  if(!is.logical(simplified)) stop("simplied can take only logical value")

  fullurl<-paste0("https://",url,"/api/v2/assets.json")
  respon.api<-GET(fullurl, authenticate(uname, pwd), progress())

  stop_for_status(respon.api, "extract asset details")
  parsed <- fromJSON(content(respon.api, "text", encoding = encoding), simplifyVector = FALSE)

  if(simplified==FALSE){
    return(parsed)
  } else {
    link<-NULL
    date_created<-NULL
    date_modified<-NULL
    owner<-NULL
    assetid<-NULL
    name<-NULL
    active<-NULL
    submissions<-NULL
    x<-parsed$count
    for(i in 1:x){
      link[i]<-parsed$results[[i]]$url
      date_created[i]<-parsed$results[[i]]$date_created
      date_modified[i]<-parsed$results[[i]]$date_modified
      owner[i]<-parsed$results[[i]]$owner__username
      assetid[i]<-parsed$results[[i]]$uid
      name[i]<-parsed$results[[i]]$name
      active[i]<-parsed$results[[i]]$deployment__active
      submissions[i]<-parsed$results[[i]]$deployment__submission_count
    }
    simp.parsed<- data.frame(name=name, asset=assetid, active=active,submissions=submissions, owner=owner,
                             date_created=date_created, date_modified=date_modified, URL=link)
    return(simp.parsed)
  }

}



#' Extract data from kobotoolbox
#'
#'@description
#' `kobotools_kpi_data` is a wrapper for kobotoolbox API `https://[URL]/api/v2/assets/{assetid}/data/`
#'
#'@details
#' The function takes the url as one of the inputs. And asset id as another. Both are strings. The asset id is found by running
#' the [kobotools_api()] function.Other parameters are username and password.
#'
#' @param url The `[kpi-url]` of kobotoolbox. Default is "kobo.humanitarianresponse.info"
#' @param assetid is the asset id of the asset for which the data is to be downloaded. The id can be found by running [kobotools_data_list_kc()]
#' @param uname is username of your kobotoolbox account
#' @param pwd is the password of the account
#' @param encoding is the encoding to be used. Default is "UTF-8".
#'
#' @return The function returns the data in json format
#'
#'
#' @importFrom httr GET content authenticate progress
#' @importFrom jsonlite fromJSON
#'
#'
#'
#' @export

kobotools_kpi_data<- function(assetid,url="kobo.humanitarianresponse.info", uname="", pwd="", encoding = "UTF-8") {

  if(!is.character(url)) stop("URL entered is not a string")
  if(!is.character(uname)) stop("uname (username) entered is not a string")
  if(!is.character(pwd)) stop("pwd (password) entered is not a string")
  if(!is.character(assetid)) stop("assetid entered in not string")
  if(is.null(url)) stop("URL empty")
  if(is.null(uname)) stop("uname (username) empty")
  if(is.null(pwd)) stop("pwd (password) empty")
  if(is.null(assetid)) stop("assetid empty")


  fullurl<-paste0("https://",url,"/api/v2/assets/",assetid,"/data/")
  respon.kpi<-GET(fullurl, authenticate(uname, pwd), progress())
  stop_for_status(respon.kpi, "extract data")
  dt<-content(respon.kpi, encoding = encoding)
  return(dt)
}

#' Know your API token or check
#'
#'@description
#' `get_kobo_token` is a wrapper for kobotoolbox API `https://"[url]"/token/?format=json`
#'
#'@details
#' The function returns the API token.
#'
#' @param url The `[url]` of kobotoolbox. Default is "kobo.humanitarianresponse.info".
#' @param uname is username of your kobotoolbox account
#' @param pwd is the password of the account
#' @param encoding is the encoding to be used. Default is "UTF-8".
#'
#' @return The function returns the token associated with your id and password in the given url.
#'
#'
#' @importFrom httr GET content authenticate progress
#' @importFrom jsonlite fromJSON
#'
#'
#' @export

get_kobo_token <- function(url="kobo.humanitarianresponse.info", uname="", pwd="", encoding = "UTF-8"){

  if(!is.character(url)) stop("URL entered is not a string")
  if(!is.character(uname)) stop("uname (username) entered is not a string")
  if(!is.character(pwd)) stop("pwd (password) entered is not a string")
  if(is.null(url)) stop("URL empty")
  if(is.null(uname)) stop("uname (username) empty")
  if(is.null(pwd)) stop("pwd (password) empty")

  fullurl<-paste0("https://",url,"/token/?format=json")
  respon.token<-GET(fullurl, authenticate(uname, pwd), progress())
  stop_for_status(respon.token,"extract token")
  tkn<-fromJSON(content(respon.token,"text", encoding = encoding))
  return(tkn)
}


#' See list of exports created
#'
#'@description
#' `kobo_exports` is a wrapper for kobotoolbox API `https://[url]/exports/`
#'
#'@details
#' The function returns the export views.
#'
#' @param url The `[url]` of kobotoolbox. Default is "kobo.humanitarianresponse.info".
#' @param uname is username of your kobotoolbox account
#' @param pwd is the password of the account
#' @param encoding is the encoding to be used. Default is "UTF-8".
#'
#' @return The function returns a list of exports available for the account id and password entered.
#'
#'
#' @importFrom httr GET content authenticate progress
#' @importFrom jsonlite fromJSON
#'
#' @export

kobo_exports <- function(url="kobo.humanitarianresponse.info", uname="", pwd="", encoding = "UTF-8"){

  if(!is.character(url)) stop("URL entered is not a string")
  if(!is.character(uname)) stop("uname (username) entered is not a string")
  if(!is.character(pwd)) stop("pwd (password) entered is not a string")
  if(is.null(url)) stop("URL empty")
  if(is.null(uname)) stop("uname (username) empty")
  if(is.null(pwd)) stop("pwd (password) empty")

  fullurl<-paste0("https://",url,"/exports/")
  respon.exp<-GET(fullurl, authenticate(uname, pwd), progress())
  stop_for_status(respon.exp,"extract export list.")
  exports<-fromJSON(content(respon.exp,"text", encoding = encoding))
  return(exports)
}


#' Create an export
#'
#'@description
#' `kobo_export_create` is a wrapper for kobotoolbox API `https://[url]/exports/..`
#'
#'@details
#' The function creates an export of survey data. If successful, returns the URL of the data that can be directly downloaded/read/imported in R.
#'
#' @param url The `[url]` of kobotoolbox Default is "kobo.humanitarianresponse.info".
#' @param uname is username of your kobotoolbox account
#' @param pwd is the password of the account
#' @param assetid is the id of the asset for which the export is to be created
#' @param all takes logical value in string format. Used to specify whether fields from all form versions will be included in the export.Acceptable values are "true" or "false". Default value is "false".
#' @param lang takes the language. For e.g. "English (en)".
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
#'
#'
#'
#' @return The function creates an export, prints and returns the URL of the export created
#'
#'
#' @importFrom httr POST content authenticate progress GET
#' @importFrom jsonlite fromJSON
#'
#' @export


kobo_export_create <- function(url="kobo.humanitarianresponse.info", uname="", pwd="",
                               assetid="", type= "csv", all="false", lang="_default",
                               hierarchy="false", include_grp="true",grp_sep="/",
                               multi_sel="both", fields=NULL, media_url ="true",
                               sub_ids=NULL, qry=NULL, flatten="true"){

  export_res<-export_creator(url=url, uname=uname, pwd=pwd,
                 assetid=assetid, type= type, all=all, lang=lang,
                 hierarchy=hierarchy, include_grp=include_grp,grp_sep=grp_sep,
                 multi_sel=multi_sel, fields=fields, media_url=media_url,
                 sub_ids=sub_ids, qry=qry, flatten=flatten)

  print(export_res[1])
  return(export_res[1])
}


#' Creates a data frame after creating a 'csv' export and downloading it
#'
#'@description
#' `kobo_df_download` is a wrapper for kobotoolbox API `https://[url]/exports/..`
#'
#'@details
#' The function creates an export of survey data in 'csv'. If successful, it attempts to download the data and and return a data frame.
#'
#' @param url The `[url]` of kobotoolbox Default is "kobo.humanitarianresponse.info".
#' @param uname is username of your kobotoolbox account
#' @param pwd is the password of the account
#' @param assetid is the id of the asset for which the export is to be created
#' @param all takes logical value in string format. Used to specify whether fields from all form versions will be included in the export.Acceptable values are "true" or "false". Default value is "false".
#' @param lang takes the language. For e.g. "English (en)".
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
#' @importFrom httr POST content authenticate progress DELETE GET
#' @importFrom jsonlite fromJSON
#' @importFrom utils read.csv
#'
#' @export


kobo_df_download <- function(url="kobo.humanitarianresponse.info", uname="", pwd="",
                               assetid="", all="false", lang="_default",
                               hierarchy="false", include_grp="true",grp_sep="/",fsep=";",
                              multi_sel="both", media_url="true", fields=NULL, sub_ids=NULL, sleep=2){

  new_export_details<-export_creator(url=url, uname=uname, pwd=pwd,
                                                 assetid=assetid, type= "csv", all=all, lang=lang,
                                                 hierarchy=hierarchy, include_grp=include_grp,grp_sep=grp_sep,
                                     multi_sel=multi_sel, fields=fields, media_url=media_url, sub_ids=sub_ids)

  Sys.sleep(sleep)
  dff<-export_downloader(new_export_details[[1]],fsep, uname, pwd, sleep)

  deleteact<-DELETE(url=paste0(url,"/api/v2/assets/",assetid,"/exports/",new_export_details[[2]],"/"),
                    authenticate(user=uname, password =pwd), progress())
  while(is.na(deleteact$status_code) | is.null(deleteact$status_code)){
    print("Attempting export deletion \n")
  }
  warn_for_status(deleteact,"delete export. Please delete manually.")
  if(deleteact$status_code==204) print("Export deleted from server")
  return(dff)
}

