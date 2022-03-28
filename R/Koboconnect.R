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
#' @importFrom httr GET add_headers content progress
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
  respon<-GET(fullurl, authenticate(uname, pwd), progress())

  if(respon$status_code!=200) stop(paste0("Error in GET response. Expected 200, recieved ",respon$status_code))

  parsed <- fromJSON(content(respon, "text", encoding = encoding), simplifyVector = FALSE)

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
#' @return The function returns the data
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
  respon<-GET(fullurl, authenticate(uname, pwd), progress())
  if(respon$status_code!=200) stop(paste0("Error in GET response. Expected 200, recieved ",respon$status_code))
  dt<-content(respon, encoding = encoding)
  dt
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
#' @export

get_kobo_token <- function(url="kobo.humanitarianresponse.info", uname="", pwd="", encoding = "UTF-8"){

  if(!is.character(url)) stop("URL entered is not a string")
  if(!is.character(uname)) stop("uname (username) entered is not a string")
  if(!is.character(pwd)) stop("pwd (password) entered is not a string")
  if(is.null(url)) stop("URL empty")
  if(is.null(uname)) stop("uname (username) empty")
  if(is.null(pwd)) stop("pwd (password) empty")

  fullurl<-paste0("https://",url,"/token/?format=json")
  respon<-GET(fullurl, authenticate(uname, pwd), progress())
  if(respon$status_code!=200) stop(paste0("Error in GET response. Expected 200, recieved ",respon$status_code))
  tkn<-fromJSON(content(respon,"text", encoding = encoding))
  tkn
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
#' @return The function returns the token associated with your id and password in the given url.
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
  respon<-GET(fullurl, authenticate(uname, pwd), progress())
  if(respon$status_code!=200) stop(paste0("Error in GET response. Expected 200, recieved ",respon$status_code))
  exports<-fromJSON(content(respon,"text", encoding = encoding))
  exports
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
#' @param type is the type of export to be created. For e.g. "xls", "csv" or "geojson".
#' @param all takes logical value in string format. "true" indicates the field from all versions of the asset to be used. Defaults to "false".
#' @param lang takes the language. For e.g. "English (en)".
#' @param heirarchy takes logical value in string format. "true" indicates hierarchy will be used in all labels. Default value is "false".
#' @param grp_sep is the group separator. Default value is "/".
#' @param include_grp defines whether or not to include groups. Default value is "true".
#'
#' @return The function returns the token associated with your id and password in the given url.
#'
#'
#' @importFrom httr POST content authenticate progress
#' @importFrom jsonlite fromJSON
#'
#' @export


kobo_export_create <- function(url="kobo.humanitarianresponse.info", uname="", pwd="",
                               assetid="", type= "csv", all="false", lang="_default",
                               hierarchy="false", include_grp="true",grp_sep="/"){

  if(!is.character(url)) stop("URL entered is not a string")
  if(!is.character(uname)) stop("uname (username) entered is not a string")
  if(!is.character(pwd)) stop("pwd (password) entered is not a string")
  if(!is.character(assetid)) stop("assetid (asset id) entered is not a string")
  if(!is.character(type)) stop("type entered is not a string")
  if(!is.character(all)) stop("all entered is not a string")
  if(!is.character(lang)) stop("lang entered is not a string")
  if(!is.character(hierarchy)) stop("hierarchy entered is not a string")
  if(!is.character(grp_sep)) stop("grp_sep entered is not a string")
  if(!is.character(include_grp)) stop("include_grp entered is not a string")

  if(is.null(url)) stop("URL empty")
  if(is.null(uname)) stop("uname (username) empty")
  if(is.null(pwd)) stop("pwd (password) empty")
  if(is.null(assetid)) stop("assetid (asset id) empty")
  if(is.null(type)) stop("type empty")
  if(is.null(all)) stop("all empty")
  if(is.null(lang)) stop("lang empty")
  if(is.null(hierarchy)) stop("hierarchy empty")
  if(is.null(grp_sep)) stop("grp_sep empty")
  if(is.null(include_grp)) stop("include_grp empty")

  pre_export<-kobo_exports(url=url, uname=uname, pwd=pwd)
  pre_count<-as.integer(pre_export$count)

  fullurl<-paste0("https://",url,"/exports/")
  task<-POST(fullurl, authenticate(uname, pwd),
             body=list(
               source=paste0("https://",url,"/assets/",assetid,"/"),
               type=type, fields_from_all_versions= all, lang=lang, hierarchy_in_labels = hierarchy,
               include_groups=include_grp,
               group_sep=grp_sep
             ),
             progress())
  if(task$status_code!=201) stop(paste0("Error in GET response. Expected 201, recieved ",respon$status_code))

  if(task$status_code==201) cat("Export instruction sent succesfully. Waiting for result. \n")

  post_export<-kobo_exports(url=url, uname=uname, pwd=pwd)
  post_count<-as.integer(post_export$count)

  while(post_count<=pre_count){
    cat("\n ...Execution in Progress \n")
    post_export<-kobo_exports(url=url, uname=uname, pwd=pwd)
    post_count<-as.integer(post_export$count)
  }
  rm(pre_export)
  rm(pre_count)

  if(post_export$results$status[post_count]=="error"){
    stop(paste0("Did not execute. Encountered ",post_export$results$messages$error_type[post_count],". ",
           post_export$results$messages$error[post_count],". \n"))
  }


  print("Export successful")
  # print(post_count)
  while(is.na(post_export$results$result[post_count])){
    # print("waiting.. \n")
    post_export<-kobo_exports(url=url, uname=uname, pwd=pwd)
  }
  print(post_export$results$result[post_count])
  return(post_export$results$result[post_count])

}


