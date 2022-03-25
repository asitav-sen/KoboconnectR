#' Check kobotools API and retrieve overall info about the projects/assets
#'
#'@description
#' `kobotools_api` is a wrapper for kobotools API `https://[kpi-url]/api/v2/assets.json`
#'
#'@details
#' The function takes two variables. First one is `url` which is the `[kpi-url]`. For most users it will be "kobo.humanitarianresponse.info" or
#' "kf.kobotoolbox.org". Former is the default. The second parameter is `simplified` which takes a logical value. If set to true,
#' the function will return selected values from the parsed data and return a data frame. When set to false, a json will be returned with
#' all the details.
#'
#' @param url The `[kpi-url]` of kobotools. Default is "kobo.humanitarianresponse.info"
#' @param simplified A logical value, default is true
#' @param uname takes the username
#' @param pwd takes the password
#' @return The function returns the asset details from the API, inform of a data frame or json.
#'
#'
#'
#' @importFrom httr GET add_headers content
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

kobotools_api<- function(url="kobo.humanitarianresponse.info", simplified=T, uname="scary_scarecrow", pwd="sybWE6USkFxDsr4") {


  if(!is.character(url)) stop("URL entered is not a string")
  if(!is.character(uname)) stop("uname (username) entered is not a string")
  if(!is.character(pwd)) stop("pwd (password) entered is not a string")
  if(is.null(url)) stop("URL empty")
  if(is.null(uname)) stop("uname (username) empty")
  if(is.null(pwd)) stop("pwd (password) empty")
  if(!is.logical(simplified)) stop("simplied can take only logical value")

  fullurl<-paste0("https://",url,"/api/v2/assets.json")
  respon<-GET(fullurl, authenticate(uname, pwd))

  if(respon$status_code!=200) stop(paste0("Error in GET response. Expected 200, recieved ",respon$status_code))

  parsed <- fromJSON(content(respon, "text"), simplifyVector = FALSE)

  if(simplified==F){
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



#' Extract data from kobotools
#'
#'@description
#' `kobotools_kpi_data` is a wrapper for kobotools API `https://[URL]/api/v2/assets/{assetid}/data/`
#'
#'@details
#' The function takes the url as one of the inputs. And asset id as another. Both are strings. The asset id is found by running
#' the [kobotools_api()] function.Other parameters are username and password.
#'
#' @param url The `[kpi-url]` of kobotools. Default is "kobo.humanitarianresponse.info"
#' @param assetid is the asset id of the asset for which the data is to be downloaded. The id can be found by running [kobotools_data_list_kc()]
#' @param uname is username of your kobotool account
#' @param pwd is the password of the account
#'
#' @return The function returns the data
#'
#'
#' @importFrom httr GET content authenticate
#' @importFrom jsonlite fromJSON
#'
#'
#'
#' @export

kobotools_kpi_data<- function(assetid,url="kobo.humanitarianresponse.info", uname="", pwd="") {

  if(!is.character(url)) stop("URL entered is not a string")
  if(!is.character(uname)) stop("uname (username) entered is not a string")
  if(!is.character(pwd)) stop("pwd (password) entered is not a string")
  if(!is.character(assetid)) stop("assetid entered in not string")
  if(is.null(url)) stop("URL empty")
  if(is.null(uname)) stop("uname (username) empty")
  if(is.null(pwd)) stop("pwd (password) empty")
  if(is.null(assetid)) stop("assetid empty")


  fullurl<-paste0("https://",url,"/api/v2/assets/",assetid,"/data/")
  respon<-GET(fullurl, authenticate(uname, pwd))
  if(respon$status_code!=200) stop(paste0("Error in GET response. Expected 200, recieved ",respon$status_code))
  dt<-content(respon)
  dt
}

#' Extract data from kobotools
#'
#'@description
#' `get_kobo_token` is a wrapper for kobotools API `https://"[url]"/token/?format=json`
#'
#'@details
#' The function returns the API token.
#'
#' @param url The `[url]` of kobotools. Default is "kobo.humanitarianresponse.info".
#' @param uname is username of your kobotool account
#' @param pwd is the password of the account
#'
#' @return The function returns the token associated with your id and password in the given url.
#'
#'
#' @importFrom httr GET content authenticate
#' @importFrom jsonlite fromJSON
#'
#' @export

get_kobo_token <- function(url="kobo.humanitarianresponse.info", uname="", pwd=""){

  if(!is.character(url)) stop("URL entered is not a string")
  if(!is.character(uname)) stop("uname (username) entered is not a string")
  if(!is.character(pwd)) stop("pwd (password) entered is not a string")
  if(is.null(url)) stop("URL empty")
  if(is.null(uname)) stop("uname (username) empty")
  if(is.null(pwd)) stop("pwd (password) empty")

  fullurl<-paste0("https://",url,"/token/?format=json")
  respon<-GET(fullurl, authenticate(uname, pwd))
  if(respon$status_code!=200) stop(paste0("Error in GET response. Expected 200, recieved ",respon$status_code))
  tkn<-fromJSON(content(respon,"text"))
  tkn
}



