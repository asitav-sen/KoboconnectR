library(httr)
library(safer)
library(jsonlite)
library(tidyverse)

#' Set encrypted token in Renviron
#'
#'@description
#' `set_kobo_token` encrypts your token and stores in Renviron. It can later be retrieved by using `get_kobo_token`.
#'
#'@details
#' The function takes two parameters. The first is the `token` which must be a string/character wrapped in double quotes.
#' The second is the `samplekey`. The default is set to "abracadabra". One may change it.
#' @param token The Kobotoools API token as string
#' @param samplekey A string that you want to set as key
#' @return The function encrypts the token using the key provided and stores in Renviron.
#' @examples
#' set_kobo_token("d7a1faf575e047d4ebb1ed")
#' set_kobo_token("mykobotoolstoken", "mykey")
#'
#' @seealso [get_kobo_token()]
#'
#' @importFrom safer encrypt_string
#'
#' @export

set_kobo_token<-function(token=NULL, samplekey="abracadabra"){

  # check if token is string
  if (!is.character(token)) {
    stop("Yeah. I only work with Good characters. Try a new one.",
         call. = FALSE)
  }

  # check if null
  if (is.null(token)) {
    stop("I don't like emptiness. Try again.",
         call. = FALSE)
  }

  # Encrypt key
  kobo_token_key<-samplekey

  # Encrypt token
  encrypted_kobo_token<-encrypt_string(token, samplekey)

  # Store in Renviron
  Sys.setenv(kobo_key=kobo_token_key, kobo_token=encrypted_kobo_token)

}


#' Retrieve encrypted token from Renviron
#'
#'@description
#' `get_kobo_token` retrieves your kobotools token from Renviron, which was set using `set_kobo_token`.
#'
#'@details The function does not take in input. It simply returns the kobotools token saved in the Renviron.
#'
#' @return Returns the original token as a string
#' @examples
#' get_kobo_token()
#'
#' @seealso [set_kobo_token()]
#'
#' @importFrom safer decrypt_string
#'
#' @export

get_kobo_token <- function() {


  # get the variables from R environment
  kobo_token_key<-Sys.getenv("kobo_key")
  encrypted_kobo_token<-Sys.getenv("kobo_token")

  # Decrypt the variable
  kat <- decrypt_string(encrypted_kobo_token, kobo_token_key)

  # Return token
  kat
}


#' Set userid and password
#'
#'@description
#' `set_kobo_id_pass` encrypts your kobotools id and password in Renviron. It can later be retrieved by using `get_kobo_id` and `get_kobo_pass`.
#'
#'@details
#' The function takes three parameters. The first is the `id` which must be a string/character wrapped in double quotes.
#' The second one is the `password` which must also be a string.
#' The third is the `samplekey`. The default is set to "abracadabra". One may change it.
#'
#' @param id The Kobotoools id as string
#' @param password The Kobotools password as string
#' @param samplekey A string that you want to set as key
#'
#' @return The function encrypts the id and password provided and stores in Renviron.
#' @examples
#' set_kobo_id_pass("myuserid","mypassword")
#' set_kobo_token("myuserid","mypassword", "mykey")
#'
#' @seealso [get_kobo_id()], [get_kobo_pass()]
#'
#' @importFrom safer encrypt_string
#'
#' @export

set_kobo_id_pass<-function(id=NULL, password=NULL, samplekey="abracadabra"){

  # check if id and passpword is string
  if (!(is.character(id) | is.character(password))) {
    stop("Yeah. I only work with Good characters. Try a new one.",
         call. = FALSE)
  }

  # check if null
  if (is.null(id) | is.null(password)) {
    stop("I don't like emptiness. Try again.",
         call. = FALSE)
  }

  # Create encrypted key
  kobo_idpass_key<-samplekey
  encrypted_kobo_id<-encrypt_string(id, samplekey)
  encrypted_kobo_password<-encrypt_string(password, samplekey)
  Sys.setenv(kobo_idpass_key=kobo_idpass_key, kobo_id=encrypted_kobo_id, kobo_pass=encrypted_kobo_password)

}

#' Retrieve kobotools id from Renviron which was set using `set_kobo_id_pass`
#'
#'@description
#' `get_kobo_id` retrieves your kobotools id from Renviron, which was set using `set_kobo_id_pass`.
#'
#'@details The function does not take in input. It simply returns the kobotools id saved in the Renviron.
#'
#' @return Returns the original id as a string
#' @examples
#' get_kobo_id()
#'
#' @seealso [set_kobo_id_pass()]
#'
#' @importFrom safer decrypt_string
#'
#' @export

get_kobo_id <- function() {


  # get the variables from R environment
  kobo_id_key<-Sys.getenv("kobo_idpass_key")
  encrypted_kobo_id<-Sys.getenv("kobo_id")

  # Decrypt the variable
  kat <- decrypt_string(encrypted_kobo_id, kobo_id_key)

  # Return token
  kat
}


#' Retrieve kobotools password from Renviron which was set using `set_kobo_id_pass`
#'
#'@description
#' `get_kobo_pass` retrieves your kobotools password from Renviron, which was set using `set_kobo_id_pass`.
#'
#'@details The function does not take in input. It simply returns the kobotools password saved in the Renviron.
#'
#' @return Returns the original password as a string
#' @examples
#' get_kobo_pass()
#'
#' @seealso [set_kobo_id_pass()]
#'
#' @importFrom safer decrypt_string
#'
#' @export

get_kobo_pass <- function() {


  # get the variables from R environment
  kobo_id_key<-Sys.getenv("kobo_idpass_key")
  encrypted_kobo_pass<-Sys.getenv("kobo_pass")

  # Decrypt the variable
  kat <- decrypt_string(encrypted_kobo_pass, kobo_id_key)

  # Return token
  kat
}


#' Check kobotools API and retrieve overall info about the projects/assets
#'
#'@description
#' `kobotools_api` is a wrapper for kobotools API `https://[kpi-url]/api/v2/assets.json`
#'
#'@details
#' The function takes two variables. First one is `url` which is the `[kpi-url]`. FOr most users it will be "kobo.humanitarianresponse.info" or
#' "kf.kobotoolbox.org". Former is the default. THe second parameter is `simplified` which takes a logical value. If set to true,
#' the function will return selected values from the parsed data and return a data frame. WHen set to false, a json will be reurned with
#' all the details. Token needs to be set before the function can be used.
#'
#' @param url The `[kpi-url]` of kobotools. Default is "kobo.humanitarianresponse.info"
#' @param simplified A logical value, default is false
#' @param kobo.token a string (the token value)
#' @return The function returns the asset details from the API, inform of a data frame or json.
#'
#' @examples
#' kobotools_api()
#' kobotools_api(url="kobo.humanitarianresponse.info", simplified=T)
#'
#' @seealso [set_kobo_token()]
#'
#' @importFrom httr GET add_headers content
#' @importFrom jsonlite fromJSON
#'
#' @export

kobotools_api<- function(url="kobo.humanitarianresponse.info", simplified=F, kobo.token=get_kobo_token()) {

  fullurl<-paste0("https://",url,"/api/v2/assets.json")
  auth.token<-paste0("Token ",kobo.token)
  respon<-GET(fullurl, add_headers(.headers=c(Authorization=auth.token)), user.agent)
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



#' Lists available data in your kobotool
#'
#'@description
#' `kobotools_data_list_kc` is a wrapper for kobotools API `https://kc.humanitarianresponse.info/api/v1/data`
#'
#'@details
#' The function takes the `kc` url as the input. It retrieves the user id and password set using `set_kobo_id_pass`.
#' This function uses the old api (v1) of kobotools. Similar version of v2 and `kpi` was not found in the documentation yet.
#' An alternative way to use the `kpi` and v2 is to use  [kobotools_data_list_kpi()]. The utility of both these function is to
#' find out the link to the submitted data files. In this function the returned `id` from the asset list is particularly
#' important which will be used in [kobotools_kc_download()] to download csv files.
#'
#' @param url The `[kc-url]` of kobotools. Default is "kc.humanitarianresponse.info"
#' @param user.id takes the user id in string
#' @param pass takes the password as string
#' @return The function returns the list of assets/projects in kobotools, in form of a json.
#'
#' @examples
#' kobotools_data_list_kc("kc.humanitarianresponse.info")
#'
#' @seealso [kobotools_kc_download()]
#'
#' @importFrom httr GET content authenticate
#' @importFrom jsonlite fromJSON
#'
#' @export


kobotools_data_list_kc<- function(url="kc.humanitarianresponse.info", user.id=get_kobo_id(), pass=get_kobo_pass()) {
  fullurl<-paste0("https://",url,"/api/v1/data?format=json")
  userid<- user.id
  password<- pass
  respon<-GET(fullurl, authenticate(userid,password),user.agent)
  dt<-fromJSON(content(respon, "text"), simplifyVector = FALSE)
  dt

}


#' Download data from kobotools
#'
#'@description
#' `kobotools_kc_download` is a wrapper for kobotools API `https://kc.humanitarianresponse.info/api/v1/data/[assetid]`
#'
#'@details
#' The function takes the `kc` url as one of the inputs. And assetid as another. Both are strings. The asset id is found by running
#' the [kobotools_data_list_kc()] function.
#'
#' @param url The `[kc-url]` of kobotools. Default is "kc.humanitarianresponse.info"
#' @param assetid is the asset id of the asset for which the data is to be downloaded. The id can be found by running [kobotools_data_list_kc()]
#' @param user.id takes the user id in string
#' @param pass takes the password as string
#' @return The function returns the data
#'
#' @examples
#' /notrun
#' kobotools_kc_download(assetid="12345")
#'
#' @seealso [kobotools_data_list_kc()]
#'
#' @importFrom httr GET content authenticate
#' @importFrom jsonlite fromJSON
#' @importFrom utils read.csv
#'
#' @export


kobotools_kc_download<- function(assetid=NULL, url="kc.humanitarianresponse.info", user.id=get_kobo_id(), pass=get_kobo_pass()){

  fullurl<-paste0("https://",url,"/api/v1/data/",assetid)
  userid<- user.id
  password<- pass
  respon<-GET(fullurl, authenticate(userid,password), query=list(key="format",value="csv"),user.agent)
  dt<-fromJSON(content(respon,"text"))
  dt

}

#' Extract data from kobotools
#'
#'@description
#' `kobotools_kpi_data` is a wrapper for kobotools API `https://restoreprivacy.com/linkedin-data-leak-700-million-users/`
#'
#'@details
#' The function takes the `kpi` url as one of the inputs. And assetid as another. Both are strings. The asset id is found by running
#' the [kobotools_api()] function. Another parameter `tokenvalue` takes string as a value. One can use [get_kobo_token()] to
#' extract the stored token value from the Renviron.
#'
#' @param url The `[kpi-url]` of kobotools. Default is "kobo.humanitarianresponse.info"
#' @param assetid is the asset id of the asset for which the data is to be downloaded. The id can be found by running [kobotools_data_list_kc()]
#' @param forma is a string defining the format of data. Default is "json"
#' @param tokenvalue is a the value of the kobotools API token
#'
#' @return The function returns the data
#'
#' @examples
#' /notrun
#' kobotools_kpi_data(assetid="12345", url="kobo.humanitarianresponse.info", forma="json")
#'
#' @seealso [kobotools_data_list_kc()]
#'
#' @importFrom httr GET content authenticate
#' @importFrom jsonlite fromJSON
#'
#' @export

kobotools_kpi_data<- function(assetid,url="kobo.humanitarianresponse.info", forma="json", tokenvalue=get_kobo_token()) {

  fullurl<-paste0("https://",url,"/api/v2/assets/",assetid,"/data.",forma)
  auth.token<-paste0("Token ",tokenvalue)
  respon<-GET(fullurl, add_headers(.headers=c(Authorization=auth.token)),user.agent)
  dt<-content(respon)
  dt
}







