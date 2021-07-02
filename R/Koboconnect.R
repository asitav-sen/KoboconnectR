library(httr)
library(safer)



# Function to set token

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

  # Create encrypted key
  kobo_token_key<-samplekey
  encrypted_kobo_token<-encrypt_string(token, samplekey)
  Sys.setenv(kobo_key=kobo_token_key, kobo_token=encrypted_kobo_token)

}


# Function to get token from environment

get_kobo_token <- function() {


  # get the variables from R environment
  kobo_token_key<-Sys.getenv("kobo_key")
  encrypted_kobo_token<-Sys.getenv("kobo_token")

  # Decrypt the variable
  kat <- decrypt_string(encrypted_kobo_token, kobo_token_key)

  # Return token
  kat
}




get_kobo_token()
set_kobo_token("d7a1faf575e047d4ebb1519469454297dcd012ed")


kobotools_api<- function(url="kobo.humanitarianresponse.info") {

  fullurl<-paste0("https://",url,"/api/v2/assets.json")
  auth.token<-paste0("Token ",get_kobo_token())
  respon<-GET(fullurl, add_headers(.headers=c(Authorization=auth.token)), ua)
  respon

}

kobotools_api()

kobotools_list_assets<-function(){

  fullurl<-paste0("https://",url,"/api/v2/assets.json")
  auth.token<-paste0("Token ",get_kobo_token())
  respon<-GET(fullurl, add_headers(.headers=c(Authorization=auth.token)))
  respon
}


