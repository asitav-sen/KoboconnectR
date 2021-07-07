# #' Set encrypted token in Renviron
# #'
# #'@description
# #' `set_kobo_token` encrypts your token and stores in Renviron. It can later be retrieved by using `get_kobo_token`.
# #'
# #'@details
# #' The function takes two parameters. The first is the `token` which must be a string/character wrapped in double quotes.
# #' The second is the `samplekey`. The default is set to "abracadabra". One may change it.
# #' @param token The Kobotoools API token as string
# #' @param samplekey A string that you want to set as key
# #' @return The function encrypts the token using the key provided and stores in Renviron.
# #' @examples
# #' /not run
# #' set_kobo_token("d7a1faf575e047d4ebb1ed")
# #' set_kobo_token("mykobotoolstoken", "mykey")
# #'
# #' @seealso [get_kobo_token()]
# #'
# #' @importFrom safer encrypt_string
# #'
# #' @export
#
# set_kobo_token<-function(token=NULL, samplekey="abracadabra"){
#
#   # check if token is string
#   if (!is.character(token)) {
#     stop("Yeah. I only work with Good characters. Try a new one.",
#          call. = FALSE)
#   }
#
#   # check if null
#   if (is.null(token)) {
#     stop("I don't like emptiness. Try again.",
#          call. = FALSE)
#   }
#
#   # Encrypt key
#   kobo_token_key<-samplekey
#
#   # Encrypt token
#   encrypted_kobo_token<-encrypt_string(token, samplekey)
#
#   # Store in Renviron
#   Sys.setenv(kobo_key=kobo_token_key, kobo_token=encrypted_kobo_token)
#
# }
#
#
# #' Retrieve encrypted token from Renviron
# #'
# #'@description
# #' `get_kobo_token` retrieves your kobotools token from Renviron, which was set using `set_kobo_token`.
# #'
# #'@details The function does not take in input. It simply returns the kobotools token saved in the Renviron.
# #'
# #' @return Returns the original token as a string
# #' @examples
# #' get_kobo_token()
# #'
# #' @seealso [set_kobo_token()]
# #'
# #' @importFrom safer decrypt_string
# #'
# #' @export
#
# get_kobo_token <- function() {
#
#
#   # get the variables from R environment
#   kobo_token_key<-Sys.getenv("kobo_key")
#   encrypted_kobo_token<-Sys.getenv("kobo_token")
#
#   # Decrypt the variable
#   kat <- decrypt_string(encrypted_kobo_token, kobo_token_key)
#
#   # Return token
#   kat
# }
#
#
# #' Set userid and password
# #'
# #'@description
# #' `set_kobo_id_pass` encrypts your kobotools id and password in Renviron. It can later be retrieved by using `get_kobo_id` and `get_kobo_pass`.
# #'
# #'@details
# #' The function takes three parameters. The first is the `id` which must be a string/character wrapped in double quotes.
# #' The second one is the `password` which must also be a string.
# #' The third is the `samplekey`. The default is set to "abracadabra". One may change it.
# #'
# #' @param id The Kobotoools id as string
# #' @param password The Kobotools password as string
# #' @param samplekey A string that you want to set as key
# #'
# #' @return The function encrypts the id and password provided and stores in Renviron.
# #' @examples
# #' set_kobo_id_pass("myuserid","mypassword")
# #' set_kobo_token("myuserid","mypassword", "mykey")
# #'
# #' @seealso [get_kobo_id()], [get_kobo_pass()]
# #'
# #' @importFrom safer encrypt_string
# #'
# #' @export
#
# set_kobo_id_pass<-function(id=NULL, password=NULL, samplekey="abracadabra"){
#
#   # check if id and passpword is string
#   if (!(is.character(id) | is.character(password))) {
#     stop("Yeah. I only work with Good characters. Try a new one.",
#          call. = FALSE)
#   }
#
#   # check if null
#   if (is.null(id) | is.null(password)) {
#     stop("I don't like emptiness. Try again.",
#          call. = FALSE)
#   }
#
#   # Create encrypted key
#   kobo_idpass_key<-samplekey
#   encrypted_kobo_id<-encrypt_string(id, samplekey)
#   encrypted_kobo_password<-encrypt_string(password, samplekey)
#   Sys.setenv(kobo_idpass_key=kobo_idpass_key, kobo_id=encrypted_kobo_id, kobo_pass=encrypted_kobo_password)
#
# }
#
# #' Retrieve kobotools id from Renviron which was set using `set_kobo_id_pass`
# #'
# #'@description
# #' `get_kobo_id` retrieves your kobotools id from Renviron, which was set using `set_kobo_id_pass`.
# #'
# #'@details The function does not take in input. It simply returns the kobotools id saved in the Renviron.
# #'
# #' @return Returns the original id as a string
# #' @examples
# #' get_kobo_id()
# #'
# #' @seealso [set_kobo_id_pass()]
# #'
# #' @importFrom safer decrypt_string
# #'
# #' @export
#
# get_kobo_id <- function() {
#
#
#   # get the variables from R environment
#   kobo_id_key<-Sys.getenv("kobo_idpass_key")
#   encrypted_kobo_id<-Sys.getenv("kobo_id")
#
#   # Decrypt the variable
#   kat <- decrypt_string(encrypted_kobo_id, kobo_id_key)
#
#   # Return token
#   kat
# }
#
#
# #' Retrieve kobotools password from Renviron which was set using `set_kobo_id_pass`
# #'
# #'@description
# #' `get_kobo_pass` retrieves your kobotools password from Renviron, which was set using `set_kobo_id_pass`.
# #'
# #'@details The function does not take in input. It simply returns the kobotools password saved in the Renviron.
# #'
# #' @return Returns the original password as a string
# #' @examples
# #' get_kobo_pass()
# #'
# #' @seealso [set_kobo_id_pass()]
# #'
# #' @importFrom safer decrypt_string
# #'
# #' @export
#
# get_kobo_pass <- function() {
#
#
#   # get the variables from R environment
#   kobo_id_key<-Sys.getenv("kobo_idpass_key")
#   encrypted_kobo_pass<-Sys.getenv("kobo_pass")
#
#   # Decrypt the variable
#   kat <- decrypt_string(encrypted_kobo_pass, kobo_id_key)
#
#   # Return token
#   kat
# }
