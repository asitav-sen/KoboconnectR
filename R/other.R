.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Extract Kobotools data to R")
}
if (getRversion() >= "2.15.1") {
  utils::globalVariables("e")
}

is_response <- function(x) {
  class(x) == "response"
}



export_creator <- function(url = "eu.kobotoolbox.org", uname = "", pwd = "",
                           assetid = "", type = "csv", all = "false", lang = "_default",
                           hierarchy = "false", include_grp = "true", grp_sep = "/", multi_sel = "both",
                           fields = NULL, media_url = "true", sub_ids = NULL,
                           qry = NULL, flatten = "true", xls_typ_as_text = "false", sleep = 2) {

  # Validate input parameters
  if (!is.character(url)) stop("URL entered is not a string")
  if (!is.character(uname)) stop("uname (username) entered is not a string")
  if (!is.character(pwd)) stop("pwd (password) entered is not a string")
  if (!is.character(assetid)) stop("assetid (asset id) entered is not a string")
  if (!is.character(type)) stop("type entered is not a string")
  if (!is.character(all)) stop("all entered is not a string")
  if (!is.character(lang)) stop("lang entered is not a string")
  if (!is.character(hierarchy)) stop("hierarchy entered is not a string")
  if (!is.character(include_grp)) stop("include_grp entered is not a string")
  if (!is.character(multi_sel)) stop("multi_sel entered is not a string")
  if (!is.character(media_url)) stop("media_url entered is not a string")
  if (!all %in% c("true", "false")) stop("all should either be \"true\" or \"false\"")
  if (!type %in% c("csv", "xls", "geojson", "spss_labels")) stop("Invalid type entered")
  if (!hierarchy %in% c("true", "false")) stop("hierarchy should either be \"true\" or \"false\"")
  if (!include_grp %in% c("true", "false")) stop("include_grp should either be \"true\" or \"false\"")
  if (!media_url %in% c("true", "false")) stop("media_url should either be \"true\" or \"false\"")
  if (!flatten %in% c("true", "false")) stop("flatten should either be \"true\" or \"false\"")
  if (!multi_sel %in% c("both", "summary", "details")) stop("Invalid entry in multi_sel")

  if (is.null(url)) stop("URL empty")
  if (is.null(uname)) stop("uname (username) empty")
  if (is.null(pwd)) stop("pwd (password) empty")
  if (is.null(assetid)) stop("assetid (asset id) empty")
  if (is.null(type)) stop("type empty")
  if (is.null(all)) stop("all empty")
  if (is.null(lang)) stop("lang empty")
  if (is.null(hierarchy)) stop("hierarchy empty")
  if (include_grp == "true" & is.null(grp_sep)) stop("grp_sep cannot be empty")

  # Step 1: Get the list of existing exports
  pre_export <- kobo_exports(url = url, uname = uname, pwd = pwd, assetid = assetid)
  if (is.null(pre_export)) {
    stop("Failed to retrieve existing exports.")
  }
  pre_count <- ifelse(is.null(pre_export$count), 0, pre_export$count)


  # Step 2: Make the API request to create a new export
  fullurl <- paste0("https://", url, "/api/v2/assets/", assetid, "/exports/")
  task <- tryCatch(
    {
      response <- httr2::request(fullurl) |>
        httr2::req_auth_basic(uname, pwd) |>
        httr2::req_body_form(
          fields_from_all_versions = all,
          group_sep = grp_sep,
          hierarchy_in_labels = hierarchy,
          lang = lang,
          multiple_select = multi_sel,
          type = type,
          fields = if (is.null(fields)) NULL else paste(fields, collapse = ","),
          flatten = flatten,
          xls_types_as_text = xls_typ_as_text,
          include_media_url = media_url,
          submission_ids = if (is.null(sub_ids)) NULL else paste(sub_ids, collapse = ","),
          query = qry
        ) |>
        httr2::req_method("POST") |>
        httr2::req_timeout(sleep*3) |>
        httr2::req_perform()

      cat("Waiting..",sleep*3, "seconds \n")
      Sys.sleep(sleep*3)


      if (httr2::resp_status(response) != 201) {
        stop("Failed to send export instruction. HTTP status:", httr2::resp_status(response))
      }
      response
    },
    error = function(e) {
      cat("Error during export creation:", e$message, "\n")
      return(NULL)
    }
  )

  # Step 3: Wait for some time to allow the API to process
  if(!is.null(task)){
    if (task$status_code == 201) cat("Export instruction sent successfully. Waiting for result. \n")
    Sys.sleep(sleep*3)
  }


  # Step 4: Get the list of existing exports again
  post_export <- tryCatch(
    {
      x<-kobo_exports(url = url, uname = uname, pwd = pwd, assetid = assetid)
      Sys.sleep(sleep * 3)
      x
    },
    error = function(e) {
      cat("Error during retrieval of new exports:", e$message, "\n")
      return(NULL)
    }
  )

  if(!is.null(post_export)){

    post_count <- post_export$count

    #check is status is error

    if (post_export$results[[post_count]]$status == "error") {
      print(paste0(
        "Did not execute. Encountered ", post_export$results[[post_count]]$messages$error_type, ". ",
        post_export$results[[post_count]]$messages$error, ". \n"
      ))
    }
  }
  if (is.na(post_count)) {
    cat("Export process taking too long. Please increase the sleep value.\n")
    return(NULL)
  }


  # Step 6: Retrieve the URL and UID of the new export
  new_export <- post_export$results[[post_count]]
  new_url <- new_export$result
  new_uid <- new_export$uid

  result_list <- list(new_url, new_uid)
  print(result_list)

  return(result_list)
}




export_downloader <- function(exp.url, fsep, uname, pwd, sleep, type = "csv") {
  if (type == "csv") {
    tmp_file <- tempfile()
    tryCatch(
      {
        response <- httr2::request(exp.url) |>
          httr2::req_auth_basic(uname, pwd) |>
          httr2::req_method("GET") |>
          httr2::req_perform()

        Sys.sleep(sleep * 2)

        # Download the content as raw and write it to a temporary file
        dff <- httr2::resp_body_raw(response)
        writeBin(dff, tmp_file)

        Sys.sleep(sleep)

        # Read the CSV file
        dff <- read.csv(tmp_file, sep = fsep)
      },
      error = function(e) {
        cat("Error: ", e$message, "\n")
        return(NULL)
      }
    )
  }

  if (type == "xls") {
    tryCatch(
      {
        path <- "kobodl.xlsx"

        response <- httr2::request(exp.url) |>
          httr2::req_auth_basic(uname, pwd) |>
          httr2::req_method("GET") |>
          httr2::req_perform(path=path)

        Sys.sleep(sleep)

        # Read all sheets from the Excel file
        dff <- purrr::map(rlang::set_names(readxl::excel_sheets(path)), readxl::read_excel, path = path)
      },
      error = function(e) {
        cat("Error: ", e$message, "\n")
        return(NULL)
      }
    )
  }

  return(dff)
}
