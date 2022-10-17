.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Extract Kobotools data to R")
}

is_response <- function(x) {
  class(x) == "response"
}

export_creator <- function(url="kobo.humanitarianresponse.info", uname="", pwd="",
                               assetid="", type= "csv", all="false", lang="_default",
                               hierarchy="false", include_grp="true",grp_sep="/", multi_sel="both",
                           fields=NULL, media_url="true", sub_ids= NULL,
                           qry=NULL, flatten="true", xls_typ_as_text="false", sleep=2){

  if(!is.character(url)) stop("URL entered is not a string")
  if(!is.character(uname)) stop("uname (username) entered is not a string")
  if(!is.character(pwd)) stop("pwd (password) entered is not a string")
  if(!is.character(assetid)) stop("assetid (asset id) entered is not a string")
  if(!is.character(type)) stop("type entered is not a string")
  if(!is.character(all)) stop("all entered is not a string")
  if(!is.character(lang)) stop("lang entered is not a string")
  if(!is.character(hierarchy)) stop("hierarchy entered is not a string")
  #if(!is.character(grp_sep)) stop("grp_sep entered is not a string")
  if(!is.character(include_grp)) stop("include_grp entered is not a string")
  if(!is.character(multi_sel)) stop("multi_sel entered is not a string")
  if(!is.character(media_url)) stop("media_url entered is not a string")
  if(!all %in% c("true","false")) stop("all should either be \"true\" or \"false\"")
  if(!type %in% c("csv","xls","geojson","spss_labels")) stop("Invalid type entered")
  if(!hierarchy %in% c("true","false")) stop("hierarchy should either be \"true\" or \"false\"")
  if(!include_grp %in% c("true","false")) stop("include_grp should either be \"true\" or \"false\"")
  if(! media_url %in% c("true","false")) stop("media_url should either be \"true\" or \"false\"")
  if(!flatten %in% c("true","false")) stop("flatten should either be \"true\" or \"false\"")
  if(!multi_sel %in% c("both","summary","details")) stop("Invalid entry in multi_sel")

  if(is.null(url)) stop("URL empty")
  if(is.null(uname)) stop("uname (username) empty")
  if(is.null(pwd)) stop("pwd (password) empty")
  if(is.null(assetid)) stop("assetid (asset id) empty")
  if(is.null(type)) stop("type empty")
  if(is.null(all)) stop("all empty")
  if(is.null(lang)) stop("lang empty")
  if(is.null(hierarchy)) stop("hierarchy empty")
  #if(is.null(grp_sep)) stop("grp_sep empty")
  if(is.null(include_grp)) stop("include_grp empty")
  if(include_grp=="true" & is.null(grp_sep)) stop("grp_sep cannot be empty")
  #if(include_grp=="false" & !is.null(grp_sep)) stop("grp_sep should be empty if include_grp is false")

  pre_export<- kobo_exports(url=url, uname=uname, pwd=pwd)
  pre_count<-ifelse(is.null(pre_export$count),0,pre_export$count)
  pre_time<-ifelse(is.null(pre_export$results$date_created),0,max(pre_export$results$date_created))
  # pre_uid<-pre_export$results[pre_export$results$date_created==max(pre_export$results$date_created),]$uid
  # pre_create<-max(pre_export$results$date_created)
  fullurl<-paste0("https://",url,"/api/v2/assets/",assetid,"/exports/")
  task<-tryCatch(
    expr = {
      POST(fullurl, authenticate(uname, pwd),
           body=list(
             #source=paste0("https://",url,"/assets/",assetid,"/"),
             fields_from_all_versions= all,
             group_sep=grp_sep,
             hierarchy_in_labels = hierarchy,
             lang=lang,
             multiple_select=multi_sel,
             type=type,
             fields=fields,
             flatten= flatten,
             xls_types_as_text=xls_typ_as_text,
             include_media_url=media_url,
             submission_ids=sub_ids,
             query=qry
           ),
           timeout(sleep*2))
    },
    error=function(x){
      print("There was some error")
      return(NULL)
    }
  )
  #stop_for_status(task, "create export")
  if(task$status_code==201) cat("Export instruction sent successfully. Waiting for result. \n")
  Sys.sleep(sleep)

  post_export<-kobo_exports(url=url, uname=uname, pwd=pwd)
  Sys.sleep(sleep*2)
  post_count<-post_export$count
  post_time<-max(post_export$results$date_created)


  if(post_time<=pre_time){
    cat("Execution in Progress...")
    Sys.sleep(sleep*2)
  }

  if(post_time<=pre_time){
    cat("Timeout")
    return(NULL)
  } else {
    if(post_export$results$status[post_count]=="error"){
      print(paste0("Did not execute. Encountered ",post_export$results$messages$error_type[post_count],". ",
                  post_export$results$messages$error[post_count],". \n"))
      return(NULL)
    } else {
      print("Export successful")

      if(is.na(post_export$results$result[post_count])){
        print("waiting..")
        Sys.sleep(sleep*5)
      }
      if(is.na(post_export$results$result[post_count])){
        print("Could not get export list")
        return(NULL)
      } else {
        print(post_export$results$result[post_count])
        new_url=post_export$results$result[post_count]
        uid=post_export$results$uid[post_count]
        created_list<-list(new_url,uid)
        return(created_list)
      }
    }
  }

}


export_downloader<-function(exp.url, fsep, uname, pwd, sleep, type="csv"){

  if(type=="csv"){
    tmp_file <- tempfile()
    df<-httr::GET(exp.url, httr::authenticate(user=uname, password = pwd),progress())
    Sys.sleep(sleep)
    dff<-httr::content(df, type="raw",encoding = "UTF-8")
    Sys.sleep(sleep)
    writeBin(dff, tmp_file)
    dff<-read.csv(tmp_file, sep=fsep)
  }

  if(type=="xls"){
    httr::GET(exp.url, httr::authenticate(user=uname, password = pwd),
              httr::write_disk("kobodl.xlsx", overwrite = TRUE), progress())
    path<-"kobodl.xlsx"
    dff<-
      path |>
      readxl::excel_sheets() |>
      rlang::set_names() |>
      purrr::map(readxl::read_excel, path=path)
  }

  return(dff)
}
