#' finalize training booking submission
#' @description
#' @author Farid Azouaou
#' @param candidate_submission
#' @export

finalize_submission <- function(registration_questions = NULL,candidate_submission = NULL){
  training_file <- "fair_training_candidates.csv"
  candidate_submission2 <<- candidate_submission
  registration_questions2 <<- registration_questions

  registration_questions <- registration_questions%>%dplyr::mutate(question_id = input_id)%>%
    dplyr::select(-input_id)%>%
    dplyr::select( required, question_id)%>%
    dplyr::distinct(question_id, .keep_all = TRUE)


  candidate_submission <- candidate_submission%>%dplyr::left_join(registration_questions,by = c("question_id"))

  required_filled <- candidate_submission%>%dplyr::filter(response == "" & required == TRUE)
  if(nrow(required_filled)>0)return("missing required inputs")

  candidate_submission <- candidate_submission%>%dplyr::select(-required)

  temp_colnames <- c(paste0(candidate_submission$question_id),"creation_time")

  temp_colnames[grepl("course_date_",temp_colnames)] <- "course_date"

  candidate_submission <- candidate_submission%>%
    dplyr::as_tibble()%>%
    dplyr::select(-subject_id,-question_type)%>%
    dplyr::select(-question_id)%>%
    t()%>%dplyr::as_tibble()%>%
    dplyr::mutate(creation_time = paste0(Sys.time()))
  colnames(candidate_submission) <- temp_colnames

  candidate_submission <- candidate_submission%>%dplyr::mutate(user_name = Sys.getenv("SHINYPROXY_USERNAME"))
  sc_meta_data <- load_db_inputs(target_table = "candidates_infos")
  candidate_submission %>%
    upload_to_database(sc_meta_data = sc_meta_data)
  return("registration confirmed")
}




#' check completeness
#' @description check if all attributes are complete and valid
#' @author Farid Azouaou
#' @export

check_completeness <- function(candidate_submission = NULL, registration_questions = NULL){

  missing_responses <- candidate_submission%>%dplyr::filter(response == "")%>%
    dplyr::mutate(input_id = question_id)%>%
    dplyr::select(-question_id)%>%
    dplyr::left_join(registration_questions , by = "input_id")%>%
    dplyr::filter(required == TRUE)%>%
    dplyr::pull(question)%>%
    gsub("\\?","",.)
  return(missing_responses)
}


#' @export
mongodb_create_filter_query <- function(sc_meta_data = NULL, item_IDs = NULL, store_IDs = NULL){
  item_ID_field <- sc_meta_data$fields_output$item_ID_field
  store_ID_field <- sc_meta_data$fields_output$store_ID_field
  if(is.null(item_IDs) & is.null(store_IDs))return(NULL)
  filter_query <- list()
  if(!is.null(item_IDs)){
    if(length(item_IDs)==1){
      filter_query[[item_ID_field]] <- item_IDs
    }else{
      filter_query[[item_ID_field]] <- list('$in' = item_IDs)
    }
  }

  if(!is.null(store_IDs)){
    if(length(store_IDs)==1){
      filter_query[[store_ID_field]] <- store_IDs
    }else{
      filter_query[[store_ID_field]] <- list('$in' = store_IDs)
    }
  }
  return(filter_query)
}

#' import_data_mongo_db
#' @description import data from mongo DB databse stored on cloud based on some criteria
#' @param base_url a base url for mongoDB DATA API
#' @export
import_data_mongo_db <- function(base_url = NULL,mongo_db_api_key = NULL,target_db = "DB_Full_byline",
                                 target_table = NULL, target_filter = NULL,req_limit = 4000) {



  request_body = list(
    "dataSource"= "Cluster0",
    "database"= target_db,
    "collection" = target_table,
    "limit"= req_limit)
  #
  # if(!is.null(target_projection)){
  #   request_body$projection <- target_projection
  # }

  if(!is.null(target_filter)){
    request_body$filter <- target_filter
  }

  db_headers = httr::add_headers(
    'Access-Control-Request-Headers'= '*',
    'api-key'= mongo_db_api_key
  )


  url <- paste0(base_url ,"v1/action/find")

  api_results <- httr::POST(url = url, config = db_headers, body = request_body,encode  = "json")%>%
    httr::content(encoding = "json")


  api_results <-
    api_results$documents%>%
    do.call(dplyr::bind_rows,.)

  return(api_results)
}


#' upload_data_mongo_db
#' @description import data from mongo DB databse stored on cloud based on some criteria
#' @param base_url a base url for mongoDB DATA API
#' @export
upload_data_mongo_db <- function(base_url = NULL,mongo_db_api_key = NULL,target_db = "DB_Full_byline",
                                 target_table = NULL, input_data = NULL,req_limit = 5000,upload_mode = "update") {
  input_data <- input_data%>%dplyr::mutate_if(lubridate::is.Date,lubridate::as_datetime)%>%
    dplyr::mutate_if(lubridate::is.POSIXct,as.character)%>%
    purrr::transpose() # transform into document
  # aa <- input_data%>%t()%>%data.frame()%>%purrr::map(.,~.x)
  request_body = list(
    "dataSource" = "Cluster0",
    "database"   = target_db,
    "collection" = target_table,
    "documents"  = input_data)

  db_headers = httr::add_headers(
    'Access-Control-Request-Headers'= '*',
    'api-key'= mongo_db_api_key
  )
  url <- paste0(base_url ,"v1/action/insertMany")
  api_results <- httr::POST(url = url, config = db_headers, body = request_body,encode  = "json")
  return(api_results)
}

#' upload_to_database
#' @description t.b.d
#' @param input_data input_data
#' @export
upload_to_database <- function(input_data = NULL, upload_mode = "overwrite" , sc_meta_data=NULL){
  input_data <- input_data%>%janitor::clean_names()

  mongo_db_api_key         <- Sys.getenv(sc_meta_data$api_key_name)
  upload_data_mongo_db(base_url =sc_meta_data$base_url ,
                       mongo_db_api_key = mongo_db_api_key,
                       target_db =  sc_meta_data$target_db,
                       target_table = sc_meta_data$target_table,
                       input_data = input_data, upload_mode = upload_mode)
  return(sc_meta_data)
}

#' load_db_inputs
#' @description t.b.d
#' @param target_table data base table
#' @export
load_db_inputs <- function(target_table = "contact_form"){
  sc_meta_data <- list(base_url = "https://data.mongodb-api.com/app/data-qgigl/endpoint/data/", target_table =target_table, target_db = "fairviewer",
                       api_key_name = "MONGODB_API_KEY1")
  return(sc_meta_data)
}





