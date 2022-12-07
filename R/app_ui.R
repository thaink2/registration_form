library("dplyr")
sc_meta_data <- load_db_inputs(target_table = "contact_form")
registration_questions <- import_data_mongo_db(base_url = sc_meta_data$base_url,
                                               mongo_db_api_key =  Sys.getenv(sc_meta_data$api_key_name),
                                               target_db = sc_meta_data$target_db, target_table = sc_meta_data$target_table)%>%
  dplyr::select(-'_id')

#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    shinyalert::useShinyalert(),

    # List the first level UI elements here
    fluidPage(
      shinysurveys::surveyOutput(df = registration_questions,
                                 survey_title = "Fair Analytics",
                                 survey_description = "Complete and Activate your registration",
                                 theme = "#458B74"
                                 )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){

  add_resource_path(
    'www', app_sys('app/www')
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'trainingmanager'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}


# add email type

# shinysurveys::extendInputType(input_type = "email", {
#   polished::email_input(
#     inputId = surveyID(),
#     label = tagList(icon("envelope"), "Email"),
#     value = "",
#     width = NULL,
#     placeholder = NULL
#   )
# })
