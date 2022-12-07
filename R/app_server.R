#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {


  shinysurveys::renderSurvey()

  observeEvent(input$submit, {
    library("dplyr")
    registration_status  <- finalize_submission(registration_questions = registration_questions ,candidate_submission =  shinysurveys::getSurveyData())

    if(registration_status == "registration confirmed"){
      shinyalert::shinyalert(title = paste("Registration Confirmed, Thank you :)"), type = "success")
    }else if(registration_status == "missing required inputs"){
      shinyalert::shinyalert(title = "Some mandatory fields are missing !!", type = "error")
    }else{
      shinyalert::shinyalert(title = "Thanks, You account will be activated within few minutes", type = "success")
    }
    })
}
