

input_data <- readr::read_csv("./inst/training_questions - training_questions.csv")
input_data <- tail(input_data,-2) # name already known
library("dplyr")
sc_meta_data <- load_db_inputs(target_table = "contact_form")

input_data %>%
  upload_to_database(sc_meta_data = sc_meta_data)

