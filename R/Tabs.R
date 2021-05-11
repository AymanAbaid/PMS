library(shinyBS)

Tab1 <- tabPanel("Upload Data",
                 selectInput("file_type", "Choose Type of File to upload",
                             c("Planned Load Data",# = "PLD",
                               "Proposed Grid Data",# = "PGD",
                               "List of Transformers on Grid (MVA)" ,#= "LOTG",
                               "Grid Load Data")),
                 
                 fileInput(
                   inputId = "input_file", 
                   label = "Choose File", 
                   multiple = TRUE,
                   accept = c("text/csv",
                              "text/comma-separated-values,text/plain",".xlsx",
                              ".csv")
                 ),
                 actionButton("save_file", "Save File"),
                 
                 bsAlert("file_error")
                 
)

Tab2 <- tabPanel("View Data",
                 selectInput("file_type_view", "Choose Type of File to view",
                             c("","Planned Load Data",# = "PLD",
                               "Proposed Grid Data",# = "PGD",
                               "List of Transformers on Grid (MVA)" ,#= "LOTG",
                               "Grid Load Data")),
                 # bsAlert("file_view_error"),
                 
                 dataTableOutput('input_data_table')
                 
                 )