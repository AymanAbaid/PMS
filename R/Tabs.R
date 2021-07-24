# library(shinyBS)
source('R/helper_func_var.R')

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
                 
                 shinyBS::bsAlert("file_error")
                 
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
Tab3 <- tabPanel("Data Browser",
                  
                 selectInput("select_disco_db", "Choose DSICO",
                             Disco_List),
                 selectInput("select_figure_db", "Choose Figure",
                             c("","Cateogry Wise Sales Forecast")),
                 uiOutput('browse_data_no_of_years_ui'),
                 plotlyOutput("db_fig")
                 
                 # bsAlert("file_view_error"),
                 
                 # /dataTableOutput('input_data_table')
                 
 )
Tab4 <- tabPanel("Set Configuartion",
                 selectInput("choose_config_category", "Choose Category",
                             c("General","Category Wise Sale")),
                 
                 uiOutput('base_config_ui'),
                 uiOutput('cws_config_ui')
                 # conditionalPanel(
                 #   condition = "input.choose_config_category == ''",
                 #   textInput("base", label = "Growth Rate in %", value = cws_growth_rate),
                 #   textInput("cws_growth_rate", label = "Growth Rate in %", value = cws_growth_rate),
                 #   
                 # ),
                 # conditionalPanel(
                 #   condition = "input.choose_config_category == 'Category Wise Sale'",
                 #   textInput("cws_growth_rate", label = "Growth Rate in %", value = cws_growth_rate),
                 #   textInput("cws_growth_rate", label = "Growth Rate in %", value = cws_growth_rate),
                 #   
                 )
Tab5 <- tabPanel("Generate Report",
                 actionButton("download_report_btn", "Download Report"),
                 
                 

                 
)
                