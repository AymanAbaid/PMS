
source('R/helper_func_var.R')
library("DT")

Tab1 <- tabPanel("Upload Data",
                 div(style = "position:absolute;right:1em;", 
                     actionButton('restore_data_query_btn', 'Restore Data'),
                 ),
                 
                 selectInput("select_disco_ud", "Choose DSICO",
                             Disco_List),
                 selectInput("file_type", "Choose Type of File to upload",
                             c("Planned Load Data",# = "PLD",
                               "Proposed Grid Data",# = "PGD",
                               "List of Transformers on Grid (MVA)" ,#= "LOTG",
                               "Grid Load Data")),
                 selectInput("year_uploadData", "Select Year",
                             c( seq(1947,(base_year))),base_year),
                 fileInput(
                   inputId = "input_file", 
                   label = "Choose File", 
                   multiple = TRUE,
                   accept = c("text/csv",
                              "text/comma-separated-values,text/plain",".xlsx",
                              ".csv")
                 ),
                 downloadButton("download_sample_file", "Download Sample File"),
                 actionButton("save_file", "Save File",class="btn-primary btn-success"),
                 
                 shinyBS::bsAlert("file_error")
                 
)

Tab2 <- tabPanel("View Data",
                 selectInput("select_disco_viewData", "Choose DSICO",
                             Disco_List),
                 selectInput("file_type_viewData", "Choose Type of File to view",
                             c("","Planned Load Data",# = "PLD",
                               "Proposed Grid Data",# = "PGD",
                               "List of Transformers on Grid (MVA)" ,#= "LOTG",
                               "Grid Load Data")),
                 selectInput("year_viewData", "Select Year",
                             c( seq(1947,(base_year))),base_year ),
                 # bsAlert("file_view_error"),
                 
                 dataTableOutput('input_data_table')
                 
)
Tab3 <- tabPanel("Data Browser",
                 
                 selectInput("select_disco_db", "Choose DSICO",
                             Disco_List),
                 selectInput("select_figure_db", "Choose Figure",
                             c("","Cateogry Wise Sales Forecast","Computed Peak Demand","Energy Purchase vs. Energy Sale")),
                 uiOutput('browse_data_no_of_years_ui'),
                 plotlyOutput("db_fig")
                 
                 # bsAlert("file_view_error"),
                 
                 # /dataTableOutput('input_data_table')
                 
)
Tab4 <- tabPanel("Set Configuartion",
                 selectInput("choose_config_category", "Choose Category",
                             c("General","Growth Rates","Coincidence Factor and Losses")),
                 
                 # uiOutput('base_config_ui'),
                 # uiOutput('cws_config_ui'),
                 # DTOutput("gr_config_ui"),
                 # uiOutput('pd_config_ui'),
                 checkboxInput(inputId = "auto_fill_config_fields", label = "Auto Fill", FALSE),
                 conditionalPanel(
                   condition = "input.choose_config_category == 'General'",
                   numericInput(inputId = "base_year",label = "Enter Base Year (e.g: 2019)",value =0 ),
                   numericInput ("no_of_years", "Enter Number of Years",0)
                   
                 ),
                 conditionalPanel(
                   condition = "input.choose_config_category == 'Coincidence Factor and Losses'",
                   numericInput(inputId = "sind_cf" ,label = "Small Industry Coincidence Factor in %", value = 0),
                   numericInput(inputId = "ptub_cf" ,label = "Public Tube Well Coincidence Factor in %", value = 0),
                   numericInput(inputId = "sub_area1_cf" ,label = "Sub Area Type 1 Coincidence Factor in %", value = 0),
                   numericInput(inputId = "sub_area2_cf" ,label = "Sub Area Type 2  Coincidence Factor in %", value = 0),
                   numericInput(inputId = "sub_area3_cf" ,label = "Sub Area Type 3  Coincidence Factor in %", value = 0),
                   numericInput(inputId = "area_cf" ,label = "Area Coincidence Factor in %", value = 0),
                   numericInput(inputId = "disco_cf_pd" ,label = "DISCO Coincidence Factor in %", value = 0),
                   numericInput(inputId = "trans_losses_pd" ,label = "Transmission Losses in %", value = 0),
                   numericInput(inputId = "dist_losses_pd" ,label = "Distribution Losses in %", value = 0)
                   
                 ),
                 conditionalPanel(
                   condition = "input.choose_config_category == 'Growth Rates'",
                   dataTableOutput('gr_config_ui')
                   
                 ),
                 actionButton("config_save_btn", "Save", class = "btn-success")
)
Tab5 <- tabPanel("Generate Report",
                 downloadButton('download_report_btn', 'Generate report')
                 
                )

Tab6 <- tabPanel("Resources",
                 sidebarPanel(
                   selectInput(
                     inputId = "resources",
                     label = "List of Available Files:",
                     choices = c(list.files("Resources")),
                     size = length(list.files("Resources")),selectize = FALSE
                     # ,selected = "ALL"
                   ), width = 18
                 )
                 ,
                 downloadButton('download_resource',"Download File",class="butt" )
                 
)


