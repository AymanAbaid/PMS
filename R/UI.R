source('R/helper_func_var.R')
library(shinydashboard)
UI <- dashboardPage(
  dashboardHeader(title = "Power Market Survey"),
  dashboardSidebar(
    sidebarMenu(
      id = "menu",
      menuItem("View/Upload Data", tabName = "view_upload_data", icon = icon("fas fa-database")),
      menuItem("Browse Data", tabName = "view_graph", icon = icon("fas fa-poll")),
      menuItem(
        "Set Configuration",
        tabName = "set_configuration",
        icon = icon("fas fa-cogs")
      ),
      menuItem("Generate Report", tabName = "generate_report", icon = icon("fas fa-poll-h")),
      menuItem("Resources", tabName = "resources", icon = icon("fas fa-info"))
      
      
    )
  ),
  dashboardBody(
    shinyjs::useShinyjs(),
    shinyalert::useShinyalert(),  # Set up shinyalert
    
    tabItems(
      tabItem(tabName = "view_upload_data",
              tabsetPanel(type = "tabs",
                          tabPanel("Upload", Tab1),
                          tabPanel("View", Tab2))
      ),
      tabItem(tabName = "view_graph",
              Tab3),
      tabItem(tabName = "set_configuration", width =12,
             Tab4
              ),
      # tabItem(tabName = "set_configuration", width =12,
      #         selectInput("choose_config_category", "Choose Category",
      #                     c("General","Growth Rates","Peak Demand"))
      #         ,HTML("<br/>")
      #         ,tabsetPanel(type = "tabs",
      #                      tabPanel("Growth Rate", growth_rate_config_tab), 
      #                      tabPanel("CF Loss", CF_losses_tab)
      #         )
      # ),
      tabItem(tabName = "generate_report",
              Tab5),
      
      tabItem(tabName = "resources",
              Tab6)
      
      
      
    ))
  
)