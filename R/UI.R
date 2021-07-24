source('R/helper_func_var.R')

library(shinydashboard)

UI <- dashboardPage(
  dashboardHeader(title = "Power Market Survey"),
  dashboardSidebar(
    sidebarMenu(
      id = "menu",
      menuItem(
        "Upload Data",
        tabName = "upload_data",
        icon = icon("dashboard")
      ),
      menuItem("View Data", tabName = "view_data", icon = icon("angle-double-right")),
      menuItem("Browse Data", tabName = "view_graph", icon = icon("th")),
      menuItem(
        "Set Configuration",
        tabName = "set_configuration",
        icon = icon("th")
      ),
      menuItem("Generate Report", tabName = "generate_report", icon = icon("th"))
      
      
    )
  ),
  dashboardBody(tabItems(
    tabItem(tabName = "upload_data",
            # h2("upload_data tab content")
            Tab1),
    
    tabItem(tabName = "view_data",
            Tab2),
    tabItem(tabName = "view_graph",
            Tab3),
    tabItem(tabName = "set_configuration",
            Tab4),
    tabItem(tabName = "generate_report",
            Tab5)
    
    
  ))
  
)