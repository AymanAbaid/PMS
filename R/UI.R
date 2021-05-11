
library(shinydashboard)

UI <- dashboardPage(
  dashboardHeader(title = "Power Market Survey"),
  dashboardSidebar(
  sidebarMenu(
    menuItem("Upload Data", tabName = "upload_data", icon = icon("dashboard")),
    menuItem("View Data", tabName = "view_data", icon = icon("th"))
  )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "upload_data",
              # h2("upload_data tab content")
              Tab1
      ),
      
      tabItem(tabName = "view_data",
              Tab2      )
    )
  )
  
)