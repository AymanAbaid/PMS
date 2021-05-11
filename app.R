library(shiny)
library(shinydashboard)
library(shinyWidgets)

source('R/PMS.R', local = TRUE)
source('R/Tabs.R')
source('R/UI.R', local = TRUE)
source('R/Server.R')


shinyApp(
  ui = UI,
  server = server
)