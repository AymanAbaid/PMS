# library(shiny)
# library(data.table)
# 
# df <- rbind(
#   data.table( cat = rep('X', 40), grp = rep(LETTERS[1:4], each=10), x = rep(1:10, times=4), y = rnorm(40) ),
#   data.table( cat = rep('Y', 30), grp = rep(LETTERS[1:3], each=10), x = rep(1:10, times=3), y = rnorm(30) ),  
#   data.table( cat = rep('Z', 30), grp = rep(LETTERS[4:6], each=10), x = rep(1:10, times=3), y = rnorm(30) )
# )
# server <- function(input, output) {
#   
#   rv <- reactiveValues(
#     i  = NULL,
#     df = NULL
#   )
#   
#   observe({ rv$i <- input$i })
#   
#   observe({ rv$df <- df[cat == rv$i] })
#   
#   observe({
#     for(letter in unique(rv$df$grp)){
#       local({
#         let <- letter
#         output[[let]] <- renderPlot({
#           if( let %in% c('A','D','E')) {
#             with(rv$df[grp == let], plot(x, y, type='l'))
#           } else {
#             with(rv$df[grp == let], plot(x,y))
#           }
#         })
#       })
#     }
#   })
#   
#   output$test <- renderUI({
#     lapply(unique(rv$df$grp), plotOutput)
#   })
#   
# }
# 
# ui <- fluidPage(
#   titlePanel('Title'),
#   sidebarLayout(
#     sidebarPanel(
#       helpText('Select the Category you would like to view.'),
#       selectInput('i', 'Category', c('X','Y','Z'), selectize=TRUE)
#     ),
#     
#     mainPanel(
#       uiOutput('test')
#     )
#   )
# )
# 
# shinyApp(ui, server)