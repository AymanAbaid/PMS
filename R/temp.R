# # library(googledrive)
# #  
# # # ## To create non authorization for communication with google drive
# # library(gargle)
# # options(gargle_oauth_cache = ".secrets_aa", gargle_oauth_email = TRUE)
# # 
# # upload_files_to_google_drive = function(filename, data){
# # 
# #   file_path_on_drive = paste0("PITC_Data/", filename)
# #   file_path_local = paste0("Drive_Download/", filename)
# #   # data = Historic_Forecast
# #   # filename = "Historic_Forecast.csv"
# #   ## Save the data locally
# #   write.csv(data, file_path_local, row.names = FALSE)
# # 
# #   ## Save the data in drive
# #   drive_upload(media = file_path_local, path="PITC_Data/", overwrite = TRUE)
# # 
# # }
# 
# # 
# # # # library(shiny)
# # # # library(data.table)
# # # # 
# # # # df <- rbind(
# # # #   data.table( cat = rep('X', 40), grp = rep(LETTERS[1:4], each=10), x = rep(1:10, times=4), y = rnorm(40) ),
# # # #   data.table( cat = rep('Y', 30), grp = rep(LETTERS[1:3], each=10), x = rep(1:10, times=3), y = rnorm(30) ),  
# # # #   data.table( cat = rep('Z', 30), grp = rep(LETTERS[4:6], each=10), x = rep(1:10, times=3), y = rnorm(30) )
# # # # )
# # # # server <- function(input, output) {
# # # #   
# # # #   rv <- reactiveValues(
# # # #     i  = NULL,
# # # #     df = NULL
# # # #   )
# # # #   
# # # #   observe({ rv$i <- input$i })
# # # #   
# # # #   observe({ rv$df <- df[cat == rv$i] })
# # # #   
# # # #   observe({
# # # #     for(letter in unique(rv$df$grp)){
# # # #       local({
# # # #         let <- letter
# # # #         output[[let]] <- renderPlot({
# # # #           if( let %in% c('A','D','E')) {
# # # #             with(rv$df[grp == let], plot(x, y, type='l'))
# # # #           } else {
# # # #             with(rv$df[grp == let], plot(x,y))
# # # #           }
# # # #         })
# # # #       })
# # # #     }
# # # #   })
# # # #   
# # # #   output$test <- renderUI({
# # # #     lapply(unique(rv$df$grp), plotOutput)
# # # #   })
# # # #   
# # # # }
# # # # 
# # # # ui <- fluidPage(
# # # #   titlePanel('Title'),
# # # #   sidebarLayout(
# # # #     sidebarPanel(
# # # #       helpText('Select the Category you would like to view.'),
# # # #       selectInput('i', 'Category', c('X','Y','Z'), selectize=TRUE)
# # # #     ),
# # # #     
# # # #     mainPanel(
# # # #       uiOutput('test')
# # # #     )
# # # #   )
# # # # )
# # # # 
# # # # shinyApp(ui, server)
# # # #-----------------------------------------
# # # categ<- c("domestic","commercial","small_industry","public_tube_well")
# # # 
# # # df<-read.csv("Data/FESCO-Planned Load Data-exist+planned.csv")
# # # df$
# # # df$peak_demand_kwh <- rep(0, nrow(df))
# # # for ( i in 1:length(categ)){
# # #   load_factor = df[,paste(categ[i],"_load_factor", sep="")] * 0.01
# # #   energy_used_kwh = df[,categ[i]]
# # #   peak_demand_kwh = energy_used_kwh/(30*24*12*load_factor)
# # #   df$peak_demand_kwh = peak_demand_kwh + df$peak_demand_kwh
# # # 
# # # }
# # # t<- df[,c("year","peak_demand_kwh")]
# # # library(plotly)
# # # 
# # # trace_0 <- rnorm(100, mean = 5)
# # # # trace_1 <- rnorm(100, mean = 0)
# # # # trace_2 <- rnorm(100, mean = -5)
# # # x <- c(1:100)
# # # 
# # # data <- data.frame(x, trace_0)
# # # 
# # # fig <- plot_ly(data, x = ~x)
# # # fig <- fig %>% add_trace(y = ~trace_0, name = 'trace 0',mode = 'lines')
# # # fig
# # # #----------------------------------------------------- 
# # # # df$peak_demand = df
# # # # df[ year = unique(df["year"]), ]
# # # years_ = as.list(as.data.frame(t(unique(df$year)))) 
# # # (unique(df$year))
# # # # colnames(df[,categ])
# # # # colnames(df)
# # # # 
# # # # # categories_var
# # # # # # for (i in (1:nrow(unique_yrs) )) 
# # # # #   for (i in 0:10 )
# # # # #   {
# # # # #    print(i)
# # # # #    year_i = df[ df$year == i,]
# # # # #    year_i = 
# # # # #    # year_i$total_demand = year_i$domestic + year_i$commercial  + year_i$public_light + year_i$public_tube_well +  year_i$public_tube_well
# # # # #   
# # # # #    }
# # # # #   
# # # input<- data.frame(sind_cf=56, ptub_cf=70,
# # #                                sub_area1_cf=85, sub_area2_cf=90,
# # #                                sub_area3_cf = 95, area_cf= 85,
# # #                                disco_cf_pd = 90, trans_losses_pd =1 ,
# # #                                dist_losses_pd = 15)
# # # 
# # # categories_var <-
# # #   c(
# # #     "domestic",
# # #     "commercial",
# # #     "public_light",
# # #     "small_industry",
# # #     "medium_large_industry",
# # #     "public_tube_well",
# # #     "bulk"
# # #   ) #### MAke sure It is be same as Schema
# # 
# # # library(plotly)
# # # 
# # # month <- c('January', 'February', 'March', 'April', 'May', 'June', 'July',
# # #            'August', 'September', 'October', 'November', 'December')
# # # high_2000 <- c(32.5, 37.6, 49.9, 53.0, 69.1, 75.4, 76.5, 76.6, 70.7, 60.6, 45.1, 29.3)
# # # data <- data.frame(month, high_2000)
# # # #The default order will be alphabetized unless specified as below:
# # # data$month <- factor(data$month, levels = data[["month"]])
# # # fig <- plot_ly(data, x = ~month, y = ~high_2000, name = 'High 2014', type = 'scatter', mode = 'lines',
# # #                lin???e = list(color = 'rgb(205, 12, 24)', width = 4)) 
# # # fig <- fig %>% layout(title = "Average High and Low Temperatures in New York",
# # #                       xaxis = list(title = "Year"),
# # #                       yaxis = list (title = "Peak Demand (MWh)"))
# # # 
# # # fig
# # 
# # # growth_rate_df<- read.csv( paste(Config_dir, "growth_rate_df.csv", sep = "") )
# # 
# # # colnames(growth_rate_df[,-1]) <- categories_var
# # # write.csv(growth_rate_df,paste(Config_dir, "growth_rate_df_autofill.csv", sep = ""))
# # library(plotly)
# # 
# # # read in Walmart data
# # df <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/1962_2006_walmart_store_openings.csv")
# # 
# # df <- peak_demand_df
# # colnames(peak_demand_df)
# # peak_demand_df$energy_purchase = peak_demand_df$energy_sent_out + peak_demand_df$total_loss
# # 
# # 
# # fig2 <- plot_ly(data = peak_demand_df, x = ~year, y = ~energy_purchase, type = "bar", showlegend=FALSE,
# #                 marker=list(color=~year, showscale=FALSE), ylab= "kjlkjlj") 
# # # fig2 <- fig2 %>% add_lines(y = peak_demand_df$energy_sent_out, showlegend=TRUE, color = 'Green', name ="Energy Sent out" , width =10)
# # 
# # # fig2 <- fig2 %>% add_trace(y = ~peak_demand_df$energy_sent_out, name = 'Low 2000', line = list(color = 'rgb(22, 96, 167)', width = 4, dash = 'dot'))
# # fig2 <- fig2 %>% add_trace(  y = ~peak_demand_df$energy_sent_out, type = 'scatter',  
# #                              mode = 'lines+markers', width = 10,showlegend=TRUE,  name ="Energy Sent out",
# #                              hoverinfo = 'text',
# #                              text = ~paste('</br> Energy Sent Out: ', peak_demand_df$energy_sent_out,
# #                                            '</br> Year: ', peak_demand_df$year))
# # 
# # fig2 <- fig2 %>% layout(showlegend=TRUE, xaxis = list(side="right", showgrid=FALSE),
# #                         yaxis=list(showgrid=FALSE))
# 
# 
# # all_gr<-readxl::excel_sheets("C:\\Users\\Ayman\\Documents\\PMS-Data\\PMS 2019-20\\DISCOs_Growth Rate.xlsx")
# # 
# # library(readxl)
# # DISCOs_Growth_Rate <- read_excel("C:\\Users\\Ayman\\Documents\\PMS-Data\\PMS 2019-20\\DISCOs_Growth Rate.xlsx")
# # View(DISCOs_Growth_Rate)
# # 
# # library(shiny)
# # library(DT)
# # library(shinycssloaders)
# # shinyApp(
# #   ui = fluidPage(fluidRow(column(12,withSpinner(DT::DTOutput('tbl'))))),
# #   server = function(input, output) {
# #     Sys.sleep(2)
# #     output$tbl = renderDT(
# #       iris, options = list(lengthChange = FALSE)
# #     )
# #   }
# # )
# # 
# # if (interactive()) {
# #   
# #   library(shiny)
# #   library(shinybusy)
# #   
# #   ui <- fluidPage(
# #     
# #     tags$h1("Modal with spinner"),
# #     actionButton("sleep1", "Launch a long calculation"),
# #     actionButton("sleep2", "And another one")
# #   )
# #   
# #   server <- function(input, output, session) {
# #     
# #     observeEvent(input$sleep1, {
# #       show_modal_spinner()
# #       Sys.sleep(5)
# #       remove_modal_spinner()
# #     })
# #     
# #     observeEvent(input$sleep2, {
# #       show_modal_spinner(
# #         spin = "cube-grid",
# #         color = "firebrick",
# #         text = "Please wait..."
# #       )
# #       Sys.sleep(5)
# #       remove_modal_spinner()
# #     })
# #     
# #   }
# #   
# #   shinyApp(ui, server)
# #   
# # }
# 
# 
# require(R.utils)
# library(R.utils)
# ## function that can take a long time
# fn1 <- function(x)
# {
#   for (i in 1:x^x)
#   {
#     rep(x, 1000)
#   }
#   return("finished")
# }
# 
# ## test timeout
# R.utils::withTimeout(fn1(3), timeout = 1, onTimeout = "error") # should be fine
# 
# 
# Sh
# y <- try({setTimeLimit(cpu, elapsed); fn1(18)}, silent = TRUE) 
