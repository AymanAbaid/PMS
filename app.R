# Load the required packages
list_of_packages <- c("shiny", "shinydashboard","shinyWidgets","lubridate","easycsv","rmarkdown", "dplyr","plotly","shinyBS")
lapply(list_of_packages, library, character.only = TRUE)



source('R/helper_func_var.R')
# source('R/Configuration.R')
source('R/Tabs.R')
source('R/UI.R')#, local = TRUE)



server <- function(input, output, session) {

# Initializing Reactive Values --------------------------------------------
  RV <- reactiveValues(
    file_exist_for_view = FALSE,
    df_view = NULL,
    db_fig = NULL,
    db_df = NULL,
    db_year = NULL
  )
  
  config_env <- reactiveValues(
    base_year  = NULL ,
    no_of_years = NULL,
    domestic_gr  = NULL ,
    commercial_gr  = NULL ,
    public_light_gr = NULL ,
    small_industry_gr  = NULL ,
    medium_large_industry_gr  = NULL ,
    public_tube_well_gr = NULL ,
    bulk_gr  = NULL
    
  )


# Load Configuration Data -------------------------------------------------

    f.load_data <- function()
    {
      myenv <- new.env()
      load(file = paste(Config_dir, "config.RData", sep = ""),
           envir = myenv)
      myenv
    }
    some <- f.load_data()
    s <- as.list(some)
    l <- isolate(reactiveValuesToList(config_env))
    l_names <- names(l)
    for (i in 1:length(l_names))
    {
      obj = l_names[i]
      config_env[[obj]] <- s[obj]
      
    }
  


# Upload Data -------------------------------------------------------------

  observeEvent(input$file_type_view, {
    file_name = paste(input$file_type_view, ".csv", sep = "")
    file_path = paste(Data_dir, file_name, sep = "")
    
    if (file.exists(file_path))
    {
      RV$file_exist_for_view = TRUE
      RV$df_view <- read.csv(file_path)
      
      
    }
    else{
       if (input$file_type_view != "") {
        show_alert(
          title = "File Not Found !!",
          text = paste(input$file_type_view, " File Does not Exist", sep =
                         ""),
          type = "error"
        )
        
        RV$df_view <- NULL
      }
    }
    
  })
  
  observeEvent(input$save_file, {
    ext <- tools::file_ext(input$input_file$datapath)
    if (ext != "csv") {
      ## Make sure input file is a CSV file
      
      createAlert(
        session,
        "file_error",
        "File Extension Error",
        style = "warning",
        title = "Oops",
        content = "Input File should be a CSV file.",
        append = FALSE
      )
    }
    else{
      df <- read.csv(input$input_file$datapath)
      
      file_ = input$file_type
      
      if (file_ == "Planned Load Data")
      {
        input_file_error = column_validation (df,
                                              Planned_Load_Data_cols,
                                              Planned_Load_Data_char_cols,
                                              file_)
      }
      
      else if (file_ == "Proposed Grid Data")
      {
        input_file_error = column_validation (df,
                                              Proposed_Grid_Data_cols,
                                              Proposed_Grid_Data_char_cols,
                                              file_)
      }
      
      else if (file_ == "List of Transformers on Grid (MVA)")
      {
        input_file_error = column_validation (
          df,
          List_of_Transformers_on_Grid_Data,
          List_of_Transformers_on_Grid_char_Data,
          file_
        )
      }
      
      else if (file_ == "Grid Load Data")
      {
        input_file_error = column_validation (df,
                                              Grid_Load_Data_cols,
                                              Grid_Load_Data_char_cols,
                                              file_)
      }
      
      
      print(input_file_error)
      if (!is.null(input_file_error))
      {
        createAlert(
          session,
          "file_error",
          "File Format Error",
          style = "danger",
          title = "File Format Error",
          content = input_file_error,
          append = FALSE
        )
        
      }
      else{
        ### Save file locally
        file_name = paste(input$file_type, ".csv", sep = "")
        file_path = paste(Data_dir, file_name, sep = "")
        write.csv(df , file_path, row.names = FALSE)
        createAlert(
          session,
          "file_error",
          "File uploaded sucessfully",
          style = "success",
          title = "Success",
          content = "Input File uploaded sucessfully",
          append = FALSE
        )
      }
      
    }
  })
  
  
# View Data ---------------------------------------------------------------
  output$input_data_table <- renderDataTable(RV$df_view,
                                             options = list(
                                               pageLength = 10,
                                               width = "100%",
                                               scrollX = TRUE
                                             ))
  
  ################################################Reset data when tab changes
  # observeEvent(input$menu, {
  #   if ((input$menu) == "view_graph")
  #   {
  #     print("on tba load   ")
  #     ## Reset "Select Figure" input value
  #     updateTextInput(session, "select_figure_db", value = "")
  #     
  #   }
  #   
  # })

  ################################################# Browse Table and Figures ###########################################
  
  observeEvent(input$select_figure_db , {
    if (input$select_figure_db == "Cateogry Wise Sales Forecast")
    {
      df = read.csv(paste(Data_dir, "Grid Load Data.csv", sep = ""))
      df <-
        df[df$year == 0,][categories_var] # get value for base year only
      df <- data.frame(t(colSums(df)))
      gorwth_rate_for_each_category = data.frame(matrix(ncol = length(categories_var), nrow =
                                                          1))
      colnames(gorwth_rate_for_each_category)  <- categories_var
      
      
      for (i in (1:length(categories_var)))
      {
        env_var = paste(categories_var[i], "_gr", sep = "")
        gorwth_rate_for_each_category[[categories_var[i]]] =  isolate(config_env[[env_var]])
        
      }
      start_year <-
        as.numeric(substr(config_env$base_year, 1, 4))
      df <-
        forecast_using_cagr (
          df,
          gorwth_rate_for_each_category,
          as.numeric(config_env$no_of_years),
          start_year
        )
      RV$db_df <- df
      
    }
  })
  
  observeEvent(input$browse_data_no_of_years , {
    RV$db_year <- input$browse_data_no_of_years
    print(paste("new Year ", RV$db_year, " df size", nrow(df)))
  })
  
  
  
  output$db_fig <- renderPlotly({
    if (input$browse_data_no_of_years == "" ||
        input$select_figure_db == "")
      return(NULL)
    
    df <- RV$db_df
    lbl = Categories
    curr_yr = RV$db_year
    
    vals = (df[df$year == curr_yr, categories_var])#/(10^9) # to convert itno MW
    vals <- unname(unlist(list(vals)))
    title = paste("Cateogry Wise Sales Forecast", curr_yr)
    RV$db_fig <- display_pie_chart(lbl, vals, title)
    
    RV$db_fig
  })
  
  
  ################################################# Configuration UI ###########################################
  
  
  output$base_config_ui <- renderUI({
    if (input$choose_config_category == 'General') {
      o = tagList()
      o[[1]] = textInput(inputId = "base_year",
                         label = "Enter Base Year (e.g: 2019-2020)",
                         value = config_env$base_year)
      o[[2]] = textInput(inputId = "no_of_years", "Enter Number of Years", config_env$no_of_years)
      o[[3]] = actionButton("general_config_btn", "Save", class = "btn-success")
      
      o
    }
  })
  
  output$browse_data_no_of_years_ui <- renderUI({
    start_year <- as.numeric(substr(config_env$base_year, 1, 4))
    ny <- as.numeric(config_env$no_of_years)
    year_list <-
      paste(seq(start_year, (start_year + ny)), seq(start_year + 1, (start_year + ny + 1)), sep = "-")
    
    selectInput("browse_data_no_of_years", "Select Year", year_list)
  })
  
  
  output$cws_config_ui <- renderUI({
    if (input$choose_config_category == 'Category Wise Sale') {
      out = tagList()
      i = 1
      out[[i]] = checkboxInput(inputId = "use_cagr_formula_cws", label = "Calculate Growth Rate from Historical Data", FALSE)
      inc(i)
      for (k in 1:length(categories_var)) {
        input_id = paste(categories_var[k], "_gr", sep = "")
        label = paste("Growth Rate for ",
                      categories_var[k],
                      " in percentage",
                      sep = "")
        val = config_env[[input_id]]
        out[[i]] = textInput(inputId = input_id,
                             label = label,
                             value = val)
        inc(i)
      }
      out[[inc(i)]] = actionButton("cws_config_btn", "Save", class = "btn-success")
      out
    }
    
  })
  
  observeEvent(input$use_cagr_formula_cws, {
    if (input$use_cagr_formula_cws == TRUE) {
      categories_gr = calculate_cagr_for_all_categories(NULL)## get calculated Growth Rate using Historical data
      
      for (k in 1:length(categories_var)) {
        input_id = paste(categories_var[k], "_gr", sep = "")
        
        val = categories_gr[Categories[k]][1]
        updateTextInput(session , input_id, value =  paste("", categories_gr[Categories[k]][1]))
        print(paste(input_id, "at k = ", k, "", val))
        
        # label = paste("Growth Rate for ", Categories[k], " in percentage", sep="" )
        # val = config_env$categories_growth_rate[Categories[i]][1]
        # print( paste(input_id , label, val))
        # out[[i]]= textInput(inputId = input_id, label = label, value = val)
        # inc(i)
        
      }
    }
    
  })
  
  #### General Configuration Save Button
  observeEvent(input$general_config_btn, {

    base_year_new = input$base_year
    no_of_years_new = input$no_of_years
    
    if (!(
      base_year_new == config_env$base_year &&
      no_of_years_new == config_env$no_of_years
    ))
      ##  if true we need to save new data
    {
      config_env$base_year = base_year_new
      config_env$no_of_years =  no_of_years_new
      # Write whole Config_env RV to file
      {
        new_env <- new.env()
        for (obj in names(isolate(reactiveValuesToList(config_env)))) {
          assign(obj, unlist(config_env[[obj]], use.names = F), envir = new_env)
          save(
            list = ls(new_env),
            file = paste(Config_dir, "config.RData", sep = ""),
            envir = new_env
          )
          
        }
      }
      
    }
  })
  
  #### "Category wise sale" Configuration Save Button
  observeEvent(input$cws_config_btn, {
    for (obj in categories_var)
    {
      id = paste(obj, "_gr", sep = "")
      
      config_env[[id]] = as.numeric(input[[id]])
    }
    # Write whole Config_env RV to .RData file
    {
      new_env <- new.env()
      for (obj in names(isolate(reactiveValuesToList(config_env)))) {
        # assign(obj, unlist(config_env[[obj]],use.names=F))
        # save(list = ls(), file = paste(Config_dir, "config.RData", sep = ""))
        
        assign(obj, unlist(config_env[[obj]], use.names = F), envir = new_env)
        save(
          list = ls(new_env),
          file = paste(Config_dir, "config.RData", sep = ""),
          envir = new_env
        )
        
      }
    }
    
    
    
    
    
  })
  
  #####################################################Report Generation
  
  observeEvent(input$download_report_btn, {
    # get directory path
    report_dir <- easycsv::choose_dir()
    
    
    if (!is.null(report_dir) &&
        !is.na(report_dir))
    {
      start_year <- as.numeric(substr(config_env$base_year, 1, 4))
      end_year <- start_year + as.numeric(config_env$no_of_years)
      filename <-
        paste0("Power Market Survey ", start_year, "-", end_year , ".html")
      # list_rv = reactiveValuesToList(config_env)
      
      ## Render PMS Doc from RMD File
      rmarkdown::render(
        "PMS-doc.Rmd",
        "html_document",
        output_file = paste0(report_dir, "\\", filename, sep = "")
      )
    }
    
    
  })
  
  
}

shinyApp(ui = UI,
         server = server)
