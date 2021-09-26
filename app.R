source('R/packages.R')
source('R/helper_func_var.R')
source('R/Tabs.R')
source('R/UI.R')#, local = TRUE)

server <- function(input, output, session) {

  ################################################# Download Data From Drive--------------------------------------------
  show_modal_spinner(spin = "cube-grid",
                     color = "firebrick",
                     text = "Please wait...")
  ## Global vars
  Upload_file_exist_on_drive = FALSE
  Upload_file_exist_locally = FALSE
  
  ## Download Configuration data
  # download_files_from_google_drive(".", Config_dir,Config_dir,NULL)
  ##Download Load data
  # download_files_from_google_drive(".", Data_dir,Data_dir,NULL)

  remove_modal_spinner()

  ################################################# Initializing Reactive Values --------------------------------------------
  RV <- reactiveValues(
    file_exist_for_view = FALSE,
    df_view = NULL,
    db_fig = NULL,
    db_df = NULL,
    db_year = NULL,
    save_file_df = NULL
  )
  config_env <- reactiveValues(
    base_year  = NULL ,
    no_of_years = NULL,
    sind_cf = NULL,
    # Small Industry Coincidence Factor in %
    ptub_cf = NULL,
    # Public Tube Well Coincidence Factor in %
    sub_area1_cf = NULL,
    # Sub Area Type 1 Coincidence Factor in %
    sub_area2_cf = NULL,
    # Sub Area Type 2  Coincidence Factor in %
    sub_area3_cf = NULL,
    # Sub Area Type 3  Coincidence Factor in %
    area_cf = NULL,
    # Area  Coincidence Factor in %
    trans_losses_pd = NULL,
    # Transmission Losses
    dist_losses_pd = NULL,
    # Distribution Losses
    disco_cf_pd = NULL # DISCO Coincidence Factor in %
    
    
  )
  config_DFs <- reactiveValues(growth_rate_df = NULL)
  
  ################################################# Load Configuration Data -------------------------------------------------
  
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
  growth_rate_df <-
    read.csv(paste(Config_dir, "growth_rate_df.csv", sep = ""))
  row.names(growth_rate_df) <- Disco_List
  # growth_rate_df<- growth_rate_df[,-1]
  config_DFs$growth_rate_df <- growth_rate_df[, -1]
  # row.names(reactive(config_DFs$growth_rate_df)) <- Disco_List
  
  ################################################# Upload Data -------------------------------------------------------------
  
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
      RV$save_file_df <- df
      
      file_ = input$file_type
      #check File Type
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
        file_name = paste(
          input$select_disco_ud,
          "-",
          input$year_uploadData,
          "-",
          input$file_type,
          ".csv",
          sep = ""
        )
        
        # Check if file already exists in local dir or on drive
        File_exist = FALSE
        show_modal_spinner(text = "Checking if File already exists....")
        ## time out functionality
        File_exist = Upload_file_exist_locally = file.exists(paste(Data_dir, file_name, sep =""))
        if (!Upload_file_exist_locally) 
        {
          Upload_file_exist_on_drive = File_exist = download_files_from_google_drive(file_name, Data_dir,Data_dir,file_name)
        }
        remove_modal_spinner()
        
        if (File_exist )
        {
          ask_confirmation(
            "upload_file_exist",
            title = "File Already Exists!",
            text = paste(
              "File ",
              file_name,
              " Already Exists. Do you want to replace it?",
              sep = ""
            ),
            type = "confirm"
          )
          
          
        }
        else{### Saving
          tryCatch({
            show_modal_spinner(text = "Uploading....")
            
            ### Save file locally
            file_path = paste(Data_dir, file_name, sep = "")
            write.csv(df , file_path, row.names = FALSE)
            
            ### Upload file on Google Drive
            upload_files_to_google_drive(file_name, Data_dir)
            
            remove_modal_spinner()
            
            # #saved successfully popup
            {
              show_alert(title = "Success !!",
                         text = "File saved successfully",
                         type = "success")
            }
          },
          error = function(cond)
          {
            # error popup
            {
              show_alert(title = "Error !!",
                         text = "Unable to save. Error occured",
                         type = "error")
              message(cond)
            }
            
          },
          warning = function(cond)
          {
            # error popup
            {
              show_alert(title = "Error !!",
                         text = "Unable to save. Error occured",
                         type = "error")
              message(cond)
            }
            
          }
          # ,finally = {
          #   # reset rv value
          #   RV$save_file = FALSE
          # }
          )}
        
      }
      
    }
  })
  
  observeEvent(input$upload_file_exist, {
    file_name = paste(
      input$select_disco_ud,
      "-",
      input$year_uploadData,
      "-",
      input$file_type,
      ".csv",
      sep = ""
    )
    
    if (isTRUE(input$upload_file_exist)) {
      # Replace File
      {
        ### Saving
        tryCatch({
          show_modal_spinner(text = "Uploading....")
          file_path = paste(Data_dir, file_name, sep = "")
          

          ## move already available file
          new_name =  paste(file_name, Sys.time(), sep = " ")
          if(Upload_file_exist_on_drive)
          {
            drive_mv(file_path, path = Data_backup_dir ,name = new_name,overwrite = TRUE)
          }
          else if (Upload_file_exist_locally)
          { # if file does not exist on drive, upload file to backup folder
            drive_upload(media = file_path, path= Data_backup_dir ,name =new_name, overwrite = TRUE)
            
          }
            
          ### Save New file locally
          write.csv(RV$save_file_df , file_path, row.names = FALSE)
          
          
          ### Upload New file on Google Drive
          upload_files_to_google_drive(file_name , Data_dir)
          
          remove_modal_spinner()
          
          # #saved successfully popup
          {
            show_alert(title = "Success !!",
                       text = "File saved successfully",
                       type = "success")
          }
        },
        error = function(cond)
        {
          # error popup
          {
            show_alert(title = "Error !!",
                       text = "Unable to save. Error occured",
                       type = "error")
            message(cond)
          }
          
        },
        warning = function(cond)
        {
          # error popup
          {
            show_alert(title = "Error !!",
                       text = "Unable to save. Error occured",
                       type = "error")
            message(cond)
          }
          
        })
      }
    }

  })
  output$download_sample_file <- downloadHandler(
    filename = function() {
      paste(input$file_type, " Sample File.csv", sep = "")
    },
    content = function(file) {
      dat <-
        read.csv(paste(
          "Sample Files\\",
          paste(input$file_type, " Sample File.csv", sep = ""),
          sep = ""
        ))
      write.csv(dat, file)
    }
  )
  
  observeEvent(input$restore_data_query_btn, {
    
    file = paste0(input$select_disco_ud,"-",
                  input$year_uploadData ,"-",
                  input$file_type)
    content_list=get_drive_folder_content(Data_backup_dir, file)
    shinyalert(html = TRUE, text = tagList(
      selectInput(
        inputId = "restore_date",
        label = "List of Available Files:",
        choices = content_list ,#c(list.files("Resources")),
        size = length(content_list),selectize = FALSE
      )    
      ), confirmButtonText = "Restore", showCancelButton = TRUE,
    callbackR = function(restore) {
      if(isTRUE(restore))
      {
      local_filename = paste0(input$select_disco_ud,"-",
                   input$year_uploadData ,"-",
                   input$file_type,".csv")
      show_modal_spinner(text = "Restoring...")
      download_files_from_google_drive(input$restore_date,Data_backup_dir,Data_dir,local_filename)
      remove_modal_spinner()
      }
      }
    )

  })
  
  ################################################# View Data ---------------------------------------------------------------
  observe({
    input$file_type_viewData
    input$select_disco_viewData
    input$year_viewData
    
    
    file_name = paste(
      input$select_disco_viewData,
      "-",
      input$year_viewData,
      "-",
      input$file_type_viewData,
      ".csv",
      sep = ""
    )
    file_path = paste(Data_dir, file_name, sep = "")
    
    if (file.exists(file_path))
    {
      RV$file_exist_for_view = TRUE
      RV$df_view <- read.csv(file_path)
      
      
    }
    else{
      if (input$file_type_viewData != "") {
        show_alert(
          title = "File Not Found !!",
          text = paste(input$file_type_viewData, " File Does not Exist", sep =
                         ""),
          type = "error"
        )
        
        RV$df_view <- NULL
      }
    }
    
  })
  
  output$input_data_table <- renderDataTable(RV$df_view,
                                             options = list(
                                               pageLength = 10,
                                               width = "100%",
                                               scrollX = TRUE
                                             ))
  
  ################################################Reset data when tab changes
  observeEvent(input$menu, {
    if ((input$menu) == "view_graph")
    {
      ## Reset "Select Figure" input value
      updateTextInput(session, "select_figure_db", value = "")
    }
    else if ((input$menu) == "view_data")
    {
      ## Reset "Select File type" input value
      updateTextInput(session, "file_type_view", value = "")
      ## Set Table Data to NULL
      RV$df_view <- NULL
      
    }
    else if ((input$menu) == "set_configuration")
    {
      updateTextInput(session, "file_type_view", value = "")
      ## Set Table Data to NULL
      RV$df_view <- NULL
      
    }
  })
  
  ################################################# Browse Table and Figures ###########################################
  
  observe({
    input$select_figure_db
    input$select_disco_db
    if (input$select_figure_db == "Cateogry Wise Sales Forecast")
    {
      selected_disco <- input$select_disco_db
      
      #get file path
      file_name = paste(selected_disco,
                        "-",
                        config_env$base_year,
                        "-Planned Load Data.csv",
                        sep = "")
      file_path = paste(Data_dir, file_name, sep = "")
      
      #check if file exists
      if (file.exists(file_path))
      {
        RV$file_exist_for_view = TRUE
        #read file
        df <- read.csv(file_path)
        #replace NA's with 0
        df <- replace(df, is.na(df), 0)
        
        df <-
          df[df$year == 0, ][categories_var] # get value for base year only
        df <- data.frame(t(colSums(df)))
        #get growth rate for selected disco
        growth_rate_for_each_category = data.frame(matrix(ncol = length(categories_var), nrow =
                                                            1))
        colnames(growth_rate_for_each_category)  <- categories_var
        
        gr <- (config_DFs$growth_rate_df)
        for (i in (1:length(categories_var)))
        {
          growth_rate_for_each_category[[categories_var[i]]] =   gr[selected_disco, categories_var[i]]
          
        }
        
        start_year <- as.numeric(config_env$base_year)
        df <-
          forecast_using_cagr (
            df,
            growth_rate_for_each_category,
            as.numeric(config_env$no_of_years),
            start_year
          )
        RV$db_df <- df
        shinyjs::show("db_fig")
      }
      else{
        file_exist_for_view = FALSE
        show_alert(
          title = "Data Not Available !!",
          text = paste(input$file_type_viewData, " Data Does not Exist", sep =
                         ""),
          type = "error"
        )
        shinyjs::hide("db_fig")
        RV$df_view <- NULL
        # RV$db_fig <- NULL
        # output$db_fig <- NULL
        # }
      }
      
    }
    else if (input$select_figure_db == "Computed Peak Demand")
    {
      selected_disco <- input$select_disco_db
      
      file_name = paste(selected_disco,
                        "-",
                        config_env$base_year,
                        "-Planned Load Data.csv",
                        sep = "")
      file_path = paste(Data_dir, file_name, sep = "")
      
      if (file.exists(file_path))
      {
        RV$file_exist_for_view = TRUE
        df <- read.csv(file_path)
        #remove NA's
        df <-  df[!is.na(df$area_type), ]
        df <-  df[!is.na(df$area), ]
        
        # pd_input
        peak_demand_input <<-
          data.frame(
            sind_cf = config_env$sind_cf,
            ptub_cf = config_env$ptub_cf,
            
            sub_area1_cf = config_env$sub_area1_cf,
            sub_area2_cf = config_env$sub_area2_cf,
            
            sub_area3_cf = config_env$sub_area3_cf,
            area_cf = config_env$area_cf,
            
            disco_cf_pd = config_env$disco_cf_pd,
            trans_losses_pd = config_env$trans_losses_pd,
            dist_losses_pd = config_env$dist_losses_pd
          )
        # get peak_demand for base_year
        pd_base_year <-
          calculate_base_year_peak_demand(df, peak_demand_input)
        
        #get growth rate for selected DISCO
        selected_disco <- input$select_disco_db
        
        #get growth rate for selected disco
        growth_rate_for_each_category = data.frame(matrix(ncol = length(categories_var), nrow =
                                                            1))
        colnames(growth_rate_for_each_category)  <- categories_var
        gr <- (config_DFs$growth_rate_df)
        for (i in (1:length(categories_var)))
        {
          growth_rate_for_each_category[[categories_var[i]]] =   gr[selected_disco, categories_var[i]]
          
        }
        start_year <- as.numeric(config_env$base_year)
        
        # forecast using cagr
        pd_forecast <<-
          forecast_using_cagr (
            pd_base_year,
            growth_rate_for_each_category,
            as.numeric(config_env$no_of_years),
            start_year
          )
        
        #Converting Peak demand from Mega to kilo
        
        # Calculate Total Energy
        pd_forecast$total_energy = rowSums(pd_forecast[, categories_var])
        
        # calculate dist and trans loss
        for (i in 2:nrow(pd_forecast)) {
          pd_forecast$trans_loss[i]  <-
            pd_forecast$trans_loss[i - 1] - pd_forecast$trans_loss[i - 1] * (peak_demand_input$trans_losses_pd /
                                                                               100)
          pd_forecast$dist_loss[i]  <-
            pd_forecast$dist_loss[i - 1] - pd_forecast$dist_loss[i - 1] * (peak_demand_input$dist_losses_pd /
                                                                             100)
        }
        
        # Calculate loss percantage
        pd_forecast$dist_loss_percentage <-
          pd_forecast$dist_loss / pd_forecast$total_energy * 100
        pd_forecast$trans_loss_percentage <-
          pd_forecast$trans_loss / pd_forecast$total_energy * 100
        
        pd_forecast$total_loss = pd_forecast$dist_loss + pd_forecast$trans_loss
        pd_forecast$total_loss_percentage = pd_forecast$dist_loss_percentage + pd_forecast$trans_loss_percentage
        
        # Calculate energy_sent_out
        pd_forecast$energy_sent_out <-
          pd_forecast$total_energy / (1 - (pd_forecast$total_loss_percentage / 100))
        # Calculate peak demand
        Load_factor_percentage = (pd_forecast$energy_sent_out[1]) / (pd_forecast$peak[1] * 8760) *
          100
        
        pd_forecast$peak[2:nrow(pd_forecast)] <-
          (pd_forecast$energy_sent_out[2:nrow(pd_forecast)]) / (Load_factor_percentage * 8760) *
          100
        
        RV$db_df <- pd_forecast
        shinyjs::show("db_fig")
      }
      else{
        file_exist_for_view = FALSE
        show_alert(
          title = "Data Not Available !!",
          text = paste(input$file_type_viewData, " Data Does not Exist", sep =
                         ""),
          type = "error"
        )
        shinyjs::hide("db_fig")
        RV$df_view <- NULL
      }
    }
    
    else if (input$select_figure_db == "Energy Purchase vs. Energy Sale")
    {
      selected_disco <- input$select_disco_db
      
      file_name = paste(selected_disco,
                        "-",
                        config_env$base_year,
                        "-Planned Load Data.csv",
                        sep = "")
      file_path = paste(Data_dir, file_name, sep = "")
      
      if (file.exists(file_path))
      {
        RV$file_exist_for_view = TRUE
        df <- read.csv(file_path)
        #remove NA's
        df <-  df[!is.na(df$area_type), ]
        df <-  df[!is.na(df$area), ]
        
        # pd_input
        peak_demand_input <<-
          data.frame(
            sind_cf = config_env$sind_cf,
            ptub_cf = config_env$ptub_cf,
            
            sub_area1_cf = config_env$sub_area1_cf,
            sub_area2_cf = config_env$sub_area2_cf,
            
            sub_area3_cf = config_env$sub_area3_cf,
            area_cf = config_env$area_cf,
            
            disco_cf_pd = config_env$disco_cf_pd,
            trans_losses_pd = config_env$trans_losses_pd,
            dist_losses_pd = config_env$dist_losses_pd
          )
        # get peak_demand for base_year
        pd_base_year <-
          calculate_base_year_peak_demand(df, peak_demand_input)
        
        #get growth rate for selected DISCO
        selected_disco <- input$select_disco_db
        
        #get growth rate for selected disco
        growth_rate_for_each_category = data.frame(matrix(ncol = length(categories_var), nrow =
                                                            1))
        colnames(growth_rate_for_each_category)  <- categories_var
        gr <- (config_DFs$growth_rate_df)
        for (i in (1:length(categories_var)))
        {
          growth_rate_for_each_category[[categories_var[i]]] =   gr[selected_disco, categories_var[i]]
          
        }
        start_year <- as.numeric(config_env$base_year)
        
        # forecast using cagr
        pd_forecast <<-
          forecast_using_cagr (
            pd_base_year,
            growth_rate_for_each_category,
            as.numeric(config_env$no_of_years),
            start_year
          )
        
        #Converting Peak demand from Mega to kilo
        
        # Calculate Total Energy
        pd_forecast$total_energy = rowSums(pd_forecast[, categories_var])
        # pd_forecast <<- pd_forecast
        # peak_demand_input <<- peak_demand_input
        
        # calculate dist and trans loss
        for (i in 2:nrow(pd_forecast)) {
          pd_forecast$trans_loss[i]  <-
            pd_forecast$trans_loss[i - 1] - pd_forecast$trans_loss[i - 1] * (peak_demand_input$trans_losses_pd /
                                                                               100)
          pd_forecast$dist_loss[i]  <-
            pd_forecast$dist_loss[i - 1] - pd_forecast$dist_loss[i - 1] * (peak_demand_input$dist_losses_pd /
                                                                             100)
        }
        
        # Calculate loss percantage
        pd_forecast$dist_loss_percentage <-
          pd_forecast$dist_loss / pd_forecast$total_energy * 100
        pd_forecast$trans_loss_percentage <-
          pd_forecast$trans_loss / pd_forecast$total_energy * 100
        
        pd_forecast$total_loss = pd_forecast$dist_loss + pd_forecast$trans_loss
        pd_forecast$total_loss_percentage = pd_forecast$dist_loss_percentage + pd_forecast$trans_loss_percentage
        
        # Calculate energy_sent_out
        pd_forecast$energy_sent_out <-
          pd_forecast$total_energy / (1 - (pd_forecast$total_loss_percentage / 100))
        # Calculate peak demand
        Load_factor_percentage = (pd_forecast$energy_sent_out[1]) / (pd_forecast$peak[1] * 8760) *
          100
        
        pd_forecast$peak[2:nrow(pd_forecast)] <-
          (pd_forecast$energy_sent_out[2:nrow(pd_forecast)]) / (Load_factor_percentage * 8760) *
          100
        
        
        pd_forecast$energy_purchase = pd_forecast$energy_sent_out + pd_forecast$total_loss
        
        
        RV$db_df <- pd_forecast
        shinyjs::show("db_fig")
      }
      else{
        file_exist_for_view = FALSE
        show_alert(
          title = "Data Not Available !!",
          text = paste(input$file_type_viewData, " Data Does not Exist", sep =
                         ""),
          type = "error"
        )
        shinyjs::hide("db_fig")
        RV$df_view <- NULL
      }
    }
    
    
    
  })
  
  observeEvent(input$browse_data_no_of_years , {
    RV$db_year <- input$browse_data_no_of_years
  })

  
  output$db_fig <- renderPlotly({
    if (input$browse_data_no_of_years == "" || #RV$df_view == NULL ||
        input$select_figure_db == "" || !RV$file_exist_for_view)
    {
      return(NULL)
      
    }
    
    else if (input$select_figure_db == "Cateogry Wise Sales Forecast")
    {
      shinyjs::show("browse_data_no_of_years")
      
      df <- RV$db_df
      lbl = Categories
      curr_yr = RV$db_year
      
      vals = (df[df$year == curr_yr, categories_var])#/(10^9) # to convert itno MW
      vals <- unname(unlist(list(vals)))
      title = paste("Cateogry Wise Sales Forecast",
                    curr_yr,
                    "in",
                    input$select_disco_db)
      RV$db_fig <- display_pie_chart(lbl, vals, title)
      
    }
    else if (input$select_figure_db == "Computed Peak Demand")
    {
      shinyjs::hide("browse_data_no_of_years")
      df <- RV$db_df
      lbl = df$year
      
      vals = df$peak
      vals <- unname(unlist(list(vals)))
      title = paste(
        "Computed Peak Demand Forecast of",
        input$select_disco_db,
        "(",
        df$year[1],
        "-",
        df$year[length(lbl)],
        ")"
      )
      RV$db_fig <- display_line_chart(lbl, vals, title)
      
    }
    else if (input$select_figure_db == "Energy Purchase vs. Energy Sale")
    {
      shinyjs::hide("browse_data_no_of_years")
      
      xlbl <- "Year"
      y1lbl <- "Energy Purchase (MWh)"
      y2lbl <- "Energy Sale (MWh)"
      
      title <-
        paste(input$select_disco_db, "-", input$select_figure_db)
      input_data <- RV$db_df
      y1data <- input_data$energy_purchase
      y1data <- round(y1data / (10 ^ 6), 2)
      
      y2data <- input_data$energy_sent_out
      y2data <- round(y2data / (10 ^ 6), 2)
      
      xdata <- as.character(input_data$year)
      
      RV$db_fig <-
        display_bar_chart(xlbl, y1lbl, y2lbl , y2data, y1data, xdata, title)
    }
    
    RV$db_fig
  })
  
  
  ################################################# Configuration UI ###########################################
  observeEvent(input$choose_config_category, {
    if (input$choose_config_category == 'General')
    {
      updateNumericInput(session , "base_year" , value =   config_env$base_year$base_year)
      updateNumericInput(session , "no_of_years" , value =   config_env$no_of_years$no_of_years)
    }
    else if (input$choose_config_category == 'Growth Rates') {
      # df<- config_DFs$growth_rate_df
      output$gr_config_ui <-
        renderDataTable(
          config_DFs$growth_rate_df,
          editable = list(target = "cell"),
          selection = "none",
          extensions = "AutoFill",
          callback = JS(callback),
          caption = "Growth Rate in %",
          options = list(autoFill = TRUE,
                         dom = 't')
        )
    }
    else if (input$choose_config_category == 'Coincidence Factor and Losses') {
      updateNumericInput(session , "sind_cf" , value = config_env$sind_cf$sind_cf)
      updateNumericInput(session , "ptub_cf", value = config_env$ptub_cf$ptub_cf)
      updateNumericInput(session ,
                         "sub_area1_cf",
                         value = config_env$sub_area1_cf$sub_area1_cf)
      updateNumericInput(session ,
                         "sub_area2_cf",
                         value = config_env$sub_area2_cf$sub_area2_cf)
      updateNumericInput(session ,
                         "sub_area3_cf",
                         value = config_env$sub_area3_cf$sub_area3_cf)
      updateNumericInput(session , "area_cf" , value = config_env$area_cf$area_cf)
      updateNumericInput(session ,
                         "trans_losses_pd",
                         value = config_env$trans_losses_pd$trans_losses_pd)
      updateNumericInput(session ,
                         "dist_losses_pd",
                         value =  config_env$dist_losses_pd$dist_losses_pd)
      updateNumericInput(session , "disco_cf_pd" , value = config_env$disco_cf_pd$disco_cf_pd)
      
    }
    
  })
  
  output$browse_data_no_of_years_ui <- renderUI({
    start_year <- as.numeric(config_env$base_year)
    ny <- as.numeric(config_env$no_of_years)
    year_list <-
      seq(start_year, (start_year + ny))
    
    selectInput("browse_data_no_of_years", "Select Year", year_list)
  })
  
  ## Edit Growth Rate Table JS
  {
    callback <- c(
      "var tbl = $(table.table().node());",
      "var id = tbl.closest('.datatables').attr('id');",
      "table.on('autoFill', function(e, datatable, cells){",
      "  var out = [];",
      "  for(var i = 0; i < cells.length; ++i){",
      "    var cells_i = cells[i];",
      "    for(var j = 0; j < cells_i.length; ++j){",
      "      var c = cells_i[j];",
      "      var value = c.set === null ? '' : c.set;",
      # null => problem in R
      "      out.push({",
      "        row: c.index.row + 1,",
      "        col: c.index.column,",
      "        value: value",
      "      });",
      # to color the autofilled cells, uncomment the two lines below
      #  "      $(table.cell(c.index.row, c.index.column).node())",
      #  "        .css('background-color', 'yellow');",
      "    }",
      "  }",
      "  Shiny.setInputValue(id + '_cells_filled:DT.cellInfo', out);",
      "  table.rows().invalidate();",
      # this updates the column type
      "});"
    )
  }
  
  ## Saving  Growth Rate Table in RV
  observeEvent(input$gr_config_ui_cell_edit, {
    proxy1 = dataTableProxy('table1')
    
    info = input$gr_config_ui_cell_edit
    str(info)
    i = info$row
    j = info$col
    v = info$value
    
    # get new value
    new_value <-
      DT::coerceValue(v, config_DFs$growth_rate_df [i, j])
    
    # update rv
    config_DFs$growth_rate_df [i, j] <<- new_value
    
    # update browser
    replaceData(proxy1, config_DFs$growth_rate_df , resetPaging = TRUE)  # important
  })
  
  
  ## Auto Fill Growth Rate Configuration Table
  observeEvent(input$auto_fill_config_fields, {
    if (input$auto_fill_config_fields == TRUE) {
      if (input$choose_config_category == 'General')
      {
        updateNumericInput(session , "base_year" , value =   base_year)
        updateNumericInput(session , "no_of_years" , value =   no_of_years)
        
      }
      else if (input$choose_config_category == 'Growth Rates') {
        growth_rate_df <-
          read.csv(paste(Config_dir, "growth_rate_df_autofill.csv", sep = ""))
        row.names(growth_rate_df) <- Disco_List
        config_DFs$growth_rate_df <<- growth_rate_df[, -1]
      }
      else if (input$choose_config_category == 'Coincidence Factor and Losses')
      {
        updateNumericInput(session , "sind_cf" , value =   sind_cf)
        updateNumericInput(session , "ptub_cf", value = ptub_cf)
        updateNumericInput(session , "sub_area1_cf", value = sub_area1_cf)
        updateNumericInput(session , "sub_area2_cf", value = sub_area2_cf)
        updateNumericInput(session , "sub_area3_cf", value = sub_area3_cf)
        updateNumericInput(session , "area_cf" , value = area_cf)
        updateNumericInput(session , "trans_losses_pd", value = trans_losses_pd)
        updateNumericInput(session , "dist_losses_pd", value =  dist_losses_pd)
      }
    }
    
    
    
  })
  
  ## General Configuration Save Button
  observeEvent(input$config_save_btn, {
    if (input$choose_config_category == 'General')
    {
      config_env$base_year =  input$base_year
      config_env$no_of_years =  input$no_of_years
      # Write whole Config_env RV to file
      tryCatch({
        new_env <- new.env()
        for (obj in names(isolate(reactiveValuesToList(config_env)))) {
          assign(obj, unlist(config_env[[obj]], use.names = F), envir = new_env)
          save(
            list = ls(new_env),
            file = paste(Config_dir, "config.RData", sep = ""),
            envir = new_env
          )
          
        }
        # save Data to drive
        show_modal_spinner(text = "Saving...")
        upload_files_to_google_drive("config.RData", Config_dir)
        remove_modal_spinner()
        
        # saved successfully popup
        {
          show_alert(title = "Success !!",
                     text = "Configuration saved successfully",
                     type = "success")
        }
        
      },
      error = function(cond)
      {
        # error popup
        {
          show_alert(title = "Error !!",
                     text = "Unable to save. Error occured",
                     type = "error")
          message(cond)
        }
        
      })
      
    }
    else if (input$choose_config_category == 'Growth Rates')
    {
      tryCatch({
        #Save locally
        write.csv(
          config_DFs$growth_rate_df ,
          paste(Config_dir, "growth_rate_df.csv", sep = "")
        )
        
        # save Data to Google Drive
        show_modal_spinner(text = "Saving...")
        upload_files_to_google_drive("growth_rate_df.csv", Config_dir)
        remove_modal_spinner()
        
        # saved successfully popup
        {
          show_alert(title = "Success !!",
                     text = "Configuration saved successfully",
                     type = "success")
        }
        
      },
      error = function(cond)
      {
        # error popup
        {
          show_alert(title = "Error !!",
                     text = "Unable to save. Error occured",
                     type = "error")
          message(cond)
        }
        
      })
      
    }
    
    else if (input$choose_config_category == 'Coincidence Factor and Losses')
    {
      config_env$sind_cf = input$sind_cf
      config_env$ptub_cf =  input$ptub_cf
      
      config_env$sub_area1_cf =  input$sub_area1_cf
      config_env$sub_area2_cf =  input$sub_area2_cf
      config_env$sub_area3_cf =  input$sub_area3_cf
      config_env$area_cf =  input$area_cf
      config_env$disco_cf_pd =  input$disco_cf_pd
      
      config_env$trans_losses_pd =  input$trans_losses_pd
      config_env$dist_losses_pd =  input$dist_losses_pd
      
      #Saving
      tryCatch({
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
        
        # save Data to drive
        show_modal_spinner(text = "Saving...")
        upload_files_to_google_drive("config.RData", Config_dir)
        remove_modal_spinner()
        
        # saved successfully popup
        {
          show_alert(title = "Success !!",
                     text = "Configuration saved successfully",
                     type = "success")
        }
      },
      error = function(cond)
      {
        # error popup
        {
          show_alert(title = "Error !!",
                     text = "Unable to save. Error occured",
                     type = "error")
          message(cond)
        }
        
      })
      
      
      
    }
    
    
    
  })
  ################################################# Report Generation ###########################################
  output$download_report_btn <- downloadHandler(
    filename =  'report.html',
    contentType =  'html',
    content = function(filename) {
      if (file.exists('report.html'))file.remove('report.html')
      if (file.exists('PMS-doc.md'))file.remove('PMS-doc.md')

      htmlKnitted <- rmarkdown::render("PMS-doc.Rmd",
                                       quiet = TRUE)
      x <-
        readLines(con = htmlKnitted) #"plain" version, without knitrBootstrap
      writeLines(x, con = filename)
    }
  )
  ################################################# Resources ###########################################
  output$download_resource <- downloadHandler(
    filename = function() {
      input$resources
    },
    content = function(file) {
      ppt_report = read_pptx(paste0(Resources_dir, input$resources))
      print(ppt_report, target = file)
      
    }
  )
  
  
}

shinyApp(ui = UI,
         server = server)
