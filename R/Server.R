
server <- function(input,output,session){
  
  observeEvent(input$save_file, {
    # closeAlert(session, "file_error")
    # 
    
        
    
    ext <- tools::file_ext(input$input_file$datapath)
    if (ext != "csv"){
      ## Make sure input file is a CSV file
       
      createAlert(session, "file_error", "File Extension Error", style = "warning", title = "Oops",
                  content = "Input File should be a CSV file.", append = FALSE)
    }
    else{
      df <- read.csv(input$input_file$datapath)

      file_ = input$file_type

      if ( file_ == "Planned Load Data")
      {
        input_file_error = column_validation (df, Planned_Load_Data_cols,Planned_Load_Data_char_cols, file_)
      }

      else if ( file_ == "Proposed Grid Data")
      {
        input_file_error = column_validation (df, Proposed_Grid_Data_cols,Proposed_Grid_Data_char_cols, file_)
      }

      else if ( file_ == "List of Transformers on Grid (MVA)")
      {
        input_file_error = column_validation (df, List_of_Transformers_on_Grid_Data,List_of_Transformers_on_Grid_char_Data, file_)
      }

      else if ( file_ == "Grid Load Data")
      {
        input_file_error = column_validation (df, Grid_Load_Data_cols,Grid_Load_Data_char_cols, file_)
      }

      
      print( input_file_error )
      if ( !is.null( input_file_error ) )
      {
        createAlert(session, "file_error", "File Format Error",style = "danger", title = "File Format Error",
                  content = input_file_error, append = FALSE)
        
      }
      else{
        
      ### Save file locally
      # print(getwd())
      file_name = paste(input$file_type,".csv", sep="")
      file_path = paste( Data_dir,file_name, sep="")
      write.csv(df , file_path, row.names = FALSE)
      createAlert(session, "file_error", "File uploaded sucessfully",style = "success", title = "Success",
                  content = "Input File uploaded sucessfully", append = FALSE)

        }

    }


    
  
    })
  
  RV <- reactiveValues(
    file_exist_for_view = FALSE,
    df_view = NULL
    
    )
                  
  output$input_data_table <- renderDataTable(
    RV$df_view
    
    )
  
  observeEvent(input$file_type_view, { 
    

    file_name = paste(input$file_type_view,".csv", sep="")
    file_path = paste( Data_dir,file_name, sep="")
    
    if (file.exists(file_path))
    {
      
      RV$file_exist_for_view = TRUE
      RV$df_view <- read.csv(file_path)
      
      
    }
    else{
      # createAlert(session, "file_view_error", "File Not Found",style = "danger", title = "File Not Found",
                  # content = paste(input$file_type_view, " File Does not Exist", sep=""), append = FALSE)
      if (input$file_type_view != ""){
        
      show_alert(
        title = "File Not Found !!",
        text = paste(input$file_type_view, " File Does not Exist", sep=""),
        type = "error"
      )
      
      RV$df_view <- NULL
      }
    }
    
    })
  
}
