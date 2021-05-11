
#### Helper Functions

verify_column_names <- function(df, columns, file_name)
{
  if (!is.data.frame(df)) {
    return(paste("Error!!! ", file_name, "must not be Empty"))
  }
  if(!all(i <- rlang::has_name(df,columns)))
    return(sprintf(
      "%s file doesn't contain: %s",
      file_name,
      paste(columns[!i], collapse=", ")))
}

column_validation <- function(df, required_cols,char_cols, filename)
{

  error_msg = verify_column_names (df, required_cols,filename)
  missing_cols_error_msg<-NULL
  extra_cols_error_msg <- NULL
  type_checking_error_msg <- NULL


  if (is.null(error_msg)) ## all columns in Input file are Relevant
  {
    # if ( length(colnames(df)) < length(required_cols)  ) # check for missing columns
    # {
    #
    #
    #   missing_cols <- required_cols[-pmatch(colnames(df), required_cols)]
    #   missing_cols_error_msg =  do.call(paste, c(as.list(extra_cols), sep = " , "))
    #   missing_cols_error_msg = paste("Missing columns : " ,missing_cols_error_msg , sep =" " )
    #
    # }
    if(length(colnames(df)) > length(required_cols) ) # check for extra columns
    {
      extra_cols <- colnames(df)[-pmatch(required_cols,colnames(df))]
      extra_cols_error_msg =  do.call(paste, c(as.list(extra_cols), sep = " , "))
      extra_cols_error_msg = paste("Remove Extra columns : " ,extra_cols_error_msg , sep =" " )

    }

    if(length(colnames(df))>0) ## check type of data
    {


      type_checking_error_msg = NULL
      numeric_cols <- required_cols[-pmatch(char_cols,required_cols)]

      # verify numeric cols
      for ( i in 1:length(numeric_cols))
      {
        x <- df[[numeric_cols[i]]]

        if( class(x) != "integer" )
        {
          type_checking_error_msg = paste( type_checking_error_msg,"Column", numeric_cols[i] ,"should be numeric ;" , sep=" ")
        }
      }
      # verify char cols

      for ( i in 1:length(char_cols))
      {
        x <- df[[char_cols[i]]]

        if( class(x) != "character" )
        {
          type_checking_error_msg = paste(  type_checking_error_msg,"Column", char_cols[i] ,"should be character" , "\n",sep=" ")
        }
      }

    }

  }
  # print(paste0("Errors ",error_msg,missing_cols_error_msg,extra_cols_error_msg,type_checking_error_msg,sep=" ; " ))
  final_error_message = paste0(error_msg ,"\n", missing_cols_error_msg , "<br>")
  final_error_message = paste0(final_error_message , extra_cols_error_msg , "<br>")
  final_error_message = paste0(final_error_message , type_checking_error_msg )
  
  if( is.null( error_msg) & is.null ( missing_cols_error_msg )  
     &  is.null( extra_cols_error_msg)   & is.null ( type_checking_error_msg ))
  {
    return (NULL)
  }
  else{
    print("Error")
    print(final_error_message)
    return(final_error_message)
    # return (paste0(error_msg,missing_cols_error_msg,extra_cols_error_msg,type_checking_error_msg,sep=" ; " ))
  }

}



####Variables

Data_dir = "Data/"


{
  Proposed_Grid_Data_cols <- c("sr_no" , "disco_no","grid_no","existing_grid_name","proposed_grid_no","proposed_grid_name",
                               "transient_grid_no","transient_grid_name","commisioning_year","132_kv_line","domestic","commercial","public_light",
                               "small_industry","public_tube_well","bulk_energy","bulk_demand","medium_large_industry_energy","medium_large_industry_demand")

  Proposed_Grid_Data_char_cols <- c("existing_grid_name","proposed_grid_name","transient_grid_name")


  Planned_Load_Data_cols <- c("area_code","power_market","load_center","transient_grid_flag","grid_no","proposed_grid_no",
                              "province","division","district","area","sub_area",
                              "area_type","year","consumer_type","name_of_area","name_of_sub_area",
                              "domestic","commercial","public_light",
                              "small_industry","public_tube_well","bulk_energy","bulk_demand","medium_large_industry_energy","medium_large_industry_demand",
                              "domestic_load_factor","commercial_load_factor",
                              "small_industry_load_factor","public_tube_well_load_factor",
                              "small_industry_coincidence_factor","public_tube_well_coincidence_factor"

  )

  Planned_Load_Data_char_cols <- c("name_of_area","name_of_sub_area")


  Grid_Load_Data_cols <- c("area_code","power_market","load_center","transient_grid_flag","grid_no","proposed_grid_no",
                           "province","division","district","area","sub_area","area_type",
                           "year","consumer_type","name_of_area","name_of_substation",
                           "domestic","commercial","public_light",
                           "small_industry","public_tube_well","bulk_energy","bulk_demand","medium_large_industry_energy","medium_large_industry_demand",
                           "domestic_load_factor","commercial_load_factor",
                           "small_industry_load_factor","public_tube_well_load_factor",
                           "small_industry_coincidence_factor","public_tube_well_coincidence_factor"

  )

  Grid_Load_Data_char_cols <- c("name_of_area","name_of_substation")


  List_of_Transformers_on_Grid_Data <-  c("grid_no","grid_name","installed_transformer_1","installed_transformer_2","installed_transformer_3","installed_transformer_4","installed_transformer_5","installed_transformer_6","total_installed_capacity")

  List_of_Transformers_on_Grid_char_Data <-  c("grid_name")
}