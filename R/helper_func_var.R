library(dplyr)
library(plotly)
#### Helper Functions

verify_column_names <- function(df, columns, file_name)
{
  if (!is.data.frame(df)) {
    return(paste("Error!!! ", file_name, "must not be Empty"))
  }
  if (!all(i <- rlang::has_name(df, columns)))
    return(sprintf(
      "%s file doesn't contain: %s",
      file_name,
      paste(columns[!i], collapse = ", ")
    ))
}

column_validation <- function(df, required_cols, char_cols, filename)
{
  error_msg = verify_column_names (df, required_cols, filename)
  missing_cols_error_msg <- NULL
  extra_cols_error_msg <- NULL
  type_checking_error_msg <- NULL
  
  
  if (is.null(error_msg))
    ## all columns in Input file are Relevant
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
    if (length(colnames(df)) > length(required_cols))
      # check for extra columns
    {
      extra_cols <- colnames(df)[-pmatch(required_cols, colnames(df))]
      extra_cols_error_msg =  do.call(paste, c(as.list(extra_cols), sep = " , "))
      extra_cols_error_msg = paste("Remove Extra columns : " , extra_cols_error_msg , sep =
                                     " ")
      
    }
    
    if (length(colnames(df)) > 0)
      ## check type of data
    {
      type_checking_error_msg = NULL
      numeric_cols <-
        required_cols[-pmatch(char_cols, required_cols)]
      
      # verify numeric cols
      for (i in 1:length(numeric_cols))
      {
        x <- df[[numeric_cols[i]]]
        
        if (class(x) != "integer")
        {
          type_checking_error_msg = paste(
            type_checking_error_msg,
            "Column",
            numeric_cols[i] ,
            "should be numeric ;" ,
            sep = " "
          )
        }
      }
      # verify char cols
      
      for (i in 1:length(char_cols))
      {
        x <- df[[char_cols[i]]]
        
        if (class(x) != "character")
        {
          type_checking_error_msg = paste(
            type_checking_error_msg,
            "Column",
            char_cols[i] ,
            "should be character" ,
            "\n",
            sep = " "
          )
        }
      }
      
    }
    
  }
  # print(paste0("Errors ",error_msg,missing_cols_error_msg,extra_cols_error_msg,type_checking_error_msg,sep=" ; " ))
  final_error_message = paste0(error_msg , "\n", missing_cols_error_msg , "<br>")
  final_error_message = paste0(final_error_message , extra_cols_error_msg , "<br>")
  final_error_message = paste0(final_error_message , type_checking_error_msg)
  
  if (is.null(error_msg) & is.null (missing_cols_error_msg)
      &
      is.null(extra_cols_error_msg)   &
      is.null (type_checking_error_msg))
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


forecast_using_cagr <- function (df, growth_rate_for_each_category, n,start_year) {
    df <- df[rep(seq_len(nrow(df)), n + 1),]
    for (c in(1:ncol(df)))
    {
      l <- seq(1:(n+1) )-1
      
      # df[, c]  = df[, c] * (rep(growth_rate_for_each_category[c], n) / 100) +1) ^ l
      m =  (unname( unlist( rep( growth_rate_for_each_category[,c], n+1) ))/100  + 1)
      df[,c]  = df[,c]* m^l
      
    }
    df$year = paste(seq(start_year, (start_year + n)), seq(start_year + 1, (start_year +
                                                                              n + 1)), sep = "-")
    
    return (df)
    
  }

display_pie_chart <- function(lbl ,vals, title){
  
  
  # mw = 10^9
  fig <- plot_ly(labels = lbl, values = vals, type = 'pie')
  fig <- fig %>% layout(title = title,
                        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
                         ) %>%
    layout(plot_bgcolor='transparent') %>% 
    layout(paper_bgcolor='transparent') #will also accept paper_bgcolor='black' or paper_bgcolor='transparent'
  # fig <- fig %>%
  #   add_trace(
  #     text = paste( vals ,"MW")
  #     # hoverinfo = 'text',
  #     # marker = list(color='green'),
  #     # showlegend = F
  #   )
  # fig <- plot_ly(labels = categories, values = df[1,categories]/10^9, type = 'pie')
  # fig <- fig %>% layout(title = paste("Forecasted Category Wise Sale ", df[1,c("year")] , sep= ""),
  #                       xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
  #                       yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  return(fig)
  
}


# 
# calculate_cateogry_wise_sales_forecast <- function (n) {
#     ### load latest Setting
#     load("Config.RData")
#     
#     df <- read.csv("Data/Grid Load Data.csv")
#     df <- df[df$year == 0, ][categories_var] # get value for base year only
#     df <- data.frame(t(colSums(df)))
#     
#     # df <- forecast_using_cagr(df, gorwth_rate_for_each_category, 10)
#     
#     # categories_growth_rate = rep(5, ncol(df))
#     
#     # forecast_using_cagr <- function (df, gorwth_rate_for_each_category, n){
#     df = df[rep(seq_len(nrow(df)), n + 1),]
#     
#     
#     for (c in(1:ncol(df))){
#     # for each category
#     l <- seq(0:(n - 1))
#       
#       df[, c]  = df[, c] * (rep(categories_growth_rate[c], n) / 100 +
#                               1) ^ l
#       
#       
#     }
#     start_year <- as.numeric( substr(config_env$base_year,1,4) )
#     df$year = paste(seq(start_year, (start_year + n)), seq(start_year +1, (start_year + n + 1)), sep = "-")
#     
#     
#     
# 
# }
inc <- function(x){eval.parent(substitute(x <- x + 1))}

calculate_cagr_for_all_categories <- function(Historical_Df){
  categories_gr = data.frame( matrix( ncol = length(Categories), nrow=1))
  colnames(categories_gr) <- Categories
  categories_gr[1,]<- 1:length(Categories)
  
  
  return(categories_gr)
}

save_config_data<- function(){

  
  new_env <- new.env()
  # new_env <- emptyenv
  for (obj in names(isolate( reactiveValuesToList(config_env) )) ){
    
    # print(paste("env vals : ", obj ," <-" ,  isolate(config_env[[obj]])))
    
    assign(obj, isolate(config_env[[obj]]), envir = new_env)
    
  }
  
  
  
  # l <- ( isolate( reactiveValuesToList(config_env) ) )
  save(list = ls(new_env), file = paste(Config_dir, "config.RData", sep = ""), envir = new_env)
  
  
 
}


###########File paths
{
Data_dir = "Data/"
Config_dir = "Config/"
}

###########Variables

{
  Proposed_Grid_Data_cols <-
    c(
      "sr_no" ,
      "disco_no",
      "grid_no",
      "existing_grid_name",
      "proposed_grid_no",
      "proposed_grid_name",
      "transient_grid_no",
      "transient_grid_name",
      "commisioning_year",
      "132_kv_line",
      "domestic",
      "commercial",
      "public_light",
      "small_industry",
      "public_tube_well",
      "bulk",
      "bulk_demand",
      "medium_large_industry",
      "medium_large_industry_demand"
    )
  
  Proposed_Grid_Data_char_cols <-
    c("existing_grid_name",
      "proposed_grid_name",
      "transient_grid_name")
  
  
  Planned_Load_Data_cols <-
    c(
      "area_code",
      "power_market",
      "load_center",
      "transient_grid_flag",
      "grid_no",
      "proposed_grid_no",
      "province",
      "division",
      "district",
      "area",
      "sub_area",
      "area_type",
      "year",
      "consumer_type",
      "name_of_area",
      "name_of_sub_area",
      "domestic",
      "commercial",
      "public_light",
      "small_industry",
      "public_tube_well",
      "bulk",
      "bulk_demand",
      "medium_large_industry",
      "medium_large_industry_demand",
      "domestic_load_factor",
      "commercial_load_factor",
      "small_industry_load_factor",
      "public_tube_well_load_factor",
      "small_industry_coincidence_factor",
      "public_tube_well_coincidence_factor"
      
    )
  
  Planned_Load_Data_char_cols <-
    c("name_of_area", "name_of_sub_area")
  
  
  Grid_Load_Data_cols <-
    c(
      "area_code",
      "power_market",
      "load_center",
      "transient_grid_flag",
      "grid_no",
      "proposed_grid_no",
      "province",
      "division",
      "district",
      "area",
      "sub_area",
      "area_type",
      "year",
      "consumer_type",
      "name_of_area",
      "name_of_substation",
      "domestic",
      "commercial",
      "public_light",
      "small_industry",
      "public_tube_well",
      "bulk",
      "bulk_demand",
      "medium_large_industry",
      "medium_large_industry_demand",
      "domestic_load_factor",
      "commercial_load_factor",
      "small_industry_load_factor",
      "public_tube_well_load_factor",
      "small_industry_coincidence_factor",
      "public_tube_well_coincidence_factor"
      
    )
  
  Grid_Load_Data_char_cols <- c("name_of_area", "name_of_substation")
  
  
  List_of_Transformers_on_Grid_Data <-
    c(
      "grid_no",
      "grid_name",
      "installed_transformer_1",
      "installed_transformer_2",
      "installed_transformer_3",
      "installed_transformer_4",
      "installed_transformer_5",
      "installed_transformer_6",
      "total_installed_capacity"
    )
  
  List_of_Transformers_on_Grid_char_Data <-  c("grid_name")
  
  
  Disco_List <-
    c("LESCO", "GESCO", "FESCO", "IESCO", "MEPCO", "TESCO", "QESCO")
  
  categories_var <-
    c(
      "domestic",
      "commercial",
      "public_light",
      "small_industry",
      "medium_large_industry",
      "public_tube_well",
      "bulk"
    ) #### MAke sure It is be same as Schema 
  
  Categories <-
    gsub("(^|[[:space:]])([[:alpha:]])",
         "\\1\\U\\2",
         gsub("_", " ", categories_var),
         perl = TRUE)
  
  
  
}
