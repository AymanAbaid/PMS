library(dplyr)
library(plotly)
#### Helper Functions
library(gargle)
options(gargle_oauth_cache = ".secrets", gargle_oauth_email = TRUE)
library(googledrive)


# filename = "GEPCO-2019-Planned Load Data.csv"
# parent_dir ="Data/"
# local_dir ="Data/"
# drive_download(filename, parent_dir)#, local_dir, filename)

upload_files_to_google_drive = function(filename, parent_dir){
  
  file_path_on_drive = paste0(parent_dir, filename)
  file_path_local = paste0(parent_dir, filename)
  ## Save the data in drive
  drive_upload(media = file_path_local, path=parent_dir, name = filename, overwrite = TRUE)

}

download_files_from_google_drive = function(filename, parent_dir, local_dir, local_file_name){
  download_successfull = FALSE
  if ( filename == "."){
    ## download all files in parent_Directory
    tryCatch(
      {
        ls_ <- googledrive::drive_ls(parent_dir)
        
        for ( i in 1:nrow(ls_))
        {
          filename <- ls_$name[i]
          file_path_on_drive = paste0(parent_dir, filename)
          file_path_local = paste0(local_dir, filename)
          R.utils::withTimeout(  
            drive_download(file_path_on_drive,file_path_local, overwrite = TRUE)
            , timeout = 180, onTimeout = "error")
        }
        download_successfull =TRUE
      }
      ,error = function(e){download_successfull =(FALSE)}
      ,warning = function(w){download_successfull =(FALSE)}
    )
    
  }
  else{
  file_path_on_drive = paste0(parent_dir, filename)
  file_path_local = paste0(local_dir, local_file_name)
  ## download the data in drive and save locally
  tryCatch(
  {
    # R.utils::withTimeout(  
    drive_download(file_path_on_drive,file_path_local ,overwrite = TRUE)
    # , timeout = 180, onTimeout = "error")
    download_successfull =TRUE
    
  }
  ,error = function(e){download_successfull =(FALSE)}
  ,warning = function(w){download_successfull =(FALSE)}
  )
  }
  return (download_successfull)
}

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
    for (c in(1:ncol(growth_rate_for_each_category)))
    {
      l <- seq(1:(n+1) )-1
      
      m =  (unname( unlist( rep( growth_rate_for_each_category[,c], n+1) ))/100  + 1)
      df[,c]  = df[,c]* m^l
      
    }
    df$year = seq(start_year, (start_year + n))

    return (df)
    
  }

display_pie_chart <- function(lbl ,vals, title){
  
  
  fig <- plot_ly(labels = lbl, values = vals, type = 'pie')
  fig <- fig %>% layout(title = title,
                        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
                         ) %>%
    layout(plot_bgcolor='transparent') %>% 
    layout(paper_bgcolor='transparent')
  return(fig)
  
}

display_line_chart <- function(lbl ,vals, title){

  data <- data.frame(lbl, vals)
  fig <- plot_ly(data, x = ~lbl, y = ~vals, type = 'scatter', mode = 'lines+markers',
                 line = list(color = toRGB("lightblue"), width = 4)) 
  fig <- fig %>% layout(title = title, xaxis = list(title = "Year"),
                         yaxis = list (title = "Peak Demand (MWh)") )
  fig <- fig   %>%layout(plot_bgcolor='transparent')%>%
  layout(paper_bgcolor='transparent')%>%
    layout(xaxis = list(
      showgrid = F,
      tickmode = "array",
      tickvals = lbl,
      range = list(min(lbl), max(lbl))
    ), yaxis = list(gridcolor = "gray"))
  
  return(fig)
  
  
}

display_bar_chart <- function(xlbl, y1lbl,y2lbl , y2data, y1data, xdata, title)
  {
  fig2 <- plot_ly(
    x = xdata,
    y = y1data,
    type = "bar",
    showlegend = FALSE,
    marker = list(color =  ~ xdata, showscale = FALSE)
  )
  fig2 <-
    fig2 %>% layout(
      title = title,
      xaxis = list(title = xlbl),
      yaxis = list (title = y1lbl)
    )
  fig2 <-
    fig2 %>% add_trace(
      y = y2data,
      type = 'scatter',
      mode = 'lines+markers',
      showlegend = TRUE,
      name = y2lbl,
      line = list(color = 'purple'),
      hoverinfo = 'text',
      text = ~ paste(
        '</br>',y2lbl,' : ',
        y2data,
        '</br> Year: ',
        xdata
      )
    )
  
  fig2 <- fig2   %>%layout(plot_bgcolor='transparent')%>%
    layout(
      paper_bgcolor='transparent',
      showlegend = TRUE,
      xaxis = list(
        side = "right", showgrid = FALSE),
      yaxis = list(showgrid = FALSE)
    )

  return(fig2)
  
}

calculate_base_year_peak_demand <- function(df, input){
  col_names = categories_var
  new_cols<- paste0(col_names, "_peak_demand", sep="") 
  
  #Converting percenatges to numbers
  input = input/100
  
  new_df<- as.data.frame(matrix(nrow=nrow(df), ncol=length(new_cols)))
  colnames(new_df) = new_cols
  
  #Computing Peak Demand and conversion to MWh from kWh
  new_df$domestic_peak_demand <- (df$domestic/1000)/((df$domestic_load_factor/100) *8760)
  new_df$commercial_peak_demand <- (df$commercial/1000)/((df$commercial_load_factor/100) *8760)
  new_df$public_tube_well_peak_demand <- (df$public_tube_well/1000)/((df$public_tube_well_load_factor/100) *8760)
  new_df$small_industry_peak_demand <- (df$small_industry/1000)/((df$small_industry_load_factor/100) *8760)
  new_df$bulk_peak_demand = df$bulk_demand/1000
  new_df$medium_large_industry_peak_demand = df$medium_large_industry_demand/1000
  new_df$public_light_peak_demand = (df$public_light/1000)/(0.35*8760)
  
  #Replacing NAN and Infinity
  is.na(new_df)<-sapply(new_df, is.infinite)
  new_df <- replace(new_df, is.na(new_df), 0)
  df <- replace(df, is.na(df), 0)
  
  
  #Multiplying By Co_incidence factor
  new_df$small_industry_peak_demand = new_df$small_industry_peak_demand*(input$sind_cf)
  new_df$public_tube_well_peak_demand = new_df$public_tube_well_peak_demand*(input$ptub_cf)
  new_df$Total_Peak_Demand = rowSums(new_df)
  
  #binding columns
  new_df = cbind(df,new_df)
  
  
  Area_type_CF = data.frame(area_type=c(1,2,3),CF=c(input$sub_area1_cf, input$sub_area2_cf,input$sub_area3_cf))
  
  #Aggregating by Area Type
  Peak_demand_Aggregated_by_Area_Type = aggregate(new_df$Total_Peak_Demand,list(new_df$area,new_df$area_type),sum)
  colnames(Peak_demand_Aggregated_by_Area_Type)= c("area","area_type","Peak_Demand")
  
  Peak_demand_Aggregated_by_Area_Type$Peak_demand_xCF<- NA
  
  for (q in 1:nrow(Peak_demand_Aggregated_by_Area_Type)){
    
    Peak_demand_Aggregated_by_Area_Type$Peak_demand_xCF[q] = 
      Peak_demand_Aggregated_by_Area_Type$Peak_Demand[q] * 
      Area_type_CF$CF[Peak_demand_Aggregated_by_Area_Type$area_type[q] == Area_type_CF$area_type]
    
  }
  ## Aggregarting by area
  Peak_demand_Aggregated_by_Area = aggregate(Peak_demand_Aggregated_by_Area_Type$Peak_demand_xCF,
                                             list(Peak_demand_Aggregated_by_Area_Type$area),sum)
  colnames(Peak_demand_Aggregated_by_Area) <- c("area","peak_demand")
  
  Peak_demand_Aggregated_by_Area$peak_demand_xcf =Peak_demand_Aggregated_by_Area$peak_demand * (input$area_cf)
  
  
  
  ###Subtracting losses
  
  DISCO_trans_losses= 1-(input$trans_losses_pd) 
  DISCO_dist_losses= 1-(input$dist_losses_pd)
  Peak_demand_Aggregated_by_Area$peak_demand_xcf = 
    Peak_demand_Aggregated_by_Area$peak_demand_xcf *DISCO_dist_losses*DISCO_trans_losses
  
  DISCO_peak_demand_MW <- sum(Peak_demand_Aggregated_by_Area$peak_demand_xcf)*input$disco_cf_pd
  
  #### output df
  output <- as.data.frame( t(colSums(df[,col_names])) )
  # kilo to Mega conversion
  output_MW <- output/1000
  output_MW$total_energy <- rowSums(output_MW)
  output_MW$dist_loss_percentage <- input$dist_losses_pd
  output_MW$trans_loss_percentage <- input$trans_losses_pd
  output_MW$total_loss_percentage <- output_MW$trans_loss_percentage + output_MW$dist_loss_percentage
  
  output_MW$dist_loss <- (input$dist_losses_pd) * output_MW$total_energy
  output_MW$trans_loss <-(input$trans_losses_pd) * output_MW$total_energy
  output_MW$total_loss <-output_MW$dist_loss+output_MW$trans_loss
  # Energy sent out
  output_MW$energy_sent_out <-output_MW$total_energy/(1-(output_MW$total_loss_percentage))
  output_MW$peak = DISCO_peak_demand_MW
  # output_MW$dist_loss <- (1-(input$dist_losses_pd/100)) * output_MW$total_energy
  # output_MW$trans_loss <-(1-(input$trans_losses_pd/100)) * output_MW$total_energy
  
  return(output_MW)
  
}

inc <- function(x){eval.parent(substitute(x <- x + 1))}

match_substr_to_list <- function(list, sub_str){
  matched = list()
  c = 0
  for(i in 1:length(list))
  {
    if(grepl(sub_str,list[i] ))
    {
      matched[[inc(c)]]=list[i]
      
    }
  }
  return(matched)
  }


calculate_cagr_for_all_categories <- function(Historical_Df){
  categories_gr = data.frame( matrix( ncol = length(Categories), nrow=1))
  colnames(categories_gr) <- Categories
  categories_gr[1,]<- 1:length(Categories)
  
  
  return(categories_gr)
}

get_drive_folder_content = function (Data_backup_dir, file_name)
{
  list_ = drive_ls(Data_backup_dir)$name
  # print(list_)
  meta_data_list = match_substr_to_list(list_, file_name)

  return(meta_data_list)
  # return(list_)
}

###########File paths
{
Data_dir = "Data/"
Config_dir = "Config/"
Resources_dir = "Resources/"
Data_backup_dir = "Backup/"

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
  
  
  List_of_Transformers_on_Grid_Data_cols <-
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
    c("LESCO", "GEPCO", "FESCO","PESCO", "IESCO", "MEPCO", "TESCO", "QESCO","SEPCO")#SEPCO
  
  
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
  
  ## Coincidence Factor
  sind_cf = 56 # Small Industry Coincidence Factor in %
  ptub_cf = 70 # Public Tube Well Coincidence Factor in %
  sub_area1_cf = 85 # Sub Area Type 1 Coincidence Factor in %
  sub_area2_cf = 90 # Sub Area Type 2  Coincidence Factor in %
  sub_area3_cf = 95 # Sub Area Type 3  Coincidence Factor in %
  area_cf = 85 # Area  Coincidence Factor in %
  disco_cf_pd = 90 # Disco Coincidence Factor in %
  
  ## Losses
  trans_losses_pd = 1# Transmission Losses
  dist_losses_pd = 15# Distribution Losses
  
  ##
  base_year = as.numeric(format(Sys.Date(), "%Y") )-1-1
  no_of_years = 10
  
  
  # load_factor_df$Domestic
}
