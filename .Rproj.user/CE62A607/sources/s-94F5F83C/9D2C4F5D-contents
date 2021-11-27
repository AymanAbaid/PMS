# # # {Planned_Load_Data_cols <-
# # #   c(
# # #     "area_code",
# # #     "power_market",
# # #     "load_center",
# # #     "transient_grid_flag",
# # #     "grid_no",
# # #     "proposed_grid_no",
# # #     "province",
# # #     "division",
# # #     "district",
# # #     "area",
# # #     "sub_area",
# # #     "area_type",
# # #     "year",
# # #     "consumer_type",
# # #     "name_of_area",
# # #     "name_of_sub_area",
# # #     "domestic",
# # #     "commercial",
# # #     "public_light",
# # #     "small_industry",
# # #     "public_tube_well",
# # #     "bulk",
# # #     "bulk_demand",
# # #     "medium_large_industry",
# # #     "medium_large_industry_demand",
# # #     "domestic_load_factor",
# # #     "commercial_load_factor",
# # #     "small_industry_load_factor",
# # #     "public_tube_well_load_factor",
# # #     "small_industry_coincidence_factor",
# # #     "public_tube_well_coincidence_factor"
# # #
# # #   )
# # # }
# # # # # read_xlsx()
# # # #
# # # # c04<-IESCO
# # # # C05 MEPCO
# # # # C06 PESCO
# # # # C08 SEPCO
# # #
# # # # C09 TESCO
# xls_file <- readxl::read_xls("C:/Users/Ayman/Documents/PMS-Data/PMS 2019-20/Input Files/INPY1920.C09.xls")
# xls_file <- replace(xls_file, is.na(xls_file), 0)
# # # #
# colnames(xls_file)<- Planned_Load_Data_cols
# xls_file$bulk_demand <- round(as.numeric(xls_file$bulk_demand),0)
# xls_file$bulk <- round(as.numeric(xls_file$bulk),0)
# xls_file$medium_large_industry_demand<- round(as.numeric(xls_file$medium_large_industry_demand),0)
# xls_file$small_industry_coincidence_factor<- round(as.numeric(xls_file$small_industry_coincidence_factor),0)
# xls_file$public_tube_well_coincidence_factor<- round(as.numeric(xls_file$public_tube_well_coincidence_factor),0)
# # write.csv(xls_file,"Data/PESCO-2019-Planned Load Data.csv", row.names = FALSE)
# write.csv(xls_file,"Data/t/TESCO.csv", row.names = FALSE)
