# #
# #
# # load(paste("Config/", "config.RData", sep = ""))
# 
# 
# # Load Factor
# {
#   Disco_List <-
#     c("LESCO", "GEPCO", "FESCO","PESCO", "IESCO", "MEPCO", "TESCO", "QESCO")
# 
# 
#   categories_var <-
#     c(
#       "domestic",
#       "commercial",
#       "public_light",
#       "small_industry",
#       "medium_large_industry",
#       "public_tube_well",
#       "bulk"
#     ) #### MAke sure It is be same as Schema
# 
#   Categories <-
#     gsub("(^|[[:space:]])([[:alpha:]])",
#          "\\1\\U\\2",
#          gsub("_", " ", categories_var),
#          perl = TRUE)
# 
#   growth_rate_df <- data.frame( matrix( ncol= length(Categories), nrow = length(Disco_List) ))
# colnames(growth_rate_df) <- Categories
# rownames(growth_rate_df) <- Disco_List
# # growth_rate_df <- 5
# 
# growth_rate_df['LESCO','Domestic'] <- 6
# growth_rate_df['LESCO','Commercial'] <- 3.5
# growth_rate_df['LESCO','Public Light'] <- -4.3
# growth_rate_df['LESCO','Small Industry'] <- 2.4
# growth_rate_df['LESCO','Medium Large Industry'] <- 4.8
# growth_rate_df['LESCO','Public Tube Well'] <- 1.4
# growth_rate_df['LESCO','Bulk'] <- 0.4
# 
# growth_rate_df['GEPCO','Domestic'] <- 4.5
# growth_rate_df['GEPCO','Commercial'] <- 3.0
# growth_rate_df['GEPCO','Public Light'] <- -3.6
# growth_rate_df['GEPCO','Small Industry'] <- 3.0
# growth_rate_df['GEPCO','Medium Large Industry'] <- 5.6
# growth_rate_df['GEPCO','Public Tube Well'] <- 4.3
# growth_rate_df['GEPCO','Bulk'] <- 1.4
# 
# growth_rate_df['FESCO','Domestic'] <- 4.8
# growth_rate_df['FESCO','Commercial'] <- 5
# growth_rate_df['FESCO','Public Light'] <- -4
# growth_rate_df['FESCO','Small Industry'] <- 1.3
# growth_rate_df['FESCO','Medium Large Industry'] <- 5.5
# growth_rate_df['FESCO','Public Tube Well'] <- 1.2
# growth_rate_df['FESCO','Bulk'] <- 1.9
# 
# growth_rate_df['IESCO','Domestic'] <- 4.7
# growth_rate_df['IESCO','Commercial'] <- 3.7
# growth_rate_df['IESCO','Public Light'] <- -1.5
# growth_rate_df['IESCO','Small Industry'] <- 2.0
# growth_rate_df['IESCO','Medium Large Industry'] <- 4.0
# growth_rate_df['IESCO','Public Tube Well'] <- -2.8
# growth_rate_df['IESCO','Bulk'] <- 1.0
# 
# growth_rate_df['MEPCO','Domestic'] <- 5.9
# growth_rate_df['MEPCO','Commercial'] <- 4.2
# growth_rate_df['MEPCO','Public Light'] <- -2.3
# growth_rate_df['MEPCO','Small Industry'] <- 2.2
# growth_rate_df['MEPCO','Medium Large Industry'] <- 3.9
# growth_rate_df['MEPCO','Public Tube Well'] <- 1.5
# growth_rate_df['MEPCO','Bulk'] <- 2.2
# 
# growth_rate_df['PESCO','Domestic'] <- 3.9
# growth_rate_df['PESCO','Commercial'] <- 3.9
# growth_rate_df['PESCO','Public Light'] <- -1.6
# growth_rate_df['PESCO','Small Industry'] <- -1.2
# growth_rate_df['PESCO','Medium Large Industry'] <- 5.6
# growth_rate_df['PESCO','Public Tube Well'] <- -0.4
# growth_rate_df['PESCO','Bulk'] <- 1.3
# 
# growth_rate_df['HESCO','Domestic'] <- 3.06
# growth_rate_df['HESCO','Commercial'] <- 2.67
# growth_rate_df['HESCO','Public Light'] <- 0.33
# growth_rate_df['HESCO','Small Industry'] <- -5.34
# growth_rate_df['HESCO','Medium Large Industry'] <- 7.19
# growth_rate_df['HESCO','Public Tube Well'] <- -4.05
# growth_rate_df['HESCO','Bulk'] <- 1.14
# 
# growth_rate_df['QESCO','Domestic'] <- 2.3
# growth_rate_df['QESCO','Commercial'] <- 3.3
# growth_rate_df['QESCO','Public Light'] <- -2.2
# growth_rate_df['QESCO','Small Industry'] <- 0.6
# growth_rate_df['QESCO','Medium Large Industry'] <- 0.5
# growth_rate_df['QESCO','Public Tube Well'] <- -0.8
# growth_rate_df['QESCO','Bulk'] <- 2.8
# 
# growth_rate_df['TESCO','Domestic'] <- 0
# growth_rate_df['TESCO','Commercial'] <- 0
# growth_rate_df['TESCO','Public Light'] <- -0
# growth_rate_df['TESCO','Small Industry'] <- 1
# growth_rate_df['TESCO','Medium Large Industry'] <- -8
# growth_rate_df['TESCO','Public Tube Well'] <- -1
# growth_rate_df['TESCO','Bulk'] <- 0.01
# 
# colnames(growth_rate_df) <- categories_var
# 
# rm (Categories,categories_var,Disco_List)
# }
# # growth_rate_df
# # write.csv(growth_rate_df,"Config/growth_rate_df.csv")
# # base_year = 2019
# # no_of_years = 5
# # domestic_gr = 5
# # commercial_gr = 5
# # public_light_gr=5
# # small_industry_gr = 5
# # medium_large_industry_gr =5
# # public_tube_well_gr=5
# # bulk_gr =5
# # sind_cf = 56 # Small Industry Coincidence Factor in %
# # ptub_cf = 70 # Public Tube Well Coincidence Factor in %
# # sub_area1_cf = 85# Sub Area Type 1 Coincidence Factor in %
# # sub_area2_cf = 90# Sub Area Type 2  Coincidence Factor in %
# # sub_area3_cf = 95#Sub Area Type 3  Coincidence Factor in %
# # area_cf = 85 # Area  Coincidence Factor in %
# # trans_losses_pd = 1# Transmission Losses
# # dist_losses_pd = 15# Distribution Losses
# # disco_cf_pd = 90
# # ####sav all variables in Gloabl envronment in .Rdata file
# # save(list = ls(), file = paste("Config/config.RData", sep = ""), envir = environment())
# 
# # growth_rate_df <- t(growth_rate_df)
# #write.csv(growth_rate_df,"Config/growth_rate_df_autofill.csv")