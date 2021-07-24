# # source('R/PMS.R')
# #
# # save
# # base_year = "2019-2020"
# # cws_growth_rate =  5
# # categories_growth_rate = data.frame( matrix( ncol = length(Categories), nrow=1))
# # colnames(categories_growth_rate) <- Categories
# # categories_growth_rate[1,]<- 30
# #
# # save(base_year, cws_growth_rate, categories_growth_rate, file = "config.RData")
# #
# # load("config.RData")
# #
# #
# # config_env<-new.env()
# #
# # save(categories_growth_rate, file = paste(Config_dir, "config_cws.RData", sep = ""))
# #
# # no_of_years=10
# #
# #
# # save(base_year, no_of_years, file = paste(Config_dir, "config_general.RData", sep = ""))
# # load(paste(Config_dir, "config_cws.RData", sep = ""))
# #
# #
# # config_env<-new.env()
# # load(paste(Config_dir, "config_general.RData", sep = ""), config_env)
# # load(paste(Config_dir, "config_cws.RData", sep = ""), config_env)
# #
# #
# # # categories_growth_rate<- categories_gr
# #
# # # load("C:/Users/isfar.RData", ex <- new.env())
# # ls.str(config_env)
# # config_env$base_year
# # config_env$no_of_years
# # config_env$categories_growth_rate
# #
# #
# #
# #
# #
# #
# #
# #
# #
# # Config_dir= "Config/"
# #
# # load(paste(Config_dir, "config.RData", sep = ""))
# #
# # config_env <- reactiveValues(
# #   name = "Ayman Abaid",
# #   age =22
# # )
# #
# # ls(config_env )
# #
# #
# #
# # ### get all
# # rv_list<-isolate(reactiveValuesToList(config_env))
# #
# #
# # isolate({
# #   vals$name=input$name
# #   vals$age=input$age
# # })
# # for ( k in 1: length(categories_var)){
# #   input_id = paste( categories_var[k], "_gr", sep="")
# #   print(input_id)
# # }
# 
config_env<- reactiveValues(
  base_year  = 1 ,
  domestic_gr  = 1 ,
  commercial_gr  = 1 ,
  public_light_gr = 1 ,
  small_industry_gr  = 1 ,
  medium_large_industry_gr  = 1 ,
  public_tube_well_gr = 1 ,
  bulk_gr  = 1

)
# #### Empty Current Env
# 



# 
# base_year = "2019-2020"
# no_of_years = 10
# domestic_gr = 5
# commercial_gr = 5
# public_light_gr=5
# small_industry_gr = 5
# medium_large_industry_gr =5
# public_tube_well_gr=5
# bulk_gr =5
# 
# # Config_dir= "Config/"
# 
# ####sav all variables in Gloabl envronment in .Rdata file
# save(list = ls(), file = paste("Config/config.RData", sep = ""), envir = environment())