# library(forecast)
# library(ggplot2)
# library(dplyr)
# library(broom)
# library(ggpubr)
# categories_list <-
#   c(
#     "domestic",
#     "commercial",
#     "public_light",
#     "small_industry",
#     "medium_large_industry",
#     "public_tube_well") 
# # # Data Preperation ---------------------------------------------------------------
# historical_Data <-read.csv("/Users/apple/Documents/EIG/PMS-Data/Historical GEPCO.csv")
# historical_Data=historical_Data[complete.cases(historical_Data),]
# historical_Data$Covid <- 0 
# historical_Data[ (GEPCO_data$year>=2020),]$Covid = 1
# 
# train_Data <- historical_Data[1:10,]
# test_Data <- historical_Data[10:nrow(historical_Data),]
# 
# # Predicted DF
# Predicted = test_Data[0,]
# Predicted = as.data.frame(matrix(ncol =ncol(test_Data), nrow=nrow(test_Data)))
# colnames(Predicted)<- colnames(test_Data)
# 
# 
# 
# # # Linear Regresion ---------------------------------------------------------------
# historical_Data <-read.csv("/Users/apple/Documents/EIG/PMS-Data/Historical GEPCO.csv")
# historical_Data=historical_Data[complete.cases(historical_Data),]
# train_Data <- historical_Data[1:10,]
# test_Data <- historical_Data[10:nrow(historical_Data),]
# 
# # Predicted DF
# Predicted = test_Data[0,]
# Predicted = as.data.frame(matrix(ncol =ncol(test_Data), nrow=nrow(test_Data)))
# colnames(Predicted)<- colnames(test_Data)
# ## forecast using linear regression for each category
# for ( i in 1:length(categories_list))
# {
#   curr_train_df = as.data.frame(matrix(ncol =2, nrow=nrow(train_Data)))
#   colnames(curr_train_df)<- c("X","Y")
#   curr_train_df$X <-train_Data$year
#   curr_train_df$Y <- train_Data[[categories_list[i]]]
#   x_y.lm <- lm(Y ~ X, data = curr_train_df) # train model
#   
#   curr_test_df = as.data.frame(matrix(ncol =2, nrow=nrow(test_Data)))
#   colnames(curr_test_df)<- c("X","Y")
#   curr_test_df$X <-test_Data$year
#   curr_test_df$Y <- test_Data[[categories_list[i]]]
#   Predicted[categories_list[i]] <- predict(x_y.lm, curr_test_df)  # predict 
#   
# }
# 
# Predicted$year = test_Data$year
# Predicted$total = colSums(Predicted[,categories_list])
# actuals_preds <- data.frame(cbind(actuals=test_Data$Domestic, predicteds=Pred))
# accuracy( Predicted$total,test_Data$total)
# 
# # 
# # # comarison with PMS forecast
# future <- read.csv("/Users/apple/Documents/EIG/PMS-Data/Future GEPCO.csv")
# # expected_prediction<-read.csv("/Users/apple/Documents/EIG/PMS-Data/Future GEPCO.csv")
# # x_y.lm <- lm(Domestic ~ Year, data = historical_Data[,1:2])
# # Pred <- predict(x_y.lm, expected_prediction[,1:2])  # predict 
# # actuals_preds <- data.frame(cbind(actuals=expected_prediction$Domestic, predicteds=Pred))
# # accuracy( actuals_preds$predicteds,actuals_preds$actuals)
# # 
# # 
# # 
# # # Plots ---------------------------------------------------------------
# 
# # # Actual vs Linear-regression Forecast
# actuals_preds <- data.frame(cbind(actuals=test_Data$total, predicteds=Predicted$total))
# dat <- matrix(runif(40,1,20),ncol=4) # make data
# matplot(actuals_preds, type = c("b"),pch=1,col = 1:2) #plot
# legend("topleft", legend = c("LR","actual"), col=1:2, pch=1) # optional legend
# 
# 
# 
# # # Multiple Regression ---------------------------------------------------------------
# 
# historical_Data <-read.csv("/Users/apple/Documents/EIG/PMS-Data/Historical GEPCO.csv")
# historical_Data=historical_Data[complete.cases(historical_Data),]
# historical_Data$Covid <- 0 
# historical_Data[ (GEPCO_data$year>=2020),]$Covid = 1
# 
# train_Data <- historical_Data[1:10,]
# test_Data <- historical_Data[10:nrow(historical_Data),]
# 
# # Predicted DF
# Predicted = test_Data[0,]
# Predicted = as.data.frame(matrix(ncol =ncol(test_Data), nrow=nrow(test_Data)))
# colnames(Predicted)<- colnames(test_Data)
# 
# 
# 
# # # Linear Regresion ---------------------------------------------------------------
# historical_Data <-read.csv("/Users/apple/Documents/EIG/PMS-Data/Historical GEPCO.csv")
# historical_Data=historical_Data[complete.cases(historical_Data),]
# future_Data<- read.csv("/Users/apple/Documents/EIG/PMS-Data/Future GEPCO.csv")
# GEPCO_data <- rbind(historical_Data,future_Data)
# # Adding Covid Feature ----------------------------------------------------
# GEPCO_data$Covid <- 0 
# GEPCO_data[ (GEPCO_data$year>=2020),]$Covid = 1
# 
# train_Data <- GEPCO_data[1:15,]
# test_Data <- GEPCO_data[15:nrow(GEPCO_data),]
# 
# # Predicted DF
# Predicted = test_Data[0,]
# Predicted = as.data.frame(matrix(ncol =ncol(test_Data), nrow=nrow(test_Data)))
# colnames(Predicted)<- colnames(test_Data)
# ## forecast using linear regression for each category
# for ( i in 1:length(categories_list))
# {
#   curr_train_df = as.data.frame(matrix(ncol =3, nrow=nrow(train_Data)))
#   colnames(curr_train_df)<- c("X","X2","Y")
#   curr_train_df$X <-train_Data$year
#   curr_train_df$Y <- train_Data[[categories_list[i]]]
#   curr_train_df$X2 <- train_Data$Covid
#   
#   x_y.lm <- lm(Y ~ (X + X2), data = curr_train_df) # train model
#   
#   curr_test_df = as.data.frame(matrix(ncol =3, nrow=nrow(test_Data)))
#   colnames(curr_test_df)<- c("X","X2","Y")
#   curr_test_df$X <-test_Data$year
#   curr_test_df$Y <- test_Data[[categories_list[i]]]
#   curr_test_df$X2 <- test_Data$Covid
#   
#   Predicted[categories_list[i]] <- predict(x_y.lm, curr_test_df)  # predict 
#   
# }
# 
# Predicted$year = test_Data$year
# Predicted$total = rowSums(Predicted[,categories_list])
# accuracy( Predicted$total,test_Data$total)
# 
# actuals_preds <- data.frame(cbind(actuals=test_Data$total, predicteds=Predicted$total))
# dat <- matrix(runif(40,1,20),ncol=4) # make data
# matplot(actuals_preds, type = c("b"),pch=1,col = 1:2) #plot
# # legend("topleft", legend = c("LR","PMS"), col=1:2, pch=1) # optional legend