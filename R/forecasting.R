# # # library(forecast)
# # # 
# historical_Data <-read.csv("C:\\Users\\Ayman\\Documents\\RAship\\Historical GEPCO.csv")
# historical_Data=historical_Data[complete.cases(historical_Data),]
# # historical_Data = historical_Data[,2]
# expected_prediction<-read.csv("C:\\Users\\Ayman\\Documents\\RAship\\Future GEPCO.csv")
# # # to_pred = historical_Data[1:length(expected_prediction),]
# # # to_pred[,2:length(colnames(to_pred))] <- NA
# # # to_pred$Year <- 2020:2027
# # # 
# # # ###-----------------------SES
# # loading the required packages
# plot()
# plot(historical_Data[,c(1,ncol(historical_Data))],  main="Historic Data",
#      xlab="year ", ylab="Total demand")
# 
# plot(expected_prediction[,c(1,ncol(expected_prediction))],  main="Future Data",
#      xlab="year ", ylab="Total demand")
# 
# 
