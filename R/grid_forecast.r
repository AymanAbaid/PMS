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
# # 
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
# # # hist(historical_Data$Domestic)
# # # plot(historical_Data$Domestic)
# # 
# #dependent variable
# y <-train_Data$Domestic
# #indepenedent variable
# x <-train_Data$Year
# # 
# # x_y.lm <- lm(Domestic ~ Year, data = train_Data)
# # 
# # summary(x_y.lm)
# # Pred <- predict(x_y.lm, test_Data)  # predict 
# # actuals_preds <- data.frame(cbind(actuals=test_Data$Domestic, predicteds=Pred))
# # accuracy( actuals_preds$predicteds,actuals_preds$actuals)
# # 
# # # comarison with PMS forecast
# # expected_prediction<-read.csv("/Users/apple/Documents/EIG/PMS-Data/Future GEPCO.csv")
# # x_y.lm <- lm(Domestic ~ Year, data = historical_Data[,1:2])
# # Pred <- predict(x_y.lm, expected_prediction[,1:2])  # predict 
# # actuals_preds <- data.frame(cbind(actuals=expected_prediction$Domestic, predicteds=Pred))
# # accuracy( actuals_preds$predicteds,actuals_preds$actuals)
# # 
# # # Multiple Regression ---------------------------------------------------------------
# # 
# # 
# # # Plots ---------------------------------------------------------------
# # 
# # plot(historical_Data[,c(1,ncol(historical_Data))],  main="Historic Data",
# #      xlab="year ", ylab="Total demand")
# # lines(tree$age, tree$circumference, type="b", lwd=1.5,
# #       lty=linetype[i], col=colors[i], pch=plotchar[i])
# # # 
# # plot(expected_prediction[,c(1,ncol(expected_prediction))],  main="Future Data",
# #      xlab="year ", ylab="Total demand")
# # 
# # 
# # #------------------------------------------------------------
# # 
# # # convert factor to numeric for convenience
# # Orange$Tree <- as.numeric(Orange$Tree)
# # ntrees <- max(Orange$Tree)
# # 
# # # get the range for the x and y axis
# # xrange <- range(Orange$age)
# # yrange <- range(Orange$circumference)
# # 
# # # set up the plot
# # plot(xrange, yrange, type="n", xlab="Age (days)",
# #      ylab="Circumference (mm)" )
# # lines(expected_prediction$Year, expected_prediction$Total, type="b", lwd=1.5,
# #       lty=linetype[i], col=colors[i], pch=plotchar[i])
# # lines(expected_prediction$Year, expected_prediction$Total, type="b", lwd=1.5,
# #       lty=linetype[i], col=colors[i], pch=plotchar[i])
# # 
# # colors <- rainbow(ntrees)
# # linetype <- c(1:ntrees)
# # plotchar <- seq(18,18+ntrees,1)
# # 
# # # add lines
# # for (i in 1:ntrees) {
# #   tree <- subset(Orange, Tree==i)
# #   lines(expected_prediction$Year, expected_prediction$Total, type="b", lwd=1.5,
# #         lty=linetype[i], col=colors[i], pch=plotchar[i])
# # }
# # 
# # # add a title and subtitle
# # title("Tree Growth", "example of line plot")
# # 
# # # add a legend
# # legend(xrange[1], yrange[2], 1:ntrees, cex=0.8, col=colors,
# #        pch=plotchar, lty=linetype, title="Tree")