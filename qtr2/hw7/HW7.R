# Homework 7
# Hint



##-----Load Libraries-----
#install.packages("dplyr")
library(dplyr)
library(data.table)
library(pls)
library(glmnet)

##-----Load Data-----
load_headcount = function() {
  loginfo('Loading JitteredHeadCount.csv')
  headcount = read.csv('JitteredHeadCount.csv', stringsAsFactors = FALSE)
  return (headcount)
}
load_weather = function() {
  loginfo('Loading las_vegas_hourly_weather.csv')
  weather = read.csv('las_vegas_hourly_weather.csv', stringsAsFactors = FALSE)
  return (weather)
}


##-----Format Data----
formatData = function(headcount, weather) {
  loginfo('Formatting data...')
  headcount$DateFormat = as.Date(headcount$DateFormat, format="%m/%d/%Y")
  names(weather) = c('time','temp','dew_pt','humidity','pressure',
                     'visibility','wind_dir','wind_speed','gust_speed',
                     'precipitation','events','conditions',
                     'wind_dir_deg','date')
  
  weather$datetime = paste(weather$date,weather$time)
  weather$datetime = strptime(weather$datetime, format="%Y-%m-%d %I:%M %p")
  weather$Hour = as.numeric(format(round(weather$datetime, units="hours"), format="%H"))
  
  ##----Drop Duplicates----
  weather = weather[!duplicated(weather[c("date", 'Hour')]),]
  
  
  ##----Merge Data-----
  weather$DateFormat = weather$date
  weather$date = NULL
  weather$DateFormat = as.Date(weather$DateFormat, format="%Y-%m-%d")
  
  headcount = merge(headcount, weather, all.x=TRUE, by=c("DateFormat","Hour"))
  
  ##----Imputation for NAs in weather-----
  numeric_cols = c(11:15, 17:19, 22)
  # Linear Interpolation:
  headcount[,numeric_cols] = apply(headcount[,numeric_cols], 2, function(x) approx(x, xout=1:length(x), rule=2)$y)
  
  ##---Drop character columns----
  headcount$wind_dir = NULL
  headcount$time = NULL
  headcount$datetime = NULL
  
  ##-----Deal with events/conditions----
  headcount$events[headcount$events == ""] = "None"
  headcount$events[is.na(headcount$events)] = "None"
  headcount$conditions[is.na(headcount$conditions)] = "None"
  
  return (headcount)
  
}



##----Format Data for Time Series Exploration-----

plot_simple_exp_smoothing = function(headcount) {
  loginfo('Plotting SES')
  
  #ugly plot
  #plot(headcount$Date, headcount$HeadCount, type="l",
  #     main="Headcount Value Hourly", xlab="Date/Hour", ylab="Value")
  
  #aggregate each hour by day (a lot less noise)
  headCountAg <- aggregate(HeadCount ~ DateFormat, headcount, mean)
  plot(headCountAg$DateFormat, headCountAg$HeadCount, type="l",
       main="Headcount Value Daily", xlab="Date", ylab="Value")
  
  # use forecast's ses() function:
  exp_smooth1 = ses(headCountAg$HeadCount, alpha=0.05, h=31) # h is how many t-units to forecast out
  exp_smooth2 = ses(headCountAg$HeadCount, alpha=0.15, h=31)
  exp_smooth3 = ses(headCountAg$HeadCount, alpha=0.95, h=31)
  
  plot(exp_smooth1)
  lines(exp_smooth1$fitted, col="blue")
  lines(exp_smooth2$fitted, col="green")
  lines(exp_smooth3$fitted, col="red")
  legend('topleft', c('Original Data','alpha=0.05', 'alpha=0.15', 'alpha=0.95'),
         col=c('black','blue', 'green', 'red'), lty=c(1,1,1))
}

plot_ARIMA_model = function(headcount) {
  loginfo('Plotting ARIMA model')
  headCountAg <- aggregate(HeadCount ~ DateFormat, headcount, mean)
  #lets try simple exponential smoothing + seasonal integrated differences
  double_exp_smooth = Arima(headCountAg$HeadCount, order = c(0,1,1), seasonal=c(0,1,0))
  double_exp_fit = headCountAg$HeadCount - double_exp_smooth$residuals # fitted values
  plot(headCountAg$DateFormat, headCountAg$HeadCount,type="l", lwd=2)
  lines(headCountAg$DateFormat, double_exp_fit, col="red", lwd=2, lty=2)
  
  # prediction
  double_exp_pred = predict(double_exp_smooth, n.ahead = 30)
  loginfo(paste('Residual standard error=',double_exp_pred$se))
  lines(seq(from=headCountAg$DateFormat[366], to=headCountAg$DateFormat[366]+30, by=1)[-1],
        double_exp_pred$pred, lwd=2, col='green')
  # Add in standard error lines
  lines(seq(from=headCountAg$DateFormat[366], to=headCountAg$DateFormat[366]+30, by=1)[-1],
        double_exp_pred$pred + double_exp_pred$se, lwd=2, col='green')
  lines(seq(from=headCountAg$DateFormat[366], to=headCountAg$DateFormat[366]+30, by=1)[-1],
        double_exp_pred$pred - double_exp_pred$se, lwd=2, col='green')
}

plot_ARIMA_model_temp = function(headcount) {
  loginfo('Plotting ARIMA model temp')
  headCountAg <- aggregate(temp ~ DateFormat, headcount, mean)
  #lets try simple exponential smoothing + seasonal integrated differences
  double_exp_smooth = Arima(headCountAg$temp, order = c(0,1,1), seasonal=c(0,1,0))
  double_exp_fit = headCountAg$temp - double_exp_smooth$residuals # fitted values
  plot(headCountAg$DateFormat, headCountAg$temp,type="l", lwd=1)
  lines(headCountAg$DateFormat, double_exp_fit, col="red", lwd=2, lty=1)
  
  # prediction
  double_exp_pred = predict(double_exp_smooth, n.ahead = 30)
  loginfo(paste('Residual standard error=',double_exp_pred$se))
  lines(seq(from=headCountAg$DateFormat[366], to=headCountAg$DateFormat[366]+30, by=1)[-1],
        double_exp_pred$pred, lwd=2, col='green')
  # Add in standard error lines
  lines(seq(from=headCountAg$DateFormat[366], to=headCountAg$DateFormat[366]+30, by=1)[-1],
        double_exp_pred$pred + double_exp_pred$se, lwd=2, col='green')
  lines(seq(from=headCountAg$DateFormat[366], to=headCountAg$DateFormat[366]+30, by=1)[-1],
        double_exp_pred$pred - double_exp_pred$se, lwd=2, col='green')
}
find_best_lambda = function(headcount) {
  headCountAg <- aggregate(HeadCount ~ DateFormat, headcount, mean)
  ##-----Lasso Regression-----
  predictor = log(headcount$HeadCount)
  predictor = replace(predictor, predictor=="-Inf", 0)
  xfactors = model.matrix(log(HeadCount) ~ ., data = headcount)[,-1]
  
  log_headcount_lasso =glmnet(xfactors,predictor,alpha=1,family='gaussian')
  
  plot(log_headcount_lasso, xvar="lambda")
  
  #log_headcount_lasso # See all the possible choices
  
  coef(log_headcount_lasso)[,20][coef(log_headcount_lasso)[,20]>1e-10]
  
  log_headcount_lasso_cv = cv.glmnet(xfactors,predictor,alpha=1,family='gaussian')
  plot(log_headcount_lasso_cv)
  best_lambda = log_headcount_lasso_cv$lambda.min
  
  best_coef = coef(log_headcount_lasso)[,log_headcount_lasso$lambda == best_lambda]
  best_coef = best_coef[best_coef > 1e-10]
  return (best_lambda)
  
}


if(interactive()) {
  basicConfig(level='DEBUG')
  setwd('C:/Users/kevin/Google Drive/datasci/HW7')
  addHandler(writeToFile, file="HW7.log",level='FINEST') 
  loginfo('Starting Homework 7 run.')
  raw_headcount = load_headcount()
  raw_weather = load_weather()
  headcount = formatData(raw_headcount,raw_weather)
  best_lambda = find_best_lambda(headcount)
  loginfo(paste('Best lambda=',best_lambda))
  plot_simple_exp_smoothing(headcount)
  plot_ARIMA_model(headcount)
  plot_ARIMA_model_temp(headcount)
  loginfo('Ending HW7 run.')
}

##Explanation
# I used Lasso regression to find the best lambda value. 
# I then created a model using simple exponential smoothing to forecast 31 days with different levels of confidence.
# Next I created an ARIMA model using exponential smoothing + seasonal integrated differences. 
# The resulting forecast had a Residual Standard Error from 1-10 over 30 days.
# Finally I created an ARIMA model based on temperature as the predictor. The plots are output.
# This time the resulting Residual standard error went from 4-26. Both of these errors indicate a large margin of possible headcounts,