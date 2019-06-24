#BatchGetSymbols downloads financial data. 
library(BatchGetSymbols)
library(esquisse)
library(forecast)
library(tseries)
library(MASS)
library(lubridate)
library(Metrics)
library(quantmod)
library(ggplot2)

#this gives you 1 year of data, use Sys.Date()-3653 for 10 years of data (3650 + 2 or 3 leap years)
#you can set first.date & last.date to anything you want
#if you choose specific dates enter them in quotes in the following format: "YYYY-MM-DD"
#set dates
first.date <- Sys.Date()-3653
last.date <- as.Date('2019-04-12')
freq.data <- 'weekly'

#set tickers
#SPGI <-S&P Global, Inc.,NDAQ<-Nasdaq, Inc., T<-AT&T, CSCO<- cisco systems
#AAPL<-Apple, GOOGL<-Alphabet Inc., MSFT<-Microsoft
#WMT<- Walmart, DIS<-The Walt Disney Company, AMZN<-Amazon.com Inc.
tickers <- c('AAPL','GOOGL','WMT','DIS','AMZN')

#download the data: if you have a lot this can take time
l.out <- BatchGetSymbols(tickers = tickers,
                         first.date = first.date,
                         last.date = last.date,
                         freq.data = freq.data)


#l.out is a list of 2 data frames
#the prices are in the second one so extract it into a separate data frame to use
stockdata <- l.out[[2]]

#Plotting stock data vs Date
ggplot(data = stockdata) +
  aes(x = ref.date, y = price.close, color = ticker) +
  geom_line() +
  labs(title = 'Closing Price vs Date',
       x = 'Date',
       y = 'Closing Price') +
  theme_minimal()

#Plotting stock data vs Date (excluding Amazon and Google)
esquisser(stockdata)


#---------------------------Apple Sock------------------------------------------------------------------
#Subsettung the stock data based on tickers
apple_data <- subset(stockdata, ticker=="AAPL", select = c(ref.date,price.close))
apple_price <- apple_data$price.close
apple_ts <- ts(apple_price, start = decimal_date(ymd("2009-04-13")), frequency = 365.25/7)
#Plotting the data sets
plot(apple_ts, main = "Apple Stock Closing price vs Time", ylab = "Closing price" )

#convert to ln format
apple_lnprice <- log(apple_price)

#Moving average on ln of stock price
apple_difflnprice <- diff(apple_lnprice,1)

#Dickey-Fuller Test
adf.test(apple_price)
adf.test(apple_lnprice)
adf.test(apple_difflnprice)

#ACF, PACF 
acf(apple_lnprice, lag.max=50, main="ACF plot of Apple stock")
pacf(apple_lnprice, lag.max=50, main="PACF plot of Apple stock")


#breaking the dataset into training and testing
breakpoint <- floor(dim(apple_data)[1]*0.8)

#Divide into training and test data
apple_train <- apple_lnprice[1:breakpoint]
apple_test <- apple_lnprice[breakpoint:dim(apple_data)[1]]


#Time series and auto.arima on ln price
apple_pricearima <- ts(apple_train, start = decimal_date(ymd("2009-04-13")), frequency = 365.25/7)
apple_fit <- auto.arima(apple_pricearima, allowdrift = TRUE, trace = TRUE, test = "adf", ic = "bic")
summary(apple_fit)

#Forecasted values from ARIMA
apple_forecastedvalues <- forecast(apple_fit, h=106)
plot(apple_forecastedvalues, include= 100, xlab = "Time",ylab = "log(closing price)")

#Converting the log values to exponential
apple_value <- as.numeric(apple_forecastedvalues$mean)
apple_predictedval <- exp(apple_value)
apple_actual_value <- exp(apple_test)

#calculating the error between predicted and test data
apple_rmse <- rmse(apple_actual_value,apple_predictedval)
apple_error <- mean(abs((apple_actual_value - apple_predictedval)/apple_actual_value))

#---------------------------------------------Google Data------------------------------------------
#Subsettung the stock data based on tickers
google_data <- subset(stockdata, ticker=="GOOGL", select = c(ref.date,price.close))
google_price <- google_data$price.close
google_ts <- ts(google_price, start = decimal_date(ymd("2009-04-13")), frequency = 365.25/7)
#Plotting the data sets
plot(google_ts, main = "Google Stock Closing price vs Time", ylab = "Closing price" )

#convert to ln format
google_lnprice <- log(google_price)

#Moving average on ln of stock price
google_difflnprice <- diff(google_lnprice,1)

#Dickey-Fuller Test
adf.test(google_price)
adf.test(google_lnprice)
adf.test(google_difflnprice)

#ACF, PACF 
acf(google_lnprice, lag.max=50, main="ACF plot of Google stock")
pacf(google_lnprice, lag.max=50, main="PACF plot of Google stock")


#breaking the dataset into training and testing
breakpoint <- floor(dim(google_data)[1]*0.8)

#Divide into training and test data
google_train <- google_lnprice[1:breakpoint]
google_test <- google_lnprice[breakpoint:dim(google_data)[1]]


#Time series and auto.arima on ln price
google_pricearima <- ts(google_train, start = decimal_date(ymd("2009-04-13")), frequency = 365.25/7)
google_fit <- auto.arima(google_pricearima, allowdrift = TRUE, test = "adf", trace = TRUE, ic = "bic")
summary(google_fit)

#Forecasted values from ARIMA
google_forecastedvalues <- forecast(google_fit, h=106)
plot(google_forecastedvalues, include= 100, xlab = "Time",ylab = "log(closing price)")

#Converting the log values to exponential
google_value <- as.numeric(google_forecastedvalues$mean)
google_predictedval <- exp(google_value)
google_actual_value <- exp(google_test)


#calculating the error between predicted and test data
google_error <- mean(abs((google_actual_value - google_predictedval)/google_actual_value))
google_rmse <- rmse(google_actual_value,google_predictedval)

#----------------------------------------------Walmart Data----------------------------------------
#Subsetting the stock data based on tickers
walmart_data <- subset(stockdata, ticker=="WMT", select = c(ref.date,price.close))
wmt_price <- walmart_data$price.close
wmt_ts <- ts(wmt_price, start = decimal_date(ymd("2009-04-13")), frequency = 365.25/7)
#Plotting the data sets
plot(wmt_ts, main = "Walmart Stock Closing price vs Time", ylab = "Closing price" )

#convert to ln format
wmt_lnprice <- log(wmt_price)

#Moving average on ln of stock price
wmt_difflnprice <- diff(wmt_lnprice,1)

#Dickey-Fuller Test
adf.test(wmt_price)
adf.test(wmt_lnprice)
adf.test(wmt_difflnprice)

#ACF, PACF 
acf(wmt_lnprice, lag.max=50, main="ACF plot of Walmart stock")
pacf(wmt_lnprice, lag.max=50, main="PACF plot of Walmart stock")


#breaking the dataset into training and testing
breakpoint <- floor(dim(walmart_data)[1]*0.8)

#Divide into training and test data
wmt_train <- wmt_lnprice[1:breakpoint]
wmt_test <- wmt_lnprice[breakpoint:dim(walmart_data)[1]]


#Time series and auto.arima on ln price
wmt_pricearima <- ts(wmt_train, start = decimal_date(ymd("2009-04-13")), frequency = 365.25/7)
wmt_fit <- auto.arima(wmt_pricearima, allowdrift = TRUE, trace = TRUE, test = "adf", ic = "bic")
summary(wmt_fit)

#Forecasted values from ARIMA
wmt_forecastedvalues <- forecast(wmt_fit, h=106)
plot(wmt_forecastedvalues, include= 100, xlab = "Time",ylab = "log(closing price)")

#Converting the log values to exponential
wmt_value <- as.numeric(wmt_forecastedvalues$mean)
wmt_predictedval <- exp(wmt_value)
wmt_actual_value <- exp(wmt_test)

#calculating the error between predicted and test data
wmt_error <- mean(abs((wmt_actual_value - wmt_predictedval)/wmt_actual_value))
wmt_rmse <- rmse(wmt_actual_value,wmt_predictedval)

#---------------------------------------------Disney data------------------------------------------
#Subsetting the stock data based on tickers
disney_data <- subset(stockdata, ticker=="DIS", select = c(ref.date,price.close))
dis_price <- disney_data$price.close
dis_ts <- ts(dis_price, start = decimal_date(ymd("2009-04-13")), frequency = 365.25/7)
#Plotting the data sets
plot(dis_ts, main = "Disney Stock Closing price vs Time", ylab = "Closing price" )

#convert to ln format
dis_lnprice <- log(dis_price)

#Moving average on ln of stock price
dis_difflnprice <- diff(dis_lnprice,1)

#Dickey-Fuller Test
adf.test(dis_price)
adf.test(dis_lnprice)
adf.test(dis_difflnprice)

#ACF, PACF 
acf(dis_lnprice, lag.max=50, main="ACF plot of Disney stock")
pacf(dis_lnprice, lag.max=50, main="PACF plot of Disney stock")


#breaking the dataset into training and testing
breakpoint <- floor(dim(disney_data)[1]*0.8)

#Divide into training and test data
dis_train <- dis_lnprice[1:breakpoint]
dis_test <- dis_lnprice[breakpoint:dim(disney_data)[1]]


#Time series and auto.arima on ln price
dis_pricearima <- ts(dis_train, start = decimal_date(ymd("2009-04-13")), frequency = 365.25/7)
dis_fit <- auto.arima(dis_pricearima, allowdrift = TRUE, trace = TRUE, test = "adf", ic = "bic")
summary(dis_fit)

#Forecasted values from ARIMA
dis_forecastedvalues <- forecast(dis_fit, h=106)
plot(dis_forecastedvalues, include= 100, xlab = "Time",ylab = "log(closing price)")

#Converting the log values to exponential
dis_value <- as.numeric(dis_forecastedvalues$mean)
dis_predictedval <- exp(dis_value)
dis_actual_value <- exp(dis_test)

#calculating the error between predicted and test data
dis_error <- mean(abs((dis_actual_value - dis_predictedval)/dis_actual_value))
dis_rmse <- rmse(dis_actual_value,dis_predictedval)


#--------------------------------------------Amazon data-------------------------------------------
#Subsetting the stock data based on tickers
amazon_data <- subset(stockdata, ticker=="AMZN", select = c(ref.date,price.close))
amz_price <- amazon_data$price.close
amz_ts <- ts(amz_price, start = decimal_date(ymd("2009-04-13")), frequency = 365.25/7)
#Plotting the data sets
plot(amz_ts, main = "Amazon Stock Closing price vs Time", ylab = "Closing price" )

#convert to ln format
amz_lnprice <- log(amz_price)

#Moving average on ln of stock price
amz_difflnprice <- diff(amz_lnprice,1)

#Dickey-Fuller Test
adf.test(amz_price)
adf.test(amz_lnprice)
adf.test(amz_difflnprice)

#ACF, PACF 
acf(amz_lnprice, lag.max=50, main="ACF plot of Amazon stock")
pacf(amz_lnprice, lag.max=50, main="PACF plot of Amazon stock")


#breaking the dataset into training and testing
breakpoint <- floor(dim(amazon_data)[1]*0.8)

#Divide into training and test data
amz_train <- amz_lnprice[1:breakpoint]
amz_test <- amz_lnprice[breakpoint:dim(amazon_data)[1]]


#Time series and auto.arima on ln price
amz_pricearima <- ts(amz_train, start = decimal_date(ymd("2009-04-13")), frequency = 365.25/7)
amz_fit <- auto.arima(amz_pricearima, allowdrift = TRUE, trace = TRUE, test = "adf", ic = "bic")
summary(amz_fit)

#Forecasted values from ARIMA
amz_forecastedvalues <- forecast(amz_fit, h=106)
plot(amz_forecastedvalues, include= 100, xlab = "Time",ylab = "log(closing price)")

#Converting the log values to exponential
amz_value <- as.numeric(amz_forecastedvalues$mean)
amz_predictedval <- exp(amz_value)
amz_actual_value <- exp(amz_test)

#calculating the error between predicted and test data
amz_error <- mean(abs((amz_actual_value - amz_predictedval)/amz_actual_value))
amz_rmse <- rmse(amz_actual_value,amz_predictedval)




