# This data is for monthly anti-diabetic drug sales in Australia from 1992 to 2008. 
# Total monthly scripts for pharmaceutical products falling under ATC code A10, 
# as recorded by the Australian Health Insurance Commission. 
# 
# Please build a good model to predict the drug sales
# 
# It is due on 3 April.

library(dplyr)
library(lubridate) # for converting data to year month day
# library(caTools)
library(astsa)
library(forecast)
library(ggplot2)
# library(tseries)

source('assignment2/helpers.R')

SHOWGRAPHS = TRUE
SHOWGRAPHS = FALSE

### Examine data
df_data = read.delim("./data/drug.txt", header=TRUE, sep = ",")
head(df_data)

global.title <<- "Monthly anti-diabetic drug sales"
global.xlab <<- "Year"
global.ylab <<- "Drug Sales"
global.freq <<- 12
df_data <- df_data %>% mutate(date = ymd(date))
global.start <<- ymd(as.Date(df_data$date[[1]]))
global.end <<- ymd(as.Date(df_data$date[[nrow(df_data)]]))
### Convert to time series data
ts_all <- ts(df_data$value, start=c(year(global.start), month(global.start)),
             end=c(year(global.end), month(global.end)), frequency=global.freq)
plot.ts <- plotts(ts_all)
if (SHOWGRAPHS == TRUE)
  plot.ts
acf(ts_all, plot=SHOWGRAPHS)
length(ts_all)

library(TSA)
periodogram(df_data$value)

### Split dataset to train test set
ts_all
num_of_train <- round(0.7 * length(ts_all))
# ts_train <- window(ts_all, start=c(1992, 1), end=c(2004, 12))
# ts_val <- window(ts_all, start=c(2005, 1), end=c(2008, 6))
ts_train <- head(ts_all, num_of_train)
ts_val <- tail(ts_all, round(length(ts_all) - num_of_train))
ts_train

### Find suitable SARIMA model

nyear = 5
plot.ts
ts_all_d = diff(ts_all, differences = 1)
length(ts_all_d)
plotts(ts_all_d, "blue")
ts_all_d_D = diff(ts_all, differences = 1, lag=12)
length(ts_all_d_D)
plotts(ts_all_d_D, "blue")
acf(coredata(ts_all_d_D))

# acf(df_data_d1, lag.max=12*5)
ggAcf(coredata(ts_all_d_D), lag.max=12*nyear)
ggPacf(coredata(ts_all_d_D), lag.max=12*nyear)

# to find optimal lambda
lambda = BoxCox.lambda(ts_train) %>% print()
# now to transform vector
ts_train_log = BoxCox(ts_train,lambda) #should use BoxCoxs to reduce seasonality
# ts_train_log = log(ts_train)

plotts(ts_train,"blue")
plotts(ts_train_log, "blue")

ts_train_bc_d = diff(ts_train_log, differences = 1)
plotts(ts_train_bc_d, "blue")
length(ts_train_bc_d)
ggAcf(coredata(ts_train_bc_d), lag.max=12*nyear) # does not cut off
ggPacf(coredata(ts_train_bc_d), lag.max=12*nyear) # cut off after lag 14
ts_train_bc_dD = diff(ts_train_log, differences = 1, lag=12)
plotts(ts_train_bc_dD, "blue")
length(ts_train_bc_dD)
acf(coredata(ts_train_bc_dD), lag.max=12*nyear) # P = 2, p = 3
pacf(coredata(ts_train_bc_dD), lag.max=12*nyear) # cut off after lag 12 => Q = 1, q=3
ts_train_log_dD = diff(ts_train_log, differences = 1, lag=12)
plotts(ts_train_log_dD, "blue")
length(ts_train_log_dD)
ggAcf(coredata(ts_train_log_dD), lag.max=12*nyear) # P = 2, p = 3
ggPacf(coredata(ts_train_log_dD), lag.max=12*nyear) # cut off after lag 12 => Q = 1, q=3

# acf cut off p = 6, P = 
# pacf cut off after lag 36, Q = 2

fit.arima1 = arima(log(ts_train), 
                   order=c(3,1,3), 
                   seasonal=list(order=c(2,1,1),period=12))
checkresiduals(fit.arima1)
tsdiag(fit.arima1)
summary(fit.arima1)

fit.arima2 = arima(ts_train_log, 
                   order=c(0,1,3), 
                   seasonal=list(order=c(0,1,1),period=12))
checkresiduals(fit.arima2)
tsdiag(fit.arima2)
summary(fit.arima2)

fit.autoarimabc= arima(ts_train_log, 
                        order=c(0,1,1), 
                        seasonal=list(order=c(0,1,2),period=12))
fit.autoarimabc
checkresiduals(fit.autoarimabc)
tsdiag(fit.autoarimabc)
summary(fit.autoarimabc)

pred.arima1 <- exp(forecast(fit.arima1, h = length(ts_val)+20)$mean)
pred.arima2 <- exp(forecast(fit.arima2, h = length(ts_val)+20)$mean)
pred.autoarimabc <- exp(forecast(fit.autoarimabc, h = length(ts_val)+20)$mean)

global.pred.start <<- start(pred.arima1) %>% print()
global.pred.end <<- end(pred.arima1) %>% print()

plot.arimas <- plotforecast(ts_all, pred.arima1, "ARIMA1", pred.arima2, "ARIMA2", pred.autoarimabc, "AUTOARIMABC")
# if (SHOWGRAPHS == TRUE)
  plot.arimas

accuracy(pred.arima1, ts_all)
accuracy(pred.arima2, ts_all)
accuracy(pred.autoarimabc, ts_all)

# ## Transform the lags from years to months
# acfpl$lag <- acfpl$lag * 12
# Stationary test
# adf.test(ts_all) # does not work well for seasonal data, should not be your first choice.

### Seasonal decomposition using stl()
# fit <- stl(ts_all, s.window="period")
# plot(fit)
# 
# monthplot(ts_all)
# seasonplot(ts_all)

### Predict using Holt-Winters' Trend and Seasonality Model
log_ts_train = log(ts_train)
fit.hw.add <- hw(log_ts_train, seasonal = "additive")
fit.hw.mul <- hw(log_ts_train, seasonal = "multiplicative")

pred.hw.add <- exp(forecast(fit.hw.add$mean, h=length(ts_val))$mean)
pred.hw.mul <- exp(forecast(fit.hw.mul$mean, h=length(ts_val))$mean)
global.pred.start <<- start(pred.hw.add) %>% print()
global.pred.end <<- end(pred.hw.add) %>% print()
# global.pred.start <<- end(ts_train)
# global.pred.end <<- end(ts_val)

acc.hw.add <- accuracy(pred.hw.add, ts_all) %>% print()
acc.hw.mul <- accuracy(pred.hw.mul, ts_all) %>% print()

plot.hw <- plotforecast(ts_all, pred.hw.add, "HW ADD", pred.hw.mul, "HW MUL")
if (SHOWGRAPHS == TRUE)
  plot.hw
# grid.arrange(plot.hw, ncol = 1)

### Predict using Seasonal ARIMA Model

# fit.auto.arima <- auto.arima(ts_train, max.p=5, max.q=5,
#                              max.P=5, max.Q=5, max.d=5, max.D=5, allowdrift = TRUE,
#                              stepwise=FALSE, approximation=FALSE, parallel = TRUE)
# 
# fit.auto.arima.boxcox <- auto.arima(ts_train, max.p=5, max.q=5,
#                                     max.P=5, max.Q=5, max.d=5, max.D=5, allowdrift = TRUE,
#                                     stepwise=FALSE, approximation=FALSE, lambda=lambda, parallel = TRUE)
# 
# fit.auto.arima
# fit.auto.arima.boxcox   
# 
# fit.auto.arima.boxcox <- auto.arima(ts_train, max.p=2, max.q=2,
#                                     max.P=2, max.Q=2, max.d=2, max.D=2, allowdrift = TRUE,
#                                     stepwise=FALSE, approximation=FALSE, lambda=0)
# fit.auto.arima <- auto.arima(ts_train, max.p=5, max.q=5,
#                              max.P=5, max.Q=5, max.d=5, max.D=5, allowdrift = TRUE,
#                              stepwise=FALSE, approximation=FALSE, parallel = TRUE)
# 
# fit.auto.arima.boxcox <- auto.arima(ts_train, max.p=5, max.q=5,
#                                     max.P=5, max.Q=5, max.d=5, max.D=5, allowdrift = TRUE,
#                                     stepwise=FALSE, approximation=FALSE, lambda=0, parallel = TRUE)
# 
# fit.auto.arima
# fit.auto.arima.boxcox
# 
# pred.arima <- forecast(fit.auto.arima, h = length(ts_val))$mean
# pred.arima.boxcox <- forecast(fit.auto.arima.boxcox, h = length(ts_val))$mean
# 
# acc.arima <- accuracy(pred.arima, ts_all)
# acc.boxcox <- accuracy(pred.arima.boxcox, ts_all)
# 
# acc.arima
# acc.boxcox





