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

source('assignment2/helpers.R')

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
acf2(ts_all, plot=SHOWGRAPHS)

# Stationary test
# adf.test(ts_all)

### Seasonal decomposition using stl()
# fit <- stl(ts_all, s.window="period")
# plot(fit)
# 
# monthplot(ts_all)
# seasonplot(ts_all)

### Split dataset to train test set
num_of_train <- round(0.7 * length(ts_all))
ts_train <- head(ts_all, num_of_train)
ts_val <- tail(ts_all, round(length(ts_all) - num_of_train))

### Predict using Holt-Winters' Trend and Seasonality Model
log_ts_train = log(ts_train)
fit.hw.add <- hw(log_ts_train, seasonal = "additive")
fit.hw.mul <- hw(log_ts_train, seasonal = "multiplicative")

pred.hw.add <- exp(forecast(fit.hw.add$mean, h=length(ts_val))$mean)
pred.hw.mul <- exp(forecast(fit.hw.mul$mean, h=length(ts_val))$mean)
global.pred.start <<- start(pred.hw.add)
global.pred.end <<- end(pred.hw.add)

acc.hw.add <- accuracy(pred.hw.add, ts_all)
acc.hw.mul <- accuracy(pred.hw.mul, ts_all)

plot.hw <- plotforecast(ts_all, pred.hw.add, "HW ADD", pred.hw.mul, "HW MUL")
if (SHOWGRAPHS == TRUE)
  plot.hw
# grid.arrange(plot.hw, ncol = 1)

### Predict using Seasonal ARIMA Model
# acf2(ts_all)
ts_all_d1 = diff(ts_all, differences = 1)
plotforecast(ts_all_d1)
# acf2(ts_all_d1)
ts_all_d2 = diff(ts_all, differences = 2)
plotforecast(ts_all_d2)
# acf2(ts_all_d2)

# fit.auto.arima <- auto.arima(ts_train, max.p=2, max.q=2,
#                              max.P=2, max.Q=2, max.d=2, max.D=2, allowdrift = TRUE,
#                              stepwise=FALSE, approximation=FALSE)
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





