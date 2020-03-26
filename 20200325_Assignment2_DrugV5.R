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
# SHOWGRAPHS = FALSE

### Examine data
df_data = read.delim("./data/drug.txt", header=TRUE, sep = ",")
head(df_data)
nrow(df_data)
global.title <<- "Monthly Anti-diabetic Drug Sales"
global.xlab <<- "Year"
global.ylab <<- "Drug Sales"
global.freq <<- 12 # seasonality
df_data <- df_data %>% mutate(date = ymd(date))
global.start <<- ymd(as.Date(df_data$date[[1]]))
global.end <<- ymd(as.Date(df_data$date[[nrow(df_data)]]))
### Convert to time series data
ts_all <- ts(df_data$value, start=c(year(global.start), month(global.start)),
             end=c(year(global.end), month(global.end)), frequency=global.freq)
plot.ts <- plotts(ts_all)
if (SHOWGRAPHS == TRUE)
  plot.ts
plotAcf2(ts_all, plot=SHOWGRAPHS)

### Stationary test
adf.test(ts_all)
kpss.test(ts_all)

### Seasonal decomposition using stl()
fit <- stl(ts_all, s.window="period")
autoplot(fit)

### Split dataset to train test set
num_of_train <- round(0.8 * length(ts_all)) %>% print()
# ts_train <- window(ts_all, start=c(1991, 7), end=c(2004, 12))
# ts_val <- window(ts_all, start=c(2005, 1), end=c(2008, 6))
ts_train <- head(ts_all, num_of_train)
length(ts_train)
ts_val <- tail(ts_all, round(length(ts_all) - num_of_train))
length(ts_val)
# ts_train

### Find suitable SARIMA model
NYEAR = 5
# to find optimal lambda
lambda = BoxCox.lambda(ts_all) %>% print()
# lambda = 0.13

# now to transform vector
ts_train_t = BoxCox(ts_train,lambda) #should use BoxCoxs to reduce seasonality
plotts(ts_train)
plotts(ts_train_t)
ts_train_td = diff(ts_train_t, differences = 1)
plotts(ts_train_td, "blue")

ts_train_tdD = diff(ts_train_t, differences = 1, lag = 12)
plotts(ts_train_tdD)

# 0.7
# ggAcf(ts_train_tdD, lag.max = 12*NYEAR) # Q = 2, q = 6 or 3
# #ggAcf(coredata(ts_train.tdD), lag.max = 12*NYEAR) # Q = 2, q = 6
# ggPacf(ts_train_tdD, lag.max = 12*NYEAR) # P = 2, p = 3 

# ggAcf(ts_train_tdD, lag.max = 12*NYEAR) # Q = 2, q = 6
# #ggAcf(coredata(ts_train.tdD), lag.max = 12*NYEAR) # Q = 2, q = 6
# ggPacf(ts_train_tdD, lag.max = 12*NYEAR) # P = 3, p = 3  

# lambda = 0
# ggAcf(ts_train_tdD, lag.max = 12*NYEAR) # Q = 1, q = 9
# ggPacf(ts_train_tdD, lag.max = 12*NYEAR) # P = 2, p = 3  

# lambda = 0.1
# ggAcf(ts_train_tdD, lag.max = 12*NYEAR) # Q = 1 or 2, q = 8
# ggPacf(ts_train_tdD, lag.max = 12*NYEAR) # P = 3, p = 3  

# lambda = 0.13
ggAcf(ts_train_tdD, lag.max = 12*NYEAR) # Q = 2, q = 8
ggPacf(ts_train_tdD, lag.max = 12*NYEAR) # P = 3, p = 3  

# Try ARMA(p,q) model
# fit.arima1 = stats::arima(ts_train_t,
#                    order=c(3,1,9),
#                    seasonal=list(order=c(2,1,1),period=12))
# checkresiduals(fit.arima1)
# tsdiag(fit.arima1)
# summary(fit.arima1)

# lambda 0
# fit.arima1 = stats::arima(ts_train_t,
#                           order=c(3,1,9),
#                           seasonal=list(order=c(2,1,2),period=12))
# checkresiduals(fit.arima1)
# tsdiag(fit.arima1)
# summary(fit.arima1)
# 
# pred.arima1 <- InvBoxCox(forecast(fit.arima1, h = length(ts_val)+20)$mean, lambda = lambda)
# 
# accuracy(pred.arima1, ts_all)

# lambda 0.1
fit.arima1 = stats::arima(ts_train_t,
                          order=c(3,1,8),
                          seasonal=list(order=c(3,1,1),period=12))
checkresiduals(fit.arima1)
tsdiag(fit.arima1)
summary(fit.arima1)

pred.arima1 <- InvBoxCox(forecast(fit.arima1, h = length(ts_val)+20)$mean, lambda = lambda)

accuracy(pred.arima1, ts_all)

# Try MA(q) model
# fit.arima2 = stats::arima(ts_train_t,
#                           order=c(0,1,9),
#                           seasonal=list(order=c(0,1,1),period=12))
# checkresiduals(fit.arima2)
# tsdiag(fit.arima2)
# summary(fit.arima2)
# 
# pred.arima2 <- InvBoxCox(forecast(fit.arima2, h = length(ts_val)+20)$mean, lambda = lambda)
# 
# accuracy(pred.arima2, ts_all)

# fit.arima2 = stats::arima(ts_train_t,
#                           order=c(0,1,9),
#                           seasonal=list(order=c(0,1,1),period=12))
# checkresiduals(fit.arima2)
# tsdiag(fit.arima2)
# summary(fit.arima2)
# 
# pred.arima2 <- InvBoxCox(forecast(fit.arima2, h = length(ts_val)+20)$mean, lambda = lambda)
# 
# accuracy(pred.arima2, ts_all)

# lambda 0.1
fit.arima2 = stats::arima(ts_train_t,
                          order=c(0,1,8),
                          seasonal=list(order=c(0,1,1),period=12))
checkresiduals(fit.arima2)
tsdiag(fit.arima2)
summary(fit.arima2)
pred.arima2 <- InvBoxCox(forecast(fit.arima2, h = length(ts_val)+20)$mean, lambda = lambda)
accuracy(pred.arima2, ts_all)

global.pred.start <<- start(pred.arima1) %>% print()
global.pred.end <<- end(pred.arima1) %>% print()

plot.arimas <- plotforecast(ts_all, pred.arima1, "ARIMA1", pred.arima2, "ARIMA2")
plot.arimas


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





