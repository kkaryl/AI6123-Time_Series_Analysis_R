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
library(gridExtra)
library(tseries)

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
sltfit <- stl(ts_all, s.window="period")
autoplot(sltfit)

### Split dataset to train test set
num_of_train <- round(0.8 * length(ts_all)) %>% print()
ts_train <- head(ts_all, num_of_train)
ts_test <- tail(ts_all, round(length(ts_all) - num_of_train))
length(ts_test)

### Find suitable SARIMA model
## Find optimal lambda
# lambda = BoxCox.lambda(ts_all) %>% print()
lambda = 0
## Transform data using BoxCox to reduce variance
ts_train_t = BoxCox(ts_train,lambda)

NYEAR = 5
findPQpq <- function(ts_train_t, lambda, SHOWGRAPHS, NYEAR) {
  if (SHOWGRAPHS == TRUE)
    transTitle <- sprintf("After BoxCox Transform: lambda = %f", lambda)
    grid.arrange(plotts(ts_train), plotts(ts_train_t, "red", 
                                          title=transTitle), ncol=1)
  
  ## Perform one time differencing to remove trend 
  ts_train_td = diff(ts_train_t, differences = 1)
  plot.ts_train_td <- plotts(ts_train_td, title="d=1")
  
  ## Perform one seasonal differencing and to remove seasonality 
  ts_train_tdD = diff(ts_train_td, lag = global.freq)
  plot.ts_train_tdD <- plotts(ts_train_tdD, "darkgreen", "d=1, D=1")
  
  sltfit <- stl(ts_train_tdD, s.window="period")
  autoplot(sltfit)
  
  if (SHOWGRAPHS == TRUE)
    grid.arrange(plot.ts_train_td, plot.ts_train_tdD, ncol=1)
  
  plotAcf2(ts_train_tdD, lag.max = global.freq*NYEAR) 
}

findPQpq(ts_train, lambda=lambda, SHOWGRAPHS = SHOWGRAPHS, NYEAR=NYEAR) # Q = 2, q = 9, P = 2, p = 3 

# Q = 2, q = 4, P = 0, p = 5
# Try ARMA(p,q) model
fit.arimaARMA = stats::arima(ts_train_t,
                             order=c(5,1,4),
                             seasonal=list(order=c(0,1,2), period=global.freq))
checkresiduals(fit.arimaARMA)
tsdiag(fit.arimaARMA)
summary(fit.arimaARMA)
pred.arimaARMA <- InvBoxCox(forecast(fit.arimaARMA, h = length(ts_test)+20)$mean, lambda = lambda)
accuracy(pred.arimaARMA, ts_all) 

# Try MA(q) model
fit.arimaMA = stats::arima(ts_train_t,
                             order=c(0,1,4),
                             seasonal=list(order=c(0,1,2), period=global.freq))
checkresiduals(fit.arimaMA)
tsdiag(fit.arimaMA)
summary(fit.arimaMA)
pred.arimaMA <- InvBoxCox(forecast(fit.arimaMA, h = length(ts_test)+20)$mean, lambda = lambda)
accuracy(pred.arimaMA, ts_all)

# Try AR(p) model
fit.arimaAR = stats::arima(ts_train_t,
                           order=c(5,1,0),
                           seasonal=list(order=c(0,1,0), period=global.freq))
checkresiduals(fit.arimaAR)
tsdiag(fit.arimaAR)
summary(fit.arimaAR)
pred.arimaAR <- InvBoxCox(forecast(fit.arimaAR, h = length(ts_test)+20)$mean, lambda = lambda)
accuracy(pred.arimaAR, ts_all)

## Plot prediction graphs
global.pred.start <<- start(pred.arimaARMA)
global.pred.end <<- end(pred.arimaARMA)
plot.arimas0 <- plotforecast(ts_all, pred.arimaARMA, "ARIMA ARMA", pred.arimaMA, "ARIMA MA",
                             pred.arimaAR, "ARIMA AR")
if (SHOWGRAPHS)
  plot.arimas0
# tryARIMAModels <- function(ts_all, ts_train, lambda, p, q, P, Q, forecast_h,
#                            d=1, D=1) {
#   ts_train_t = BoxCox(ts_train ,lambda) 
#   fit.arima = stats::arima(ts_train_t,
#                            order=c(p,d,q),
#                            seasonal=list(order=c(P,D,Q), period=12))
#   checkresiduals(fit.arima)
#   tsdiag(fit.arima)
#   summary(fit.arima)
#   pred.arima <- InvBoxCox(forecast(fit.arima, h = forecast_h)$mean, lambda = lambda)
#   accuracy(pred.arima, ts_all) %>% print()
#   global.pred.start <<- start(pred.arima) 
#   global.pred.end <<- end(pred.arima)
#   return (ts(pred.arima, start=c(global.pred.start), 
#              end=c(global.pred.end), frequency=global.freq))
#   
# }
# 
# # Try ARMA(p,q) model
# pred.arimaARMA <- tryARIMAModels(ts_all, ts_train, lambda, p=3, q=9, P=2, Q=2, d=1, D=1,
#                forecast_h = length(ts_test)+20)
# 
# 
# # Try MA(q) model
# pred.arimaMA <- tryARIMAModels(ts_all, ts_train, lambda, p=0, q=9, P=0, Q=2, d=1, D=1,
#                             forecast_h = length(ts_test)+20)
# 
# plot.arimas <- plotforecast(ts_all, pred.arimaARMA, "ARIMA ARMA", pred.arimaMA, "ARIMA MA")
# plot.arimas

# Try ARMA(p,q) model
# fit.arimaARMA = stats::arima(ts_train_t,
#                              order=c(3,1,9),
#                              seasonal=list(order=c(2,1,2), period=global.freq))
# checkresiduals(fit.arimaARMA)
# tsdiag(fit.arimaARMA)
# summary(fit.arimaARMA)
# pred.arimaARMA <- InvBoxCox(forecast(fit.arimaARMA, h = length(ts_test)+20)$mean, lambda = lambda)
# pred.arimaARMA
# accuracy(pred.arimaARMA, ts_all) 
# 
# # Try MA(q) model
# fit.arimaMA = stats::arima(ts_train_t,
#                              order=c(0,1,9),
#                              seasonal=list(order=c(0,1,2),period=global.freq))
# checkresiduals(fit.arimaMA)
# tsdiag(fit.arimaMA)
# summary(fit.arimaMA)
# pred.arimaMA <- InvBoxCox(forecast(fit.arimaMA, h = length(ts_test)+20)$mean, lambda = lambda)
# accuracy(pred.arimaMA, ts_all) 
# 
# ## Plot prediction graphs
# global.pred.start <<- start(pred.arimaARMA)
# global.pred.end <<- end(pred.arimaARMA)
# plot.arimas0 <- plotforecast(ts_all, pred.arimaARMA, "ARIMA ARMA", pred.arimaMA, "ARIMA MA")
# plot.arimas0

# Try non-zero lambda
lambda = BoxCox.lambda(ts_all) %>% print() # 0.1313326
ts_train_t = BoxCox(ts_train,lambda) 
findPQpq(ts_train, lambda=lambda, SHOWGRAPHS = SHOWGRAPHS, NYEAR=NYEAR) # Q = 2, q = 6 /8, P = 3, p = 3
# Q = 0 or 2, q = 4 or 7, P = 0 or 3, p = 4 or 5
# Try ARMA(p,q) model
fit.arimaARMA = stats::arima(ts_train_t,
                             order=c(5,1,4),
                             seasonal=list(order=c(3,1,0),period=global.freq))
checkresiduals(fit.arimaARMA)
tsdiag(fit.arimaARMA)
summary(fit.arimaARMA)
pred.arimaARMA <- InvBoxCox(forecast(fit.arimaARMA, h = length(ts_test)+20)$mean, lambda = lambda)
accuracy(pred.arimaARMA, ts_all) 


# ts_train_tdD = diff(ts_train_t, differences = 1, lag = global.freq)
# plotAcf2(ts_train_tdD, lag.max = 12*NYEAR) 

# Try ARMA(p,q) model
fit.arimaARMA = stats::arima(ts_train_t,
                             order=c(3,1,6),
                             seasonal=list(order=c(3,1,2),period=global.freq))
checkresiduals(fit.arimaARMA)
tsdiag(fit.arimaARMA)
summary(fit.arimaARMA)
pred.arimaARMA <- InvBoxCox(forecast(fit.arimaARMA, h = length(ts_test)+20)$mean, lambda = lambda)
accuracy(pred.arimaARMA, ts_all) 

# Try MA(q) model
fit.arimaARMA = stats::arima(ts_train_t,
                             order=c(0,1,6),
                             seasonal=list(order=c(0,1,2),period=global.freq))
checkresiduals(fit.arimaARMA)
tsdiag(fit.arimaARMA)
summary(fit.arimaARMA)
pred.arimaARMA <- InvBoxCox(forecast(fit.arimaARMA, h = length(ts_test)+20)$mean, lambda = lambda)
accuracy(pred.arimaARMA, ts_all) 

## Plot prediction graphs
plot.arimas0.13 <- plotforecast(ts_all, pred.arimaARMA, "ARIMA ARMA", pred.arimaMA, "ARIMA MA")
if (SHOWGRAPHS)
  plot.arimas0.13 

# ## Transform data using BoxCox to reduce variance
# ts_train_t = BoxCox(ts_train,lambda) 
# if (SHOWGRAPHS == TRUE)
#   grid.arrange(plotts(ts_train), plotts(ts_train_t, "red"), ncol=1)
# 
# ## Perform one time differencing to remove trend 
# ts_train_td = diff(ts_train_t, differences = 1)
# plot.ts_train_td <- plotts(ts_train_td, title="d=1")
# 
# ## Perform one seasonal differencing and to remove seasonality 
# ts_train_tdD = diff(ts_train_t, differences = 1, lag = 12)
# # ts_train_tdD = diff(ts_train_td, lag = 12)
# plot.ts_train_tdD <- plotts(ts_train_tdD, "darkgreen", "d=1, D=1")
# 
# if (SHOWGRAPHS == TRUE)
#   grid.arrange(plot.ts_train_td, plot.ts_train_tdD, ncol=1)
# 
# plotAcf2(ts_train_tdD, lag.max = 12*NYEAR) # Q = 2, q = 9, P = 2, p = 3 

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
# ggAcf(ts_train_tdD, lag.max = 12*NYEAR) # Q = 2, q = 8
# ggPacf(ts_train_tdD, lag.max = 12*NYEAR) # P = 3, p = 3  

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
# pred.arima1 <- InvBoxCox(forecast(fit.arima1, h = length(ts_test)+20)$mean, lambda = lambda)
# 
# accuracy(pred.arima1, ts_all)

# lambda 0.1
# fit.arima1 = stats::arima(ts_train_t,
#                           order=c(3,1,8),
#                           seasonal=list(order=c(3,1,1),period=12))
# checkresiduals(fit.arima1)
# tsdiag(fit.arima1)
# summary(fit.arima1)
# 
# pred.arima1 <- InvBoxCox(forecast(fit.arima1, h = length(ts_test)+20)$mean, lambda = lambda)
# 
# accuracy(pred.arima1, ts_all)

# Try MA(q) model
# fit.arima2 = stats::arima(ts_train_t,
#                           order=c(0,1,9),
#                           seasonal=list(order=c(0,1,1),period=12))
# checkresiduals(fit.arima2)
# tsdiag(fit.arima2)
# summary(fit.arima2)
# 
# pred.arima2 <- InvBoxCox(forecast(fit.arima2, h = length(ts_test)+20)$mean, lambda = lambda)
# 
# accuracy(pred.arima2, ts_all)

# fit.arima2 = stats::arima(ts_train_t,
#                           order=c(0,1,9),
#                           seasonal=list(order=c(0,1,1),period=12))
# checkresiduals(fit.arima2)
# tsdiag(fit.arima2)
# summary(fit.arima2)
# 
# pred.arima2 <- InvBoxCox(forecast(fit.arima2, h = length(ts_test)+20)$mean, lambda = lambda)
# 
# accuracy(pred.arima2, ts_all)

# lambda 0.1
# fit.arima2 = stats::arima(ts_train_t,
#                           order=c(0,1,8),
#                           seasonal=list(order=c(0,1,1),period=12))
# checkresiduals(fit.arima2)
# tsdiag(fit.arima2)
# summary(fit.arima2)
# pred.arima2 <- InvBoxCox(forecast(fit.arima2, h = length(ts_test)+20)$mean, lambda = lambda)
# accuracy(pred.arima2, ts_all)
# 
# global.pred.start <<- start(pred.arima1) %>% print()
# global.pred.end <<- end(pred.arima1) %>% print()
# 
# plot.arimas <- plotforecast(ts_all, pred.arima1, "ARIMA1", pred.arima2, "ARIMA2")
# plot.arimas


### Predict using Holt-Winters' Trend and Seasonality Model
# lambda = BoxCox.lambda(ts_all) %>% print() # 0.1313326
lambda = 0
ts_train_t = BoxCox(ts_train,lambda) 

fit.hw.add <- hw(ts_train_t, seasonal = "additive")
fit.hw.mul <- hw(ts_train_t, seasonal = "multiplicative")

pred.hw.add <- InvBoxCox(forecast(fit.hw.add$mean, h=length(ts_test))$mean, lambda = lambda)
pred.hw.mul <- InvBoxCox(forecast(fit.hw.mul$mean, h=length(ts_test))$mean, lambda = lambda)

global.pred.start <<- start(pred.hw.add) %>% print()
global.pred.end <<- end(pred.hw.add) %>% print()

acc.hw.add <- accuracy(pred.hw.add, ts_all) %>% print()
acc.hw.mul <- accuracy(pred.hw.mul, ts_all) %>% print()

plot.hw <- plotforecast(ts_all, pred.hw.add, "HW's ADD", pred.hw.mul, "HW's MUL")
if (SHOWGRAPHS == TRUE)
  plot.hw

### Seasonal Regression Model
lambda = 0
# lambda = BoxCox.lambda(ts_train) %>% print() # 0.1313326
fit.tslm <- tslm(ts_train ~ trend + season, lambda=lambda)
pred.tslm <- forecast(fit.tslm, h = length(ts_test)+20)$mean
acc.tslm <- accuracy(pred.tslm, ts_all) %>% print()

plot.tslm <- plotforecast(ts_all, pred.tslm, "TSLM")
if (SHOWGRAPHS == TRUE)
  plot.tslm

