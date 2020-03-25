# This data is for monthly anti-diabetic drug sales in Australia from 1992 to 2008. 
# Total monthly scripts for pharmaceutical products falling under ATC code A10, 
# as recorded by the Australian Health Insurance Commission. 
# 
# Please build a good model to predict the drug sales
# 
# It is due on 3 April.
library(dplyr)
library(lubridate) # for converting data to year month day
library(xts)
library(caTools)
library(astsa)

# library(caret)

library(forecast)
library(fpp)
# library(ggplot2)
# library(gridExtra)

data2 = as.xts(read.zoo("./data/drug.txt", index.column = 1, sep = ",", header=TRUE, format="%Y-%m-%d"))
head(data2)

plot(data2,
     main="Monthly anti-diabetic drug sales",
     xlab="Year", ylab="Drug Sales")
# lines(data2,type="l")

# data = read.delim("./data/drug.txt", header=TRUE, sep = ",")
# head(data)
# nrow(data)

### Convert data to time series data for easier manipulation
# data <- data %>% mutate(date = ymd(date))

# plot(data$date, data$value,
#      main="Monthly anti-diabetic drug sales",
#      xlab="Year", ylab="Drug Sales")
# lines(data$date,data$value,type="l")
# grid (lty = 6, col = "cornsilk2")
# 
# xaxp <- par("xaxp")
# yaxp <- par("yaxp")
# 
# abline(v=seq(xaxp[1], xaxp[2], (xaxp[2]-xaxp[1])/xaxp[3]), lty=6, col = "cornsilk2")
# abline(h=seq(yaxp[1], yaxp[2], (yaxp[2]-yaxp[1])/yaxp[3]), lty=6, col = "cornsilk2")
## Data contains trending and seasonal component

acf2(data$value)
xts_data <- as.xts(data$value, order.by=data$date)
z1=diff(data2)
acf2(z1)

## Split dataset to train test set
set.seed(123)

class(ts_data)
idx <- sample.split(ts_data, 0.70)
ts_train <- ts_data[idx, ]
ts_val <- ts_data[!idx, ]
nrow(ts_train)
nrow(ts_val)


#see model parameters
mod.ar <- ar(ts_train, method = "yule-walker", lambda=0)
mod.reg <- tslm(as.ts(ts_train) ~ trend + season, lambda=0) #!!
mod.hw.mul <- HoltWinters(log(ts_train), seasonal = "mul")
mod.hw.add <- HoltWinters(log(ts_train), seasonal = "add")
mod.arima <- auto.arima(ts_train, max.p=2, max.q=2,
                        max.P=2, max.Q=2, max.d=2, max.D=2, allowdrift = TRUE,
                        stepwise=FALSE, approximation=FALSE)
mod.arima.boxcox <- auto.arima(ts_train, max.p=2, max.q=2,
                               max.P=2, max.Q=2, max.d=2, max.D=2, allowdrift = TRUE,
                               stepwise=FALSE, approximation=FALSE, lambda=0)
mod.ets <- ets(ts_train, model="ZZZ", lambda=0)
mod.tbats <- tbats(ts_train, use.box.cox=TRUE)




