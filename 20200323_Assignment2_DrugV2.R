library(dplyr)
library(lubridate) # for converting data to year month day
library(xts)
library(caTools)
library(astsa)
library(fpp)
library(ggplot2)

### Read data into XTS object
df_data = read.delim("./data/drug.txt", header=TRUE, sep = ",")
head(df_data)

df_data <- df_data %>% mutate(date = ymd(date))
plot(df_data,
     main="Monthly anti-diabetic drug sales",
     xlab="Year", ylab="Drug Sales")
lines(df_data$date, df_data$value,type="l")

acf2(df_data$value)

### Split dataset to train test set
num_of_rows <- nrow(df_data)
train_idx <- 1:round(0.7 * num_of_rows)
df_train <- df_data[train_idx, ]
test_idx <- (round(0.7 * num_of_rows) + 1):num_of_rows
df_test <- df_data[test_idx, ]

# xts_train <- as.xts(df_train$value, as.Date(df_train$date, format="%Y-%m-%d"), frequency=12)
# xts_test <- as.xts(df_test$value, as.Date(df_test$date, format="%Y-%m-%d"), frequency=12)

# mod.holt <- holt(df_train, h=20)
# mod.holt$model

# plot(df_train, type="o", main="Monthly anti-diabetic drug sales", xlab="Year", ylab="Drug Sales")
# plot(mod.holt, PI=FALSE, ylab="Oil (millions of tonnes)",
#      xlab="Year", main="Monthly anti-diabetic drug sales", 
#      fcol="white", type="o")
# lines(fitted(mod.holt), col="red")
# lines(mod.holt$mean, col="red", type="o")
xts_data <- as.xts(df_data$value, as.Date(df_data$date, format="%Y-%m-%d"))
attr(xts_data, 'frequency') <- 12
xts_train <- as.xts(df_train$value, as.Date(df_train$date, format="%Y-%m-%d"))
attr(xts_train, 'frequency') <- 12
xts_test <- as.xts(df_test$value, as.Date(df_test$date, format="%Y-%m-%d"))
attr(xts_test, 'frequency') <- 12
length(xts_test)
# mod.hw.mul <- hw(log(xts_train), seasonal = "mul", h=20)
# mod.hw.add <- hw(log(xts_train), seasonal = "add")
# pred.hw.mul <- exp(forecast(mod.hw.mul, h = length(xts_test)+20)$mean)
# pred.hw.add <- exp(forecast(mod.hw.add, h = length(xts_test)+20)$mean)
### Test different models to see which one minimizes RMSE.
# mod.ar <- ar(xts_train, method = "yule-walker", lambda=0)
# mod.reg <- tslm(xts_train ~ trend + season, lambda=0)
mod.hw.mul <- HoltWinters(log(xts_train), seasonal = "mul")
mod.hw.add <- HoltWinters(log(xts_train), seasonal = "add")
# mod.arima <- auto.arima(xts_train, max.p=2, max.q=2,
#                         max.P=2, max.Q=2, max.d=2, max.D=2, allowdrift = TRUE,
#                         stepwise=FALSE, approximation=FALSE)
# mod.arima.boxcox <- auto.arima(xts_train, max.p=2, max.q=2,
#                                max.P=2, max.Q=2, max.d=2, max.D=2, allowdrift = TRUE,
#                                stepwise=FALSE, approximation=FALSE, lambda=0)
# mod.ets <- ets(xts_train, model="ZZZ", lambda=0)
# mod.tbats <- tbats(xts_train, use.box.cox=TRUE)

### Forecast prediction for testing set and add additional 20 forecasts
# pred.ar <- forecast(mod.ar, h = length(xts_test)+20)$mean
# pred.reg <- forecast(mod.reg, h = length(xts_test)+20)$mean
pred.hw.mul <- exp(forecast(mod.hw.mul, h = length(xts_test)+20)$mean)
pred.hw.add <- exp(forecast(mod.hw.add, h = length(xts_test)+20)$mean)
# pred.arima <- forecast(mod.arima, h = length(xts_test)+20)$mean
# pred.arima.boxcox <- forecast(mod.arima.boxcox, h = length(xts_test)+20)$mean
# pred.ets <- forecast(mod.ets, h = length(xts_test)+20)$mean
# pred.tbats <- forecast(mod.tbats, h = length(xts_test)+20)$mean

dat <- data.frame(value=as.matrix(xts_data), date=as.Date(xts_data), model = "Base")
dat <- rbind(dat,
             data.frame(value=as.matrix(pred.hw.add), date=as.Date(pred.hw.add), model = "HW.Add"),
             data.frame(value=as.matrix(pred.hw.mul), date=as.Date(pred.hw.mul), model = "HW.Mul"))

plot1 <- ggplot(dat, aes(date, value, colour = model)) +
  geom_line()+
  geom_vline(aes(xintercept = as.numeric(dat$date[nrow(df_data)])), linetype = "longdash", color = "black")+
  ylab("")+
  xlab("")+
  ggtitle("Monthly anti-diabetic drug sales")+
  theme(axis.line = element_line(), axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), legend.text=element_text(), 
        legend.title=element_text(), legend.key = element_rect(colour = "black"))

grid.arrange(plot1, ncol = 1)

# ggplot(dat, aes(date, value, colour = model)) +
#   geom_line()+
#   geom_vline(aes(xintercept = as.numeric(dat$date[length(ts_all)])), linetype = "longdash", color = "black")+
#   ylab("")+
#   xlab("")+
#   ggtitle("Monthly anti-diabetic drug sales")+
#   theme(axis.line = element_line(), axis.text=element_text(color='black'),
#         axis.title = element_text(colour = 'black'), legend.text=element_text(),
#         legend.title=element_text(), legend.key = element_rect(colour = "black"))


