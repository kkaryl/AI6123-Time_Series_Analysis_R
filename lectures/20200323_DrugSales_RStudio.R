library(forecast)
library(fpp)
library(ggplot2)
library(gridExtra)

### Plot original time plot
ts_all <- a10
index(ts_all)
ggplot(ts_all, aes(as.Date(ts_all), as.matrix(ts_all))) + 
  geom_line(colour = "red")+
  ylab("Sales") + 
  xlab("Date") + 
  ggtitle("Monthly anti-diabetic drug sales")+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), 
        legend.title=element_text(), 
        legend.key = element_rect(colour = "black"))

### Analyze Seasonal & Trending components of Data
# Decompose a time series into seasonal, trend and irregular components using moving averages. 
# Deals with additive or multiplicative seasonal component.
temp <- decompose(ts_all) 

tst <- data.frame(actual = as.matrix(temp$x), date = as.Date(temp$x), seasonal = as.matrix(temp$seasonal),
                  trend = as.matrix(temp$trend), random = as.matrix(temp$random), type = temp$type)

o <- ggplot(tst, aes(tst[,2], tst[,1]))+
  geom_line()+
  ylab("observed") + 
  xlab("") + 
  ggtitle(paste("Decomposition of",  tst$type, "time series", sep=" "))+
  scale_colour_gradient2(low = "blue", mid = "red",high = "black", midpoint = mean(tst[,1]), name="Tooth Length")+
  theme(axis.line = element_line(), axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), legend.text=element_text(), 
        legend.title=element_text(), legend.key = element_rect(colour = "black"), 
        plot.margin=unit(c(0,0.5,-0.4,0.5), "cm"))

s <- ggplot(tst, aes(tst[,2], tst[,3]))+
  geom_line()+
  ylab("seasonal") + 
  xlab("") + 
  ggtitle("")+
  scale_colour_gradient2(low = "blue", mid = "red",high = "black", midpoint = mean(tst[,1]), name="Tooth Length")+
  theme(axis.line = element_line(), axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), legend.text=element_text(), 
        legend.title=element_text(), legend.key = element_rect(colour = "black"),
        plot.margin=unit(c(0,0.5,-0.4,0.5), "cm"))

t <- ggplot(tst, aes(tst[,2], tst[,4]))+
  geom_line()+
  ylab("trend") + 
  xlab("") + 
  ggtitle("")+
  scale_colour_gradient2(low = "blue", mid = "red",high = "black", midpoint = mean(tst[,1]), name="Tooth Length")+
  theme(axis.line = element_line(), axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), legend.text=element_text(), 
        legend.title=element_text(), legend.key = element_rect(colour = "black"),
        plot.margin=unit(c(0,0.5,-0.4,0.5), "cm"))

r <- ggplot(tst, aes(tst[,2], tst[,5]))+
  geom_line()+
  ylab("random") + 
  xlab("") + 
  ggtitle("")+
  scale_colour_gradient2(low = "blue", mid = "red",high = "black", midpoint = mean(tst[,1]), name="Tooth Length")+
  theme(axis.line = element_line(), axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), legend.text=element_text(), 
        legend.title=element_text(), legend.key = element_rect(colour = "black"),
        plot.margin=unit(c(0,0.5,-0.4,0.5), "cm"))

grid.arrange(o,s,t,r, ncol = 1) # Arrange the graphs into stack of 4 in 1 column

### Create a box plot to see the spread of values in different months
tst <- data.frame(cbind(as.matrix(ts_all), as.matrix(cycle(ts_all))))

ggplot(tst, aes(as.factor(tst[,2]), tst[,1])) +
  ylab("Sales") + 
  xlab("Month") + 
  ggtitle("Sales for each month") + 
  stat_boxplot(geom ='errorbar')+ 
  geom_boxplot(outlier.shape = NA) +  
  geom_jitter(aes(colour=tst[,1]), position = position_jitter(width = 0.2)) +
  scale_colour_gradient2(low = "blue", mid = "red",high = "black", midpoint = mean(tst[,1]), name="Sales")+
  theme(axis.line = element_line(), 
        axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(), legend.title=element_text(), 
        legend.key = element_rect(colour = "black"))

### Train/ Test data split (0.7/ 0.3)
tst <- data.frame(cbind(as.matrix(ts_all), as.matrix(cycle(ts_all))))
splitTrainXvat <- function(tser, perc_train){
  ntrain <- floor(length(as.vector(tser)) * perc_train)
  nval <- length(as.vector(tser)) - ntrain
  
  ttrain <- ts(as.vector(tser[1:ntrain]), start = start(tser), frequency = frequency(tser))
  tval <- ts(as.vector(tser[ntrain + 1:nval]), start = end(ttrain) + deltat(tser), 
             frequency = frequency(tser))
  
  stopifnot(length(ttrain) == ntrain)
  stopifnot(length(tval) == nval)
  
  list(ttrain, tval)
}

data <- splitTrainXvat(ts_all, 0.70)
ts_train <- data[[1]]
length(ts_train)
# class(ts_train)
ts_val <- data[[2]]
length(ts_val)
frequency(ts_train)
# class(ts_val)

### Test different models to see which one minimizes RMSE.
mod.ar <- ar(ts_train, method = "yule-walker", lambda=0)
mod.reg <- tslm(ts_train ~ trend + season, lambda=0)
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

# mod.arima
# mod.arima.boxcox

### Forecast prediction for testing set and add additional 20 forecasts
pred.ar <- forecast(mod.ar, h = length(ts_val)+20)$mean
pred.reg <- forecast(mod.reg, h = length(ts_val)+20)$mean
pred.hw.mul <- exp(forecast(mod.hw.mul, h = length(ts_val)+20)$mean)
pred.hw.add <- exp(forecast(mod.hw.add, h = length(ts_val)+20)$mean)
pred.arima <- forecast(mod.arima, h = length(ts_val)+20)$mean
pred.arima.boxcox <- forecast(mod.arima.boxcox, h = length(ts_val)+20)$mean
pred.ets <- forecast(mod.ets, h = length(ts_val)+20)$mean
pred.tbats <- forecast(mod.tbats, h = length(ts_val)+20)$mean

### Plot AR and Regression model (plot1) and ETS and TBATS model (plot2)
dat <- data.frame(Y=as.matrix(ts_all), date=as.Date(ts_all), model = "Base")
dat <- rbind(dat, 
             data.frame(Y=as.matrix(pred.ar), date=as.Date(pred.ar), model = "AR"),
             data.frame(Y=as.matrix(pred.reg), date=as.Date(pred.reg), model = "Reg"))

plot1 <- ggplot(dat, aes(date, Y, colour = model)) + 
  geom_line()+
  geom_vline(aes(xintercept = as.numeric(date[length(ts_all)])), linetype = "longdash", color = "black")+
  ylab("")+
  xlab("")+
  ggtitle("Monthly anti-diabetic drug sales")+
  theme(axis.line = element_line(), axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), legend.text=element_text(), 
        legend.title=element_text(), legend.key = element_rect(colour = "black"))


dat2 <- data.frame(Y=as.matrix(ts_all), date=as.Date(ts_all), model = "Base")
dat2 <- rbind(dat2, 
             data.frame(Y=as.matrix(pred.ets), date=as.Date(pred.ets), model = "ETS"),
             data.frame(Y=as.matrix(pred.tbats), date=as.Date(pred.tbats), model = "TBATS"))

plot2 <- ggplot(dat2, aes(date, Y, colour = model)) + 
  geom_line()+
  geom_vline(aes(xintercept = as.numeric(date[length(ts_all)])), linetype = "longdash", color = "black")+
  ylab("")+
  xlab("")+
  ggtitle("Monthly anti-diabetic drug sales")+
  theme(axis.line = element_line(), axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), legend.text=element_text(), 
        legend.title=element_text(), legend.key = element_rect(colour = "black"))

grid.arrange(plot1, plot2, ncol = 1)

### Plot HW (add), HW (mul) model (plot1) and SARIMA, SARIMA(boxcox) model (plot2)
dat <- data.frame(Y=as.matrix(ts_all), date=as.Date(ts_all), model = "Base")
dat <- rbind(dat,
             data.frame(Y=as.matrix(pred.hw.add), date=as.Date(pred.hw.add), model = "HW.Add"),
             data.frame(Y=as.matrix(pred.hw.mul), date=as.Date(pred.hw.mul), model = "HW.Mul"))

plot1 <- ggplot(dat, aes(date, Y, colour = model)) +
  geom_line()+
  geom_vline(aes(xintercept = as.numeric(dat$date[length(ts_all)])), linetype = "longdash", color = "black")+
  ylab("")+
  xlab("")+
  ggtitle("Monthly anti-diabetic drug sales")+
  theme(axis.line = element_line(), axis.text=element_text(color='black'), axis.title = element_text(colour = 'black'), legend.text=element_text(), legend.title=element_text(), legend.key = element_rect(colour = "black"))


dat <- data.frame(Y=as.matrix(ts_all), date=as.Date(ts_all), model = "Base")
dat <- rbind(dat,
             data.frame(Y=as.matrix(pred.arima), date=as.Date(pred.arima), model = "SARIMA"),
             data.frame(Y=as.matrix(pred.arima.boxcox), date=as.Date(pred.arima.boxcox), model = "SARIMA.boxcox"))

plot2 <- ggplot(dat, aes(date, Y, colour = model)) +
  geom_line()+
  geom_vline(aes(xintercept = as.numeric(date[length(ts_all)])), linetype = "longdash", color = "black")+
  ylab("")+
  xlab("")+
  ggtitle("Monthly anti-diabetic drug sales")+
  theme(axis.line = element_line(), axis.text=element_text(color='black'), axis.title = element_text(colour = 'black'), legend.text=element_text(), legend.title=element_text(), legend.key = element_rect(colour = "black"))

grid.arrange(plot1, plot2, ncol = 1)

### Calculate accuracy
tst <- rbind(accuracy(pred.ar, ts_all),
             accuracy(pred.hw.mul, ts_all),
             accuracy(pred.hw.add, ts_all),
             accuracy(pred.reg, ts_all),
             accuracy(pred.arima, ts_all),
             accuracy(pred.arima.boxcox, ts_all),
             accuracy(pred.ets, ts_all),
             accuracy(pred.tbats, ts_all))

row.names(tst) <- c("AR",
                    "HW.Mul",
                    "HW.Add",
                    "Reg",
                    "SARIMA",
                    "SARIMA.boxcox",
                    "ETS",
                    "TBATS")
tst <- data.frame(tst)
tst <- tst[,(names(tst) %in% c("RMSE", "MAE", "MAPE", "Theil.s.U"))]

round(tst[order(tst$RMSE),], 5) # Regression & SARIMA_boxcox 

### Take mean forecast of Regression and SARIMA_boxcox
temp <- data.frame(Y = as.matrix(pred.arima.boxcox)+as.matrix(pred.reg))
temp$mean <- temp$Y/2

dat <- data.frame(Y=as.matrix(ts_all), date=as.Date(ts_all), model = "Base")
dat <- rbind(dat, 
             data.frame(Y=temp$mean, date=as.Date(pred.arima.boxcox), model = "Mean Model"))

ggplot(dat, aes(date, Y, colour = model)) + 
  geom_line()+
  geom_vline(aes(xintercept = as.numeric(date[length(ts_all)])), linetype = "longdash", color = "black")+
  ylab("")+
  xlab("")+
  ggtitle("Monthly anti-diabetic drug sales")+
  theme(axis.line = element_line(), axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), legend.text=element_text(), 
        legend.title=element_text(), legend.key = element_rect(colour = "black"))

### Compare error metrics of combination model
getPerformance <- function(pred, val){
  res <- pred - val
  MAE <- sum(abs(res))/length(val)
  RSS <- sum(res^2)
  MSE <- RSS/length(val)
  RMSE <- sqrt(MSE)
  APE <- abs((val-pred)/val)
  MAPE <- mean(APE)*100
  M6 <- sum(((pred[2:length(pred)]-val[2:(length(val))])/val[1:(length(val)-1)])^2)
  N6 <- sum(((val[2:length(val)]-val[1:(length(val)-1)])/val[1:(length(val)-1)])^2)
  Theil.s.U <- sqrt(M6/N6)
  perf <- data.frame(RMSE, MAE, MAPE, Theil.s.U)
}

perf <- cbind(type = c("Mean Model"), getPerformance(as.vector(temp$mean[1:length(ts_val)]), 
                                                     as.vector(ts_val)))
perf


