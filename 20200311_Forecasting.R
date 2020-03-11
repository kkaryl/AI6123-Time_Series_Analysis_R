library(fpp)
oil
oildata <- window(oil, start=1996) # extract subset of dataset
plot(oildata, ylab="Oil (millions of tonnes)", xlab="Year",
     main=" Oil production in Saudi Arabia from 1996 to 2007 " )

###simple exponential method
###choose the best alpha of SEM
# identify optimal alpha parameter
alpha <- seq (.01 , .99, by = .01) #step 0.01
RMSE <- NA #RMSE : root of mse
for(i in seq_along(alpha)) {
  #ses = simple exp method, h = how many steps forecast
  fit <- ses(oildata , alpha = alpha[i]) #, h = 10) #h isnot essential
  RMSE[i] <- accuracy(fit)[2] # 7 components to measure the ses. The second component is RMSE
}
ind <- which(RMSE == min(RMSE)) # select the min RMSE 
print(alpha[ind]) #index = position 79th of the array
alp=alpha[ind] #0.79

###use alp to forecast
fc1=ses(oildata, alpha=alp, initial="simple" , h=5)
plot(fc1, PI=F, ylab="Oil (millions of tonnes)",
     xlab="Year", main="Forecast oil production", 
     fcol="white", type="o")
lines(fitted(fc1), col="blue", type="o")
lines(fc1$mean, col="blue", type="o") # add the five steps forecasting
# ses is not good for trending data

###holt's linear
##Within holt you can manually set the Îs and Îs.
##However, if you leave those parameters as NULL,
##the holt function will actually identify the optimal model parameters
##by minimizing AIC and BIC values.
fc2 = holt(oildata, h=5)
fc2$model
fc2 # forecast values are not constant unlike ses

# AIC     AICc      BIC
# 148.4423 155.1090 151.9826

lines(fitted(fc2), col="red")
lines(fc2$mean, col="red", type="o")

legend("topleft", lty=1, col=c("black", "blue", "red"),
       c("Data", "Simple exponential", "Holt's linear trend"))

# holt also not very good, as it is far from original data


### EXAMPLE 2
austourists
aust <- window(austourists, start=2005)
fit1 <- hw(aust, seasonal = "additive")
?hw # h = 2* frequency(x) => freq(x) = 4l
fit1 # 2 years prediction
fit2 <- hw(aust, seasonal = "multiplicative")

plot(aust, type="o", main="International", xlab="Year", ylab="International")
plot(fit2, PI=FALSE, type="o", fcol="white", xlab="Year", main="Forecast")
lines(fitted(fit1), col="red", lty=2)
lines(fitted(fit2), col="green", lty=2) # mult better than additive
# forecast plot
lines(fit1$mean, type="o", col="red") # not actual mean! just = forecast values
lines(fit2$mean, type="o", col="green")

legend("topleft", lty=1, pch=1, col=1:3, c(" data " , "HWADD" , "HWMUL" ))
  

## EXAMPLE 3
y = c(3.91, 3.86, 3.81, 3.02, 2.62, 1.89, -1.13, -3.82, -5.08, -4.42, -1.99, 0.70,
      1.86, 2.98, 1.78, 3.01, 2.13, 3.23, 3.17, 4.64, 5.20, 6.76, 5.79, 5.08, 1.88, -0.72, -2.00, -3.03,
      -2.35, -3.34, -3.21, -3.57, -4.28, -3.54, -3.16, -1.41, 0.48, 1.61, 2.42, 2.11, 2.45, 1.39, 2.04,
      1.71, 3.26, 3.20, 1.43, 1.68, 4.17, 4.75)
# seems stationary, but it fluctuates around the horizontal 
length(y)
acf(y, lag.max=30) # acf cut off after 15
pacf(y) 
fit1=arima(y, order=c(2,0,0))
fit1
fit2=arima(y, order=c(0,0,14))
fit2

fit3=auto.arima(y)
fit3
tsdiag(fit3)
  