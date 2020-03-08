y = scan("C:/Users/Karyl/Documents/Karyl/Coding/R/NTUTimeSeriesAnalysisR/data/weekly sales of tech video.txt")
plot(1:161,xlim=c(0,200),ylim=c(20,100))
plot(1:161,y,xlim=c(0,200),ylim=c(20,100))
min(y)
max(y)

# non-stationary comes from mean (trending component)
?lines
lines(1:161,y,type="l")
acf(y) #decay extremely slowly
pacf(y) #pacf cut off at lag 7 (cannot suggest AR), pacf is useless in suggesting whether model is stationary or not

?diff
z=diff(y) #apply one time differencing
z
length(z)

plot(1:160,z,xlim=c(0,200),ylim=c(-10,10))
lines(1:160,z,type="l") #trending component is gone
min(z)
max(z)
acf(z) #cut off after lag 6
pacf(z) #cut off after lag 5 

fit = arima(x = y, order = c(0,1,6)) #MA(6)
fit
tsdiag(fit) #AIC = 726 (better model than below)


fit1 = arima(x = y, order=c(5,1,0))
fit1
tsdiag(fit1) #AIC = 737

plot (1:161, y, xlim=c(0,200), ylim=c(20,100))
lines(1:161, y, type="l")
lines(1:161, y - fit$residuals, type="l", col="red") #yhat = y-fit$residuals (e)

forecast = predict(fit, n.ahead=6)
forecast
lines(162:167, forecast$pred, type="o", col="red")
lines(162:167, forecast$pred-1.96*forecast$se, col="blue") #lower limit of confidence interval
lines(162:167, forecast$pred+1.96*forecast$se, col="blue") #upper limit of confidence interval

