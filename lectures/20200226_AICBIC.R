y = c(0.50, -0.41, 0.37, -0.61, 0.23,-0.13, 0.06, -0.11, 0.18, -0.14, 0.20, 0.09, -0.03, -0.02, -0.14, -0.07,0.09, 0.09, -0.01, -0.10)
acf(y)
pacf(y)
fit = arima(y, order=c(1,0,0)) #AR(1)
fit
tsdiag(fit)


# now try MA(3)
fit1 = arima(x = y, order=c(0,0,3)) #MA(3)
fit1
# the p-values of LB is better than 0.05 (model is adequate)
tsdiag(fit1) 

AIC(fit) # AIC of fit (AR1) is smaller (neg) than AIC of fit1 (MA3)
AIC(fit1)

?pchisq # to evaluate the p-values for the LB statistics
pchisq(y)


??auto.arima # select best arima model for you based on max p, d, q auto cal AIC
autofit = auto.arima(y)
autofit
