x = c(1.0445, -0.1338, 0.6706, 0.3755, -0.5110, -
       0.2352, 0.1595, 1.6258, -1.6739, 2.4478, -3.1019, 2.6860, -0.9905, 1.2113, -0.0929, 0.9905,
      0.5213, -0.1139, -0.4062, 0.5438)
plot.ts(x) #mean seems roughly the same over the time
acf(x) #data seems stationary, ACF cut off after lag 3
pacf(x) #cut off at lag 1: can suggest AR(1) model

est = arima(x, order = c (1, 0, 0)) #means AR(1) model p = 1
est

# Coefficients:
#         ar1       intercept
#       -0.8040     0.2255
# s.e.   0.1153     0.0913
# 
# sigma^2 estimated as 0.5181:  
# log likelihood = -22.32,  aic = 50.65

predict(est, n.ahead=5) #gives prediction and standard errors related to predictions

est$residuals # = prediction errors


########################################
## ARMA
########################################

y = c(-1.30, -0.18, 0.94, -0.26, -1.05, -0.78,
      -0.82, 0.43, 0.57, 1.41, -1.47, 0.49, 0.00, 
      -0.15, -0.64, 0.24, -0.79, 0.82, -0.20, -0.80, 
      -0.22, 0.88, -0.75, 0.55, 0.73, -0.82, 0.70, 
      -1.54, 0.04, -0.70, -0.58, -1.38, -1.28, 0.49, 
      -0.76, 1.08, 0.16, 1.11, -0.06, 0.88, 0.89, 0.31, 
      0.03,-1.19, -0.38, 0.49, 1.02, -0.98, 0.50, -0.57)
plot.ts(y)
acf(y)
pacf(y)

fit = arima(y, order = c(1,0,1)) #Try ARMA(1), but it might not be
fit

# Coefficients:
#         ar1     ma1       intercept
#       -0.7013  0.5768    -0.0946
# s.e.   0.3067  0.3377     0.1024
# 
# sigma^2 estimated as 0.6086:  
# log likelihood = -58.55,  aic = 125.11

# Can use arima.sim to generate any model you want
z = arima.sim(list(order = c(1,1,0), ar = 0.7), n = 200)
plot.ts(z)
acf(z)
pacf(z)
