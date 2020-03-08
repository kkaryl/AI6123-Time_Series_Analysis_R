x1 = c(-0.06, -0.18, 0.06, 0.15, 0.13, -0.02,
      0.19, -0.13, -0.26, -0.29, -0.17, -0.10, 
      0.10, 0.17, 0.04, 0.00, 0.15, 0.11, 0.01, 0.19)
acf(x1) #cut off after lag 5
plot.ts(x1)
pacf(x1) #cut off after lag 1 (Can suggest AR(1) Model)

ab1 = acf(x)
ab1 #acf at lag 1 is 0.476 (notes: r1= 0.4755)

# look at mean to decide if it is a centralized data
mean(x1) #0.0045 is very low (centralized data)


x2 = c(5.05, 5.02, 4.78, 4.73, 4.86, 4.81,
      4.86, 4.74, 4.89, 5.03, 5.13, 5.16, 5.19, 5.13, 
      5.16, 5.10, 5.04, 5.07, 4.95, 4.91)

length(x2)
acf(x2) # cut off after lag 8
pacf(x2) # cut off after lag 1 (suggest AR(1) model)
mean(x2) #4.9805 (notes is 4.98)

ab2 = acf(x2)
ab2 # 0.775 at lag1


x3 = c(0.15, -0.06, -0.39, -0.56,
       -0.52, -0.26, -0.11, 0.32, 0.31, 0.01, 0.00, 0.17, 
       0.52, 0.32, -0.08, -0.30, -0.16, 0.32, 0.29,
       0.07)
plot.ts(x3)
mean(x3) #low mean
acf(x3) #Stationary. Cut off after lag 1
pacf(x3) #cut off after lag 2 (Can suggest MA1 or AR(2))
ab3 = acf(x3) #This example is AR(2)
ab3 #r1 = 0.637 => 0.64, r2 = 0.045 => 0.041


x4 = c(1.25, 1.64, 1.78, 1.33, 1.21, 1.04, 1.04, 1.55, 1.31, 0.89, 0.78,
       1.28, 1.79, 2.42, 2.09, 1.57, 1.05, 0.97, 1.26, 1.70)
mean(x4) #1.3975 
plot.ts(x4)
acf(x4) #cut off after lag 4
pacf(x4) # cut ff after lag 2 (MA4 or AR2 (notes))
ab4 = acf(x4)
ab4 

A <- matrix(data=c(1, 0.57, 0.57, 1), nrow=2, ncol=2, byrow=TRUE)    
b <- matrix(data=c(0.57, -0.11), nrow=2, ncol=1, byrow=FALSE)
round(solve(A, b), 3)

### Example 7
# generate AR(2) model data and store into ar.sim
ar.sim<-arima.sim(list(order=c(2,0,0), 
                       ar=c(0.7, -0.5)), n=500)
arima(ar.sim, order=c(2,0,0)) #estimated using LS approach if r1 and r2 is the same as 0.7, -0.5
?ar.yw
#use yule-walker method to estimate unknown estimate
# order.max = 2 doesnt mean AR2 just 
ts.yw <- ar.yw(ar.sim, order.max=2) 
ts.yw
# sigma^2 is the variance of Zt
acf(ar.sim) #acf not cut off
pacf(ar.sim) # cut off after lag 2


### Example 8
lh #built in dataset with 48 obs
plot.ts(lh)
mean(lh)
acf(lh) #cut off after lag 1
pacf(lh) #cut off after lag 1 ()
ts.yw <- ar.yw(lh, order.max = 5)
ts.yw # recommends order 3 even thou acf and pacf cuts off after lag 1
summary(ts.yw)

ar1 = arima(lh, order=c(1,0,0))
ar1

### Example 9 (MA(1))
x5 = c(-0.89, -0.53, 0.54, -0.26, -1.34, -1.97, -0.35, 0.46, -0.08, -1.13, 0.04, 1.64, 1.95, 
       0.94, -0.11, 0.18, 0.72, 0.91, -1.09, 0.12, 1.29, 0.79, 1.67, -0.60, -1.72, -0.76, -2.60, -1.71, -0.39
       -1.18)
mean(x5)
plot.ts(x5)
ab5 = acf(x5) # Cut off after lag 1
ab5
ma1 = arima(x5, order=c(0,0,1))
ma1


