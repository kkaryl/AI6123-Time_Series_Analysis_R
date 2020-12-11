u = seq(1,100,by =1)
ts.plot(u) # data is not stationary as mean is not constant
acf(u)  # acf is not cut off
pacf(u) # pacf is cut off (pacf is no use when determining whether data is stationary or not)


