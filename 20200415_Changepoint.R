## Changes in mean

library(changepoint)
set.seed(10)
mean.data <- c(rnorm(100, 0, 1), rnorm(100, 1, 1), rnorm(100, 0, 1),
               + rnorm(100, 0.3, 1))
ts.plot(mean.data, xlab = "Index")

m.pelt <- cpt.mean(mean.data, method = "PELT")
plot(m.pelt, type = "l", cpt.col = "blue", xlab = "Index",cpt.width = 4)
cpts(m.pelt)

m.binseg <- cpt.mean(mean.data, method = "BinSeg")
plot(m.binseg, type = "l", xlab = "Index", cpt.width = 4)
cpts(m.binseg)

m.pm <- cpt.mean(mean.data, penalty = "Manual", pen.value = "1.5 * log(n)",
                 method = "PELT")
plot(m.pm, type = "l", cpt.col = "blue", xlab = "Index", cpt.width = 4)
cpts(m.pm)

# cpt.mean(data, penalty = "SIC", pen.value = 0, method = "AMOC", Q = 5,
#          test.stat = "Normal", class = TRUE, param.estimates = TRUE)
data("Lai2005fig4", package = "changepoint")
Lai.default <- cpt.mean(Lai2005fig4[, 5], method = "PELT")
plot(Lai.default, pch = 20, col = "grey", cpt.col = "black", type = "p",
     xlab = "Index")
cpts(Lai.default)
coef(Lai.default)

# gen data with 1 change point
set.seed(18)
mean.data2 <- c(rnorm(100, 0, 1), rnorm(100, 0.3, 1))
ts.plot(mean.data2, xlab = "Index")

m.pelt2 <- cpt.mean(mean.data2, method = "PELT")
plot(m.pelt2, type = "l", cpt.col = "blue", xlab = "Index",cpt.width = 4)
cpts(m.pelt2) # no change point

m.binseg2 <- cpt.mean(mean.data2, method = "BinSeg")
plot(m.binseg2, type = "l", xlab = "Index", cpt.width = 4)
cpts(m.binseg2) # no change point

m.pm2 <- cpt.mean(mean.data2, penalty = "Manual", pen.value = "1.5 * log(n)",
                 method = "PELT") # 1.5 or 2 penalty
plot(m.pm2, type = "l", cpt.col = "blue", xlab = "Index", cpt.width = 4)
cpts(m.pm2) # 1 change point


## Changes in variance
set.seed(10)
library(gstat)
data("wind", package = "gstat")
ts.plot(wind[, 11], xlab = "Index")

wind.pelt <- cpt.var(diff(wind[, 11]), method = "PELT")
plot(wind.pelt, xlab = "Index")
cpts(wind.pelt) # cp value
ncpts(wind.pelt) # count number of change points

logLik(wind.pelt)
wind.bs <- cpt.var(diff(wind[, 11]), method = "BinSeg")
cpts(wind.bs)
ncpts(wind.bs)

# use this if both mean and variance changepoints
#cpt.meanvar











