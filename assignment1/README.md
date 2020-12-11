# AI6123: Assignment 1

## Problem Overview

The wwwusage time series data consists of the number of users connected to the internet through a server. The data are collected at a time interval of one minute and there are
100 observations. Please fit an appropriate ARIMA model for it and submit a short report including R codes, the fitted model, the diagnostic checking, AIC, etc.

## General Steps

1. Check stationarity
2. Differencing
3. ACF, PACF plots
4. Diagnostic testing

## ARIMA Models Trialed

1. ARIMA(1,1,1) - from auto-arima
2. ARIMA(3,1,0) - after one-time differencing
3. ARIMA(2,2,0) - after two-time differencing
4. ARIMA(5,2,5) - brute force lowest AIC

