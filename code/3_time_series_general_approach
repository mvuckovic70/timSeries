# few aspects of analysis:

# - time series can be conducted on a dataset as a whole
# - dataset can also be split in weeks, by categories, stores, manufacturers
# - note that various stores started selling in different times, periods are not equal
# - first part of the analysis will be done on a whole dataset
# - second part of the analysis will be broken down and deployed for each store respectively
# - results, forecasts and errors will be then compared

# metrics of accuracy is evaluated with RMSE (root mean squared error) - the less the better

# preparing dataset for time series analysis

library(lubridate)
library(zoo)
library(forecast)
library(UsingR)
library(lattice)
library(gridExtra)
library(timeSeries)

salesNew <- salesBetween[,c('monthYear','amount')]

salesNew$amount <- as.numeric(salesBetween$amount)
salesNew$monthYear <- as.yearmon(salesNew$date, "%m-%Y")

tsPrepare <- salesNew[c("monthYear", "amount")]
tsPrepare <- aggregate(tsPrepare[,2], by=list(tsPrepare$monthYear), FUN = sum)
names(tsPrepare) <- c("monthYear", "amount")
tsPrepare <- tsPrepare[order(tsPrepare$monthYear , decreasing = FALSE ),]

# check for nan's

sum(is.na(tsPrepare$monthYear))

# create time series object, subset, basic plotting

row.names(tsPrepare) <- tsPrepare$monthYear
tsPrepare$monthYear <- NULL

tsales <- ts(tsPrepare, start=c(2006, 1), end=c(2008, 12), frequency=12)
tsales.subset <- window(tsales, start=c(2006, 1), end=c(2008, 12), freuency=12)

plot(tsales)
plot(tsales.subset)

# simple moving average with plot

opar <- par(no.readonly=TRUE)
par(mar=c(1,1,1,1))
ylim <- c(min(tsales.subset), max(tsales.subset))
plot(tsales.subset, main="Raw time series")
plot(ma(tsales.subset, 3), main="Simple Moving Averages (k=3)", ylim=ylim)
plot(ma(tsales.subset, 8), main="Simple Moving Averages (k=8)", ylim=ylim)
plot(ma(tsales.subset, 15), main="Simple Moving Averages (k=15)", ylim=ylim)
par(opar)

# plot log(ts)

ltsales <- log(tsales.subset)
plot(ltsales)

# stl decomposition throws an error on

stl <- stl(ltsales, s.window = "period")

# so trying with numeric conversion

tstkr <- ts(as.numeric(ltsales), deltat=1/12)
fit <- stl(tstkr, s.window = "period")

# converting back to normal values

back <- exp(fit$time.series)

# seasonal decomposition visualization

# please note sharp sales fall in september 2008!

monthplot(tsales.subset, xlab="", ylab="")
seasonplot(tsales.subset, year.labels="TRUE", main="")

# simple exponential smoothing prediction with ets()
# ANN - simple model (level only)             -> ses(ts)
# AAN - double model (level and slope)        -> holt(ts)
# AAA - triple model (level, slope, seasonal) -> hw(ts)

fit_ets <- ets(tsales.subset, model="ANN")
forecast(fit_ets, 1)
accuracy(fit_ets)

# RMSE 39318
# accuracy is with bad results, because time series have both trend and seasonality

# exponentional smoothing holt-winters

fit_ets_aaa <- ets(log(tsales.subset), model="AAA")
fit_ets_aaa
accuracy(fit_ets_aaa) 

# prediction for 5 periods (months) and plot

pred <- forecast(fit_ets_aaa, 5)
pred
plot(pred, main="Forecast for sales", ylab="Log(tsales.subset)", xlab="Time")

# reversing to normal numbers, showing prediction in a form of table

pred$mean <- exp(pred$mean)
pred$lower <- exp(pred$lower)
pred$upper <- exp(pred$upper)
p <- cbind(pred$mean, pred$lower, pred$upper)
dimnames(p)[[2]] <- c("mean", "Lo 80", "Lo 95", "Hi 80", "Hi 95")
p

# Automatic exponential forecasting with ets()

fit_john <- ets(tsales.subset)
fit_john
accuracy(fit_john)
plot(forecast(fit_john), main="Johnson & Johnson Forecasts", ylab="Quarterly Earnings (Dollars)", xlab="Time", flty=2)

# ARIMA model, identifying parameters p,d,q, trying to make time series stationary

library(tseries)

plot(tsales.subset)
ndiffs(tsales.subset) 

# d = 1 (resulting in one differentiation )

dtsales.subset <- diff(tsales.subset)
plot(dtsales.subset)
adf.test(dtsales.subset)

Acf(dtsales.subset)
Pacf(dtsales.subset)

# fitting ARIMA

fit_arima <- arima(tsales.subset, order=c(0,0,1))
fit_arima
accuracy(fit_arima)

# fit evaluation of ARIMA model

qqnorm(fit_arima$residuals)
qqline(fit_arima$residuals)
Box.test(fit_arima$residuals, type="Ljung-Box")

# forecasting with ARIMA

forecast(fit_arima, 3)
plot(forecast(fit_arima, 3), xlab="Year", ylab="Sales")

# automated ARIMA forecast

fit_aa <- auto.arima(tsales.subset)
fit_aa
forecast(fit_aa, 3)
accuracy(fit_aa)
plot(forecast(fit_aa, 3), xlab="Year", ylab="Sales")
