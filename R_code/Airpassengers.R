library(forecast)

data("AirPassengers")
class(AirPassengers)

start(AirPassengers)

end(AirPassengers)

frequency(AirPassengers)

sum(is.na(AirPassengers))

AirPassengers

###### Explore
tsdata = ts(AirPassengers, frequency = 12)

ddata = decompose(tsdata, "multiplicative")

plot(ddata)

###############

plot(AirPassengers)

abline(reg = lm(AirPassengers~time(AirPassengers)))

cycle(AirPassengers)

# get boxplot by cycle

boxplot(AirPassengers~cycle(AirPassengers, xlab="Date"))

## Stationarity

plot(AirPassengers)

#ask R for the best model

mymodel = auto.arima(AirPassengers)
mymodel

# lets run with trace to compare the information criteria 
auto.arima(AirPassengers, ic = "aic", trace = TRUE)

## install.packages("tseries")
library(tseries)
adf.test(mymodel)

plot.ts(mymodel$residuals)

acf(ts(mymodel$residuals),main = 'ACF Residuals')
pacf(ts(mymodel$residuals),main = 'PACF Residuals')


# Use the model to forecast for the next 10 years 

myforecast = forecast(mymodel, level = c(95), h= 10*12)

plot(myforecast)


Box.test(mymodel$resid, lag=5,type = "Ljung-Box")
Box.test(mymodel$resid, lag=10,type = "Ljung-Box")
Box.test(mymodel$resid, lag=15,type = "Ljung-Box")

