library(tuneR)
library(forecast)
library(tseries)
# x <- readWave("C:/Users/SVadde/Downloads/Yewno Data Science Assignment/visions22-1.wav")

x <- readWave("C:/Users/SVadde/Downloads/Yewno Data Science Assignment/visions22-1.wav", from = 1, to = 3,
         units = "seconds", header = FALSE, toWaveMC = NULL)
head(x)
plot(x)

#number of channels
nchannel(x)

# Extract the left and right channels 
# x_left<-mono(x, which = "left")
# periodogram(x_left, width=length(x_left), overlap=0)
# plot(periodogram(x_left, width=4096, overlap=0), xlim=c(0,5000))

al <- x@left

#A stationary time series is one whose properties do not depend on the time at which the series is observed.
# So time series with trends, or with seasonality, are not stationary — the trend and seasonality will affect 
# the value of the time series at different times. On the other hand, a white noise series is stationary — 
# it does not matter when you observe it, it should look much the same at any period of time.
#Unit root test to check if time series is stationary

head(al)
alt<-as.ts(al)
is.ts(alt)

#Augmented Dickey-Fuller Test
adf.test(al, alternative = "stationary") 
# The null-hypothesis for an ADF test is that the data are non-stationary. 
# So large p-values are indicative of non-stationarity, and small p-values 
# suggest stationarity. Using the usual 5% threshold, differencing is 
# required if the p-value is greater than 0.05.

#Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test. 
kpss.test(al) 
#This reverses the hypotheses, so the null-hypothesis is that the data 
# are stationary. In this case, small p-values (e.g., less than 0.05) 
# suggest that differencing is required. 

# check ndiffs; if value = 0 then series is stationary
ndiffs(al)

### Series is stationary for t=1 to t=3

Acf(alt)
pacf(alt)

fit <- auto.arima(alt)
summary(fit)
predict <-forecast(fit,h=10)
plot(predict)
accuracy(predict)
# examine the ACF of forecast error to see if there is still some seasonality or trend that was not captured by the forecast method
# examine the ACF critical values. 

