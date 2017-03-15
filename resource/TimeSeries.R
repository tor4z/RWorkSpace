#Time Series R code
#Ryan Womack
#revised 2017-02-19

data(AirPassengers)
AP<-AirPassengers
class(AP)
start(AP)
end(AP)
frequency(AP)
summary(AP)
plot(AP)
cycle(AP)
aggregate(AP)
aggregate(AP, FUN=mean)
plot(aggregate(AP))
boxplot(AP~cycle(AP))

webdata<-"https://ryanwomack.com/data/UNRATE.csv"
webdata2<-"https://ryanwomack.com/data/CPIAUCSL.csv"
Unemployment<-read.csv(webdata, row.names=1)
Urate<-ts(Unemployment$VALUE, start=c(1948,1), freq=12)
Inflation<-read.csv(webdata2, row.names=1)
Irate<-ts((Inflation$VALUE), start=c(1948,1), freq=12)

Urate.July<-window(Urate, start=c(1980,7),freq=TRUE)
time(Urate)
plot(Urate)
abline(reg=lm(Urate~time(Urate)))
decompose(Urate)
plot(decompose(Urate))
plot(Irate,Urate)
ts.plot(Irate,Urate, col=c("blue","red"))

acf(Urate)
acf(AP)
acf(ts.intersect(Urate, AP))
ts.union(Urate,AP)

plot(HoltWinters(Urate, alpha=0.001, beta=1, gamma=0))
plot(HoltWinters(AP))
plot(HoltWinters(AP, alpha=0.1, beta=0.2, gamma=0))


AP.hw <- HoltWinters(AP)
plot(AP.hw)
AP.predict<-predict(AP.hw, n.ahead=10*12)
ts.plot(AP, AP.predict, lty=1:2)

UR.hw <- HoltWinters(Urate, seasonal="additive")
UR.predict<-predict(UR.hw, n.ahead=10*12)
ts.plot(Urate, UR.predict, lty=1:2)

#randomwalk
x<-w<-rnorm(1000)
for (t in 2:1000) x[t]<-x[t-1]+w[t]
plot(x, type="l")
acf(x)
acf(diff(x))

acf(diff(Urate))

#stationary autoregressive process
x<-w<-rnorm(1000)
for (t in 2:1000) x[t]<-(x[t-1]/2)+w[t]
plot(x, type="l")
acf(diff(x))

#partial autocorrelation
pacf(x) 
acf(x)

pacf(Urate)
acf(Urate)

#autoregression model
U.ar<-ar(Urate, method="mle")
x.ar<-ar(x, method="mle")
acf(U.ar$res[-1], na.action=na.pass)
acf(x.ar$res[-1])

#regression on time dimension
Urate.reg<-lm(Urate~time(Urate))
summary(Urate.reg)
acf(resid(Urate.reg))
pacf(resid(Urate.reg))

AP.reg<-lm(AP~time(AP))
summary(AP.reg)
confint(AP.reg)
acf(resid(AP.reg))
pacf(resid(AP.reg))

#see book on use of Generalized Least Squares
# and for seasonal time series

#ARIMA (Autoregressive integrated moving average)
#MA(3)
AP.ma <- arima(AP, order=c(0,0,3)) #estimates moving average of three time periods of "white noise"
#ARMA(1,1)
AP.arma <- arima(AP, order=c(1,0,1)) #1 prior period of AP, 1 prior period of white noise
#ARIMA(2,1,2)
# this model is composed of 2 prior periods of AP, 2 prior periods of white noise
# and a first-order difference
AP.arima <- arima(AP, order=c(2,1,2)) 

AP.predict<-predict(AP.arima, n.ahead=60)
ts.plot(AP,AP.predict$pred, lty=1:2)

#GARCH
#Generalized autoregressive conditioned heteroskedastic
library(tseries)
garch(AP, grad="numerical")

