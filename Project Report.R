# STA 567 Project Report
# Cameron Carter, Maria Dodson, Nazim Pacheco

require(TSA)
require(utils)
require(dplyr)
library(car)
library(MASS)
library(plyr)
library(lattice)
library(ppcor)
library(stats)
library(forecast)
# Import the "ma_lga_12345.csv"
data <- read.csv("https://raw.githubusercontent.com/JCameronCarter/TimeSeriesGroupProject/refs/heads/main/ma_lga_12345.csv")
house_ts <- data[data$bedrooms == 4,]
house4_ts <- ts(house_ts[,-c(1,3,4)])
options(scipen=10)
par(mfrow=c(1,2))
plot(house4_ts, type='o', ylab="Price ($)",
     main='4 Bedroom House Prices from 2007 - 2019',xlab ='Time (Fiscal Quarter)')
acf(house4_ts,lag.max=25)

par(mfrow=c(1,2))
plot((diff(house4_ts)), type='o', ylab="Price ($)",
     main='1st diff 4 Bedroom House Prices from 2007 - 2019',xlab ='Time (Fiscal Quarter)')
acf(diff(house4_ts,lag.max=25))

par(mfrow=c(1,2))
plot(diff(diff(house4_ts)), type='o', ylab="Price ($)",
     main='2nd diff 4 Bedroom House Prices from 2007 - 2019',xlab ='Time (Fiscal Quarter)')
acf(diff(diff(house4_ts,lag.max=25)))

par(mfrow=c(1,2))
acf(diff(diff(house4_ts)));pacf(diff(diff(house4_ts)))
eacf(diff(diff(house4_ts)))

m1<-Arima(house4_ts,order=c(0,2,1),include.mean=F, method='ML') # Parameter estimation of ARIMA (0,2,1)
m2<-Arima(house4_ts,order=c(1,2,0),include.mean=F, method='ML') # Parameter estimation of ARIMA (1,2,0)
data.frame(Model=c("ARIMA(1,2,0)","ARIMA(0,2,1)"), AIC=c(AIC(m1),AIC(m2)),
           BIC=c(BIC(m1),BIC(m2)))

par(mfrow=c(1,1))
ts.plot(rstandard(m1),ylab="Standardized Residuals",main="ARIMA(0,2,1)",type="o")
abline(h=0)
tsdiag(m1,gof.lag=15,omit.initial=F)

par(mfrow=c(1,1))
ts.plot(rstandard(m2),ylab="Standardized Residuals",main="ARIMA(1,2,0)",type="o")
abline(h=0)
tsdiag(m2,gof.lag=15,omit.initial=F) 

par(mfrow=c(1,1))
summary(m1)
qqnorm(rstandard(m1))
shapiro.test(rstandard(m1))# formal test for normality

par(mfrow=c(1,1))
summary(m2)
qqnorm(rstandard(m2))
shapiro.test(rstandard(m2))# formal test for normality

# Picking ARIMA(0,2,1), (m1) because it fits the best

#-------------Making 1st 10 prediction 
m1.pr<-predict(m1,n.ahead=10) # Forecasting TS at 10 future time points
forecast=m1.pr$pred;se=m1.pr$se
l<-forecast-1.96*se
u<-forecast+1.96*se
data.frame(forecast,se,l,u)

#---------------------Constructing 95% upper and lower bound around the forecast
# plotting forecast values with given CI along with the data
par(mfrow=c(1,1))
t<-1:70 # Only showing the partial series
plot(t,house4_ts[t],type="o",ylab="House Price")
lines(forecast,col="red",type="o")
lines(u,col="blue",lty="dashed")
lines(l,col="blue",lty="dashed")

#--------Making and plotting predictions 10 timesteps in the past 

ts_len <- length(house4_ts)
m1_trimmed1 <-Arima(house4_ts[0:(ts_len-10)],order=c(0,2,1),include.mean=F, method='ML')

m1_trimmed1.pr<-predict(m1_trimmed1,n.ahead=10) # Forecasting TS at 10 future time points
forecast_t1 <- m1_trimmed1.pr$pred;se_t1=m1_trimmed1.pr$se
l_t1<-forecast_t1 - 1.96*se_t1
u_t1<-forecast_t1 + 1.96*se_t1
data.frame(forecast_t1,se_t1,l_t1,u_t1)

plot(house4_ts, ylim=c(550000,950000), type="o", ylab="House Price", main="10 timesteps in the past")
mtext(expression(Y[41]),side = 3, line = 0.25)
lines(forecast_t1,col="red",type="o")
lines(u_t1,col="blue",lty="dashed")
lines(l_t1,col="blue",lty="dashed")

#--------Making and plotting predictions 6 timesteps in the past 

m1_trimmed2 <-Arima(house4_ts[0:(ts_len-5)],order=c(0,2,1),include.mean=F, method='ML') 
m1_trimmed2.pr<-predict(m1_trimmed2,n.ahead=10) 
forecast_t2 <- m1_trimmed2.pr$pred;se_t2=m1_trimmed2.pr$se
l_t2<-forecast_t2 - 1.96*se_t2
u_t2<-forecast_t2 + 1.96*se_t2
data.frame(forecast_t2,se_t2,l_t2,u_t2)

plot(house4_ts, ylim=c(550000,950000), xlim=c(0,55),type="o", ylab="House Price", main="5 timesteps in the past")
mtext(expression(Y[46]),side = 3, line = 0.25)
lines(forecast_t2,col="red",type="o")
lines(u_t2,col="blue",lty="dashed")
lines(l_t2,col="blue",lty="dashed")
