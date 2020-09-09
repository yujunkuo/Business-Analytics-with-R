library(forecast)
library(fpp)
library(ggplot2)

#分析的資料為h02
class(h02)
plot(h02,ylab="scripts (million)",xlab="Time (Month)",main="Monthly Cortecosteroid Drug Sales In Australia From 1992 To 2008")

library(ggfortify)
library(changepoint)
autoplot(cpt.meanvar(ts(h02))) #identify changes in mean and var
#can sep into 2 parts to fit models

#--------------Decomposition-----------------

fit_b <- decompose(h02, type="mult")
plot(fit_b)

trend_b <- fit_b$trend
seasonal_b<- fit_b$seasonal

ggplot(h02,aes(x=c(1:204))) +
  geom_line(aes(y=h02)) +
  geom_line(aes(y=trend_b,colour='fit_b$trend')) +
  geom_line(aes(y=seasonal_b,colour='fit_b$seasonal'))

#------------------Forecast----------------------------

reg <- tslm(h02~season+trend)
summary(reg)
fcat <- forecast(reg,h=24)
plot(fcat)

h02 %>% stlf(method='naive') %>%
  autoplot() 

#----take out seasonal effect----

data <- decompose(h02,type='mult')
data2 <- h02/data$seasonal   #if additive, cafe-data$seasonal
plot(h02)
plot(data2)

library(TSA)
per = periodogram(h02) #快速傅立葉變換 做頻譜圖

#------------------Stationary-----------------------------

adf.test(h02) #stationary. 
#因為p-value為0.01，拒絕虛無假設，因此得知該筆資料目前為stationary(平穩)

#-----------------White noise----------------------------------

Box.test(h02,12,"Ljung") #not white noise
#因為p-value為2.2e-16，拒絕虛無假設，因此得知該筆資料並非white noise(白噪)

#-----------------ACF/PACF----------------------------------

#check if obs are random and independent in this interval
acf(h02) #not white noise #ma model
#因為大部分數據皆在interval(藍虛線)以外，故此data並非white noise
Pacf(h02) 

#--------------Fit ARIMA model---------------------

#呈現數據(包含ACF與PACF)
tsdisplay(h02)

#自動配適最佳模型
ma = auto.arima(h02) 

#est. coefficients
summary(ma)

#Coefficients:
#ar1     ar2     ar3      ar4      ma1     sma1     sma2
#0.0888  0.3386  0.2302  -0.2233  -0.9068  -0.4798  -0.1624
#s.e.  0.1063  0.0976  0.0894   0.0850   0.0853   0.0913   0.0930

#模型預測
predict(ma, 5) 
p=forecast(ma,5)
plot(p)

ma %>% forecast %>% autoplot

#---------------Model diagnostics-------------------------------

#繪製經過標準化後的residuals散佈圖
plot(rstandard(ma),ylab="Standardized Residuals",type='o');abline(h=0)
#大致看起來有white noise的感覺，但仍須作進一步檢測

#檢查Normality of residuals
qqnorm(ma$residuals)
qqline(ma$residuals)
#看出residuals為常態分配

#檢查Autocorrelation of residuals
acf(residuals(ma))$acf
#因為幾乎皆分佈於interval(藍色虛線)內，因此得知residuals彼此之間並沒有序列相關的特性

library(TSA)
LB.test(ma,lag=8) #residuals are not correlated 
Box.test(ma$resid,lag=1,type="Ljung") #residuals are not correlated (可以解釋為residuals類似white noise情形)
tsdiag(ma,gof=5,omit.initial=F) #gof is the maximum number of lags in the acf function used in the model diagnostics.

accuracy(ma)

#透過圖表觀察residuals分佈
checkresiduals(ma)

#-----------------Forecasting---------------------------------
## Average method
meanf(h02,36) #36 future 36 months
## Naïve method
naive(h02,36) 
rwf(h02,36)
## Seasonal naïve method
snaive(h02,36)
## Drift method
rwf(h02,36, drift=TRUE)

forecast <- predict(ma,n.ahead = 36)
forecast(ma,36)

