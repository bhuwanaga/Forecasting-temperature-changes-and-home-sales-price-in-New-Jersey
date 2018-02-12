library(readr)

newark_atmp <- read_csv("C:/Users/bhuwa/Desktop/Courses Fall 2017/Business Forecasting/Final Exam Submission/newark_atmp.csv")
#Plot and Inference
temp_ts <- ts(newark_atmp$AvgTemp,start = c(2010,4),frequency = 12)
plot(temp_ts)


#Central Tendency
summary(temp_ts)

boxplot(temp_ts)
boxplot(temp_ts, main="Box plot ") 
boxplot(temp_ts , main="Box Plot - Temperature Changes in NJ" )
text(y = boxplot.stats(temp_ts)$stats, labels = boxplot.stats(temp_ts)$stats, x = 1.25)

#Decomposition

decomp_TempNJ <- decompose(temp_ts)
plot(decomp_TempNJ)
head(decomp_TempNJ)
?decompose
decomp_TempNJ$figure
decomp_TempNJ$x

?plot

library(fpp)
temp<- seasadj(decomp_TempNJ)
plot(temp, main="Time series adjusted for seasonality")
plot(temp_ts)
lines(temp, col="Red")
?lines


#Naïve Method
?naive
naive_forecast <- naive(temp_ts,5)
plot(naive_forecast)
?residuals
resi<- as.numeric(residuals(naive_forecast))
plot(resi, xlab="Year",ylab="Residuals" ,main="Residual Analysis")
?plot
hist(resi,xlab="Residuals",main = "Histogram of Residuals")
fit<- as.numeric(fitted.values(naive_forecast))
plot(fit)

plot(fit, resi, xlab="fitted values", ylab="Residuals", main="fitted values vs residuals")
plot(as.numeric(temp_ts))
plot(as.numeric(temp_ts), resi, xlab="Actual values", ylab="Residuals", main="Actual values vs residuals")
Acf(resi, main="Series Residuals", lag=80)
?Acf
accuracy(naive_forecast)

naive_forecast <- naive(temp_ts,12)
plot(naive_forecast)





#Simple Moving Averages
?ma
MA12_forecast <- ma(temp_ts, order=12)
lines(MA12_forecast,col="Orange")


MA3_forecast <- ma(temp_ts,order=3)
lines(MA3_forecast,col="Red")

MA6_forecast <- ma(temp_ts,order=6)
lines(MA6_forecast,col="Blue")

MA9_forecast <- ma(temp_ts,order=9)
lines(MA9_forecast,col="Green")

?meanf
mean_forecast <- meanf(NJ_Home_TS,12) 
plot(mean_forecast)

MA_forecast <- forecast(MA12_forecast)
plot(MA_forecast)
?ma
?HoltWinters
MA9_forecast <- ma(temp_ts, order=9)

MA_forecast
accuracy(MA_forecast)
#MA_forecast <- forecast(MA3_forecast)

plot(MA_forecast)

plot(temp_ts)

MA_forecast <- forecast(MA3_forecast, h=12)
accuracy(MA_forecast)
plot(MA_forecast)

MA_forecast <- forecast(MA6_forecast, h=12)
accuracy(MA_forecast)
plot(MA_forecast)

MA_forecast <- forecast(MA9_forecast, h=12)

accuracy(MA_forecast)
plot(MA_forecast)

MA21_forecast <- ma(temp_ts,order=3)
lines(MA21_forecast,col="Red")
MA_forecast <- forecast(MA21_forecast, h=12)

############################################################

#Simple Smoothing
install.packages("TTR")
library(TTR)
?sse
?HoltWinters
sse <- HoltWinters(temp_ts,beta=FALSE,gamma=FALSE)

sse_forecast <- forecast(sse, h=12)
summary(sse_forecast)
sd(sse_forecast$residuals, na.rm = TRUE)

resi<- residuals(sse_forecast)
plot(resi, xlab="Year",ylab="Residuals" ,main="Simple Smoothing - Residual Analysis")
hist(resi,xlab="Residuals",main = "Simple Smoothing - Histogram of Residuals")
fit<- fitted.values(sse_forecast)
plot(as.numeric(fit), as.numeric(resi), xlab="fitted values", ylab="Residuals", main="Simple Smoothing - fitted values vs residuals")
plot(as.numeric(temp_ts),as.numeric(resi), xlab="Actual values", ylab="Residuals", main="Simple Smoothing - Actual values vs residuals")
Acf(resi, main="Simple Smoothing - Residuals", lag=80)
accuracy(sse_forecast)
sse_forecast <- forecast(sse, h=12)
plot(sse_forecast)


##################################

#Holt-Winters 
SSE_Winters<- HoltWinters(temp_ts)
holt_forecast <- forecast(SSE_Winters, h=12)
summary(holt_forecast)
sd(holt_forecast$residuals, na.rm = TRUE)

resi<- residuals(holt_forecast)
plot(resi, xlab="Year",ylab="Residuals" ,main="Holt-Winters - Residual Analysis")
hist(resi,xlab="Residuals",main = "Holt-Winters - Histogram of Residuals")
fit<- fitted.values(holt_forecast)
plot(as.numeric(fit), as.numeric(resi), xlab="fitted values", ylab="Residuals", main="Holt-Winters - fitted values vs residuals")
plot(as.numeric(temp_ts), as.numeric(resi), xlab="Actual values", ylab="Residuals", main="Holt-Winters - Actual values vs residuals")
Acf(resi, main="Holt-Winters - Residuals", lag=150)
accuracy(holt_forecast)
holt_forecast <- forecast(SSE_Winters, h=12)
plot(holt_forecast)



#######################################################
adf.test(temp_ts, k=12)
kpss.test(temp_ts)



fit_Arima<- auto.arima(temp_ts)
fit_Arima
fit_Arima1<- auto.arima(temp_ts,trace =TRUE,stepwise = FALSE,approximation = FALSE)
fit_Arima1

a<-adf.test(temp_ts,k=12)
a



# NSDIFFS only works for seasonal data
nsdiffs(temp_ts)
# However NDIFFS works with non-seasonal data
ndiffs(temp_ts)


tsdisplay(temp_ts) 

Arima(temp_ts, order=c(4,0,2))

Arima(temp_ts, order=c(4,0,2), seasonal=c(0,0,1))

Arima(temp_ts, order=c(3,0,1), seasonal=c(0,0,1))

fit3 <- Arima(temp_ts, order=c(4,0,0), seasonal=c(0,0,1))
#fit1 <- Arima(temp_ts, order=c(3,0,1))
fit3
accuracy(fit3)
plot(fit3$residuals)
hist(fit3$residuals)
plot(as.numeric(fit3$fitted),as.numeric(fit3$residuals))
plot(as.numeric(temp_ts),as.numeric(fit3$residuals))
Acf(fit3$residuals)
accuracy(fit3)
fore_arima<-forecast(fit3,h=12)
fore_arima
plot(fore_arima)

fore_arima2<-forecast(fit3,h=24)
fore_arima2
plot(fore_arima2)

# Did it match what we picked?
auto.arima(temp_ts, stepwise=FALSE, approximation=FALSE)
auto.arima(temp_ts, stepwise=FALSE, approximation=FALSE,trace = TRUE)