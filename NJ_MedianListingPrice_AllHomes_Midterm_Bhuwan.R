library(readr)

NJ_MedianListingPrice_AllHomes <- read_csv("C:/Users/bhuwa/Desktop/Courses Fall 2017/Business Forecasting/Midterm Submission/NJ_MedianListingPrice_AllHomes.csv")

NJ_Home_Raw <- NJ_MedianListingPrice_AllHomes$Value

NJ_Home_TS <- ts(NJ_Home_Raw,frequency = 12, start = c(1996,4))
#Plot and Inference
plot(NJ_Home_TS)

#Central Tendency
summary(NJ_Home_TS)

boxplot(NJ_Home_TS)
boxplot(NJ_Home_TS, main="Box plot ") #left skewed more variability between 1st quartile and median
boxplot(NJ_Home_TS , main="Box Plot - Median home prices for House Listing in NJ")
text(y = boxplot.stats(NJ_Home_TS)$stats, labels = boxplot.stats(NJ_Home_TS)$stats, x = 1.25)

#Decomposition
decomp_NJ <- decompose(NJ_Home_TS)
plot(decomp_NJ)
head(decomp_NJ)
?decompose
decomp_NJ$figure
decomp_NJ$x

?plot

library(fpp)
temp<- seasadj(decomp_NJ)
plot(temp, main="Time series adjusted for seasonality")
plot(NJ_Home_TS)
lines(temp, col="Red")
?lines

#Naïve Method
?naive
naive_forecast <- naive(NJ_Home_TS,5)
plot(naive_forecast)
?residuals
resi<- residuals(naive_forecast)
plot(resi, xlab="Year",ylab="Residuals" ,main="Residual Analysis")
?plot
hist(resi,xlab="Residuals",main = "Histogram of Residuals")
fit<- fitted.values(naive_forecast)
plot(fit)

plot(fit, resi, xlab="fitted values", ylab="Residuals", main="fitted values vs residuals")

plot(NJ_Home_TS, resi, xlab="Actual values", ylab="Residuals", main="Actual values vs residuals")
Acf(resi, main="Series Residuals", lag=80)
?Acf
accuracy(naive_forecast)

naive_forecast <- naive(NJ_Home_TS,12)
plot(naive_forecast)


#Simple Moving Averages
?ma
MA12_forecast <- ma(NJ_Home_TS, order=12)
plot(MA12_forecast)

MA3_forecast <- ma(NJ_Home_TS,order=3)
lines(MA3_forecast,col="Red")

MA6_forecast <- ma(NJ_Home_TS,order=6)
lines(MA6_forecast,col="Blue")

MA9_forecast <- ma(NJ_Home_TS,order=9)
lines(MA9_forecast,col="Green")

?meanf
mean_forecast <- meanf(NJ_Home_TS,12) 
plot(mean_forecast)

MA_forecast <- forecast(MA12_forecast)
plot(MA_forecast)
?ma
?HoltWinters
MA9_forecast <- ma(NJ_Home_TS, order=9)

MA12_forecast <- ma(NJ_Home_TS, order=12)
MA_forecast <- forecast(MA12_forecast, h=12)
MA_forecast
accuracy(MA_forecast)
#MA_forecast <- forecast(MA3_forecast)
#plot(MA_forecast)

MA_forecast <- forecast(MA9_forecast, h=12)
accuracy(MA_forecast)
plot(MA_forecast)

#Simple Smoothing
install.packages("TTR")
library(TTR)
?sse
?HoltWinters
sse <- HoltWinters(NJ_Home_TS,beta=FALSE,gamma=FALSE)

sse_forecast <- forecast(sse, h=12)
summary(sse_forecast)
sd(sse_forecast$residuals, na.rm = TRUE)

resi<- residuals(sse_forecast)
plot(resi, xlab="Year",ylab="Residuals" ,main="Simple Smoothing - Residual Analysis")
hist(resi,xlab="Residuals",main = "Simple Smoothing - Histogram of Residuals")
fit<- fitted.values(sse_forecast)
plot(fit, resi, xlab="fitted values", ylab="Residuals", main="Simple Smoothing - fitted values vs residuals")
plot(NJ_Home_TS, resi, xlab="Actual values", ylab="Residuals", main="Simple Smoothing - Actual values vs residuals")
Acf(resi, main="Simple Smoothing - Residuals", lag=80)
accuracy(sse_forecast)
sse_forecast <- forecast(sse, h=12)
plot(sse_forecast)




#attributes(SSE_Simple)
#plot(SSE_Simple)
#SSE_Simple$SSE
#SSE_Simple$fitted

#ets(NJ_Home_TS)
#ets_forecast <- ets(NJ_Home_TS)
#plot(ets_forecast)
#ets_forecast$model
?sse
?ses
?holt
#Holt-Winters 
SSE_Winters<- HoltWinters(NJ_Home_TS)

holt_forecast <- forecast(SSE_Winters, h=12)
summary(holt_forecast)
sd(sse_forecast$residuals, na.rm = TRUE)

resi<- residuals(holt_forecast)
plot(resi, xlab="Year",ylab="Residuals" ,main="Holt-Winters - Residual Analysis")
hist(resi,xlab="Residuals",main = "Holt-Winters - Histogram of Residuals")
fit<- fitted.values(holt_forecast)
plot(fit, resi, xlab="fitted values", ylab="Residuals", main="Holt-Winters - fitted values vs residuals")
plot(NJ_Home_TS, resi, xlab="Actual values", ylab="Residuals", main="Holt-Winters - Actual values vs residuals")
Acf(resi, main="Holt-Winters - Residuals", lag=150)
accuracy(holt_forecast)
holt_forecast <- forecast(SSE_Winters, h=12)
plot(holt_forecast)
