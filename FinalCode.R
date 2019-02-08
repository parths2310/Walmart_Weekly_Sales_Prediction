#install.packages("forecast")
#install.packages("plyr")
#install.packages("reshape")
#install.packages("lubridate")
#install.packages("ggplot2")
#install.packages("gridExtra")
#install.packages("lattice")
#install.packages("tseries")
#install.packages("stats")
library(stats)
library(tseries)
library(lattice)
library(forecast)
library(plyr)
library(reshape)
library(lubridate)
library(ggplot2)

waldata <- read.csv("./walmartdata.csv", row.names = 1)
head(waldata)
summary(waldata)
str(waldata)

train <- waldata

train$Year <- as.factor(train$Year)
train$Date <- as.Date(train$Date,format= "%d-%m-%y")
train$Store <- as.factor(train$Store)
train$Size <- as.factor(train$Size)
train$Dept <- as.factor(train$Dept)
train$Weeknum <- as.factor(train$Weeknum)
head(train)
train$Weekly_Sales <- train$Weekly_Sales/1000000 # change the sales into millions

##############################Graphs for Sales per store
mainDir <- "E:/MIT Syllabus/Business Forecasting/Project Report/Walmart Data/Charts/"
for (i in 1:45){
  stOne <- train[train$Store ==i,]
  Dsale <- ddply(stOne,~Dept,summarise,total=sum(Weekly_Sales))
  Dsale$Dept <- factor(Dsale$Dept, levels=Dsale[order(-Dsale$total), "Dept"])
  dChart <- ggplot(data=Dsale, aes(x=Dept, y=total+1)) +
    geom_point(colour="red")+
    ggtitle(paste("Store",i, "Total Sale by Dept")) +
    ylab("Weekly Sale(millions)")+
    theme_bw()
  subDir <- paste("Store",i)
  if (dir.exists(file.path(mainDir, subDir))) {
    setwd(file.path(mainDir, subDir))
  }else {
    dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
    setwd(file.path(mainDir, subDir))
  }
  ps <- paste("Stores",i,".pdf")
  ggsave(filename=ps, plot=dChart)
  #dev.off()
}

##########Preparing time series of weekly sales

dtsale <- ddply(train,~Date,summarise,total=sum(Weekly_Sales)) #weekly sale by date

tsSales<- ts(dtsale$total,start=c(2010,5), end=c(2012,43),frequency=52) # weekly sale time series
plot(tsSales, ylab = "Weekly Sale(millions)", main = "Weekly sale time series")
#########Check decomposition for Seasonality and trend

tsdisplay(tsSales, main = "Weekly Sale")
plot(decompose(tsSales))
Acf(tsSales,plot=TRUE, main="")
pacf(tsSales, main="")

########ADF and KPSS Test to check if Time Series is Stationary
adf.test(tsSales)
kpss.test(tsSales)
#######################
trainData <- window(tsSales,start=c(2010,5), end=c(2012,14),frequency=52) #80% of the time series used for training
testData <- window(tsSales,start=c(2012,15), end=c(2012,43),frequency=52) #20% of the time series used for testing


adf.test(trainData)
kpss.test(trainData)

par(mfrow = c(1,2))
plot(trainData, ylab = "Weekly Sale(millions)", main = "Weekly sale time series(Training Data)")
plot(testData, ylab = "Weekly Sale(millions)", main = "Weekly sale time series(Test Data)")
par(mfrow = c(1,1))

seqplot.ts(trainData,testData,main = "Training and Testing time series", ylab = "Weekly Sale(millions)") # ploting train and test data
######Check the times of differencing needed
nsdiffs(trainData)

ndiffs(trainData)

###########forecast using mean
mf = meanf(trainData,h=length(testData),level=c(90,95),fan=FALSE,lambda=NULL)
plot(mf, ylab = "Weekly Sale(millions)") 

############forecast using Naive Method
mn = naive(trainData,h=length(testData),level=c(90,95),fan=FALSE,lambda=NULL) 
plot(mn, ylab = "Weekly Sale(millions)") 

#############Holt-Winters model
HoltWintersFit <- HoltWinters(trainData, gamma=TRUE)
#Gamma is a parameter used for the seasonal component.
#If set to FALSE, a non-seasonal model is fitted and if set to TRUE, a seasonal model is fitted.
plot(HoltWintersFit)
fc1 <- forecast.HoltWinters(HoltWintersFit, h=length(testData))
plot.forecast(fc1, ylab = "Weekly Sale(millions)")
summary(HoltWintersFit)
################Arima Model
auto.arima(trainData)
fc2 <- forecast(auto.arima(trainData),h=length(testData))
plot(fc2, ylab = "Weekly Sale(millions)")

###########Linear regression model
tsfit <- tslm(trainData ~ trend + season)
fc3 <- forecast(tsfit, h=length(testData))
plot(fc3, ylab = "Weekly Sale(millions)")
summary(tsfit)
plot(tsfit)

##################measuring model's accuracy

accuracy(mf,testData)
accuracy(mn,testData)
accuracy(fc1,testData)
accuracy(fc2,testData)
accuracy(fc3,testData)





