setwd("~/文档/15.071x/Unit2/Assignment 2")
FluTrain <- read.csv("FluTrain.csv")

#1.1
str(FluTrain)
FluTrain$Week[which.max(FluTrain$ILI)]
FluTrain$Week[which.max(FluTrain$Queries)]

#1.2
hist(FluTrain$ILI)

#1.3/2.1
plot(log(FluTrain$ILI),FluTrain$Queries)

#2.2
FluTrend1 <- lm(log(ILI) ~ Queries, data = FluTrain)
summary(FluTrend1)

#2.3
cor(log(FluTrain$ILI),FluTrain$Queries)^2

#3.1
FluTest <- read.csv("FluTest.csv")
PredTest1 = exp(predict(FluTrend1, newdata = FluTest))
PredTest1[which(FluTest$Week == "2012-03-11 - 2012-03-17")]

#3.2
Obs_ILI <- FluTest$ILI[which(FluTest$Week == "2012-03-11 - 2012-03-17")]
Est_ILI <- PredTest1[which(FluTest$Week == "2012-03-11 - 2012-03-17")]
(Obs_ILI-Est_ILI)/Obs_ILI

#3.3
SSE <- sum((PredTest1 - FluTest$ILI)^2)
RMSE <- sqrt(SSE/nrow(FluTest))
RMSE

#4.1
install.packages("zoo")
library(zoo)
ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)
FluTrain$ILILag2 = coredata(ILILag2)

#4.2
plot(log(FluTrain$ILILag2),log(FluTrain$ILI))

#4.3
FluTrend2 <- lm(log(ILI) ~ Queries + log(ILILag2), data = FluTrain)
summary(FluTrend2)

#4.4
summary(FluTrend1)

#5.1
ILILag2 = lag(zoo(FluTest$ILI), -2, na.pad=TRUE)
FluTest$ILILag2 <- coredata(ILILag2)
summary(FluTest$ILILag2)

#5.3
FluTest$ILILag2[1] = FluTrain$ILI[416]
FluTest$ILILag2[2] = FluTrain$ILI[417]
FluTest$ILILag2[1]
FluTest$ILILag2[2]

#5.4
PredTest2 <- exp(predict(FluTrend2,newdata = FluTest))
SSE <- sum((PredTest2 - FluTest$ILI)^2)
SSE
RMSE <- sqrt(SSE/nrow(FluTest))
RMSE


