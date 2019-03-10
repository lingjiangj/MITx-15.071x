setwd("~/文档/15.071x/Unit2/Assignment 2")
pisaTrain <- read.csv("pisa2009train.csv")
pisaTest <- read.csv("pisa2009test.csv")
#1.1
str(train)

#1.2
tapply(train$readingScore,train$male,mean)

#1.3
summary(train)

#1.4
pisaTrain = na.omit(pisaTrain)
pisaTest = na.omit(pisaTest)
str(pisaTrain)
str(pisaTest)

#3.1
pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")
lmScore <- lm(readingScore ~.,data = pisaTrain)
summary(lmScore)

#3.2
SSE <- sum(lmScore$residuals^2)
RMSE <- sqrt(SSE/nrow(pisaTrain))
RMSE

#3.3/3.5
lmScore$coefficients["grade"]*(11-9)

#4.1
predTest <- predict(lmScore,newdata = pisaTest)
max(predTest) - min(predTest)

#4.2
SSE <- sum((predTest - pisaTest$readingScore)^2)
SSE
RMSE <- sqrt(SSE/nrow(pisaTest))
RMSE

#4.3
baseline <- mean(pisaTrain$readingScore)
baseline
SST <- sum((pisaTest$readingScore-baseline)^2)
SST

#4.4
R2 <- 1- SSE/SST
R2

