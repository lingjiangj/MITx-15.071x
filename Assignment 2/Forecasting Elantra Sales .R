setwd("~/文档/15.071x/Unit2/Assignment 2")
elantra <- read.csv("elantra.csv")

#Problem 1
training <- subset(elantra,Year <= 2012)
testing <- subset(elantra , Year > 2012)
str(training)

#Problem 2.1/2.2/2.3
SalesReg <- lm(ElantraSales ~ Unemployment+CPI_all+CPI_energy+Queries,data = training )
summary(SalesReg)

#Problem 3.1/3.2
MonthSalesReg <- lm(ElantraSales ~ Unemployment + Queries + CPI_energy + CPI_all + Month, data=training)
summary(MonthSalesReg)

#Problem3.3
Coe <- MonthSalesReg$coefficients["Month"]
abs(Coe*(1-3))
abs(Coe*(1-5))

#Problem 4.1
training$MonthFactor <- as.factor(training$Month)
testing$MonthFactor <- as.factor(testing$Month)
ElantraLM = lm(ElantraSales ~ Unemployment + Queries + CPI_energy + CPI_all + MonthFactor, data=training)
summary(ElantraLM)

#Problem5.1/5.2
cor(training[c("Unemployment","Month","Queries","CPI_energy","CPI_all")])

#Problem6.1
ElantraLM = lm(ElantraSales ~ Unemployment + CPI_energy + CPI_all + MonthFactor, data=training)

#Problem6.2
Pred <- predict(ElantraLM,newdata = testing)
SSE <- sum((Pred-testing$ElantraSales)^2)
SSE

#Problem6.3
baseline <- mean(training$ElantraSales)
baseline

#Problem6.4
SST <- sum((testing$ElantraSales - baseline)^2)
R2 <- 1-SSE/SST
R2

#Problem6.5
max(abs(Pred - testing$ElantraSales))

#Problem6.6
testing$Month[which.max(abs(Pred - testing$ElantraSales))]
