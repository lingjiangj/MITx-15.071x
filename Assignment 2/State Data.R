setwd("~/文档/15.071x/Unit2/Assignment 2")
statedata <- read.csv("statedata.csv")

#1.1
str(statedata)
plot(statedata$x,statedata$y)

#1.2
which.max(tapply(statedata$HS.Grad,statedata$state.region,mean))

#1.3
boxplot(statedata$Murder~statedata$state.region)

#1.4
max(subset(statedata,state.region == "Northeast")["Murder"])

#2.1/2.2
LifeReg <- lm(Life.Exp ~ Population + Income + Illiteracy + Murder + HS.Grad + Frost + Area, data=statedata)
summary(LifeReg)

#2.3
plot(statedata$Income, statedata$Life.Exp)

#3.2
LifeReg <- lm(Life.Exp ~ Population + Murder + HS.Grad + Frost, data=statedata)
summary(LifeReg)

#3.3
statedata$state.name[which.min(predict(LifeReg))]
statedata$state.name[which.min(statedata$Life.Exp)]

#3.4
statedata$state.name[which.max(predict(LifeReg))]
statedata$state.name[which.max(statedata$Life.Exp)]

#3.5
statedata$state.name[which.min(abs(LifeReg$residuals))]
statedata$state.name[which.max(abs(LifeReg$residuals))]
