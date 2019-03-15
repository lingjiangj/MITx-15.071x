setwd("~/文档/15.071x/Unit3/Assignment 3")

#Problem 1.1
parole <- read.csv("parole.csv")
str(parole)

#Problem 1.2
table(parole$violator)

#Problem 2.2
parole$state <- as.factor(parole$state)
parole$crime <- as.factor(parole$crime)
summary(parole$state)
summary(parole$crime)
table(parole$state)
table(parole$crime)

#Problem 3.1
set.seed(144)
library(caTools)
split = sample.split(parole$violator,SplitRatio = 0.7)
train = subset(parole,split == TRUE)
test = subset(parole,split == FALSE)

#Problem 4.1
mod <- glm(violator ~.,data = train, family = "binomial")
summary(mod)

#Problem 4.2
exp(1.61)

#Problem 4.3
Log = -4.2411574 + 0.3869904*1 + 0.8867192*1 - 0.0001756*50 + 0.4433007*0 + 0.8349797*0 - 3.3967878*0 - 0.1238867*3 + 0.0802954*12 + 1.6119919*0 + 0.6837143*1 - 0.2781054*0 - 0.0117627*0 
exp(Log)
1/(1+exp(-Log))

#Problem 5.1
testPrediction <- predict(mod,newdata = test, type = "response")
summary(testPrediction)

#Problem 5.2
table(test$violator,testPrediction >=0.5)
12/(11+12)
167/(167+12)
(12+167)/(12+11+167+12)

#Problem 5.3
table(test$violator)
179/(179+23)

#Problem 5.6
library(ROCR)
pred <- prediction(testPrediction,test$violator)
as.numeric(performance(pred, "auc")@y.values)







