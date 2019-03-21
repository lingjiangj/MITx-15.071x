setwd("~/文档/15.071x/Unit 4/Assignment 4")
data(state)
statedata = data.frame(state.x77)

#Problem 1.1
RegModel <- lm(Life.Exp~.,data =statedata)
summary(RegModel)

#Problem 1.2
prediction <- predict(RegModel)
sum(RegModel$residuals^2)

#Problem 1.3
RegModel2 <- lm(Life.Exp ~ Population+ Murder+ Frost+ HS.Grad,data = statedata )
summary(RegModel2)

#Problem 1.4
sum(RegModel2$residuals^2)

#Problem 2.1
library(rpart)
library(rpart.plot)
CARTModel <- rpart(Life.Exp~.,data = statedata)
prp(CARTModel)

#Problem 2.2
CARTPredict <- predict(CARTModel)
sum((statedata$Life.Exp-CARTPredict)^2)

#Problem 2.3
CARTModel2 <- rpart(Life.Exp~.,data = statedata, minbucket = 5)
prp(CARTModel2)

#Problem 2.5
CARTPredict2 <- predict(CARTModel2)
sum((CARTPredict2-statedata$Life.Exp)^2)

#Problem 2.6
CARTModel3 <- rpart(Life.Exp~Area,data = statedata, minbucket = 1)
CARTPredict3 <- predict(CARTModel3)
sum((statedata$Life.Exp-CARTPredict3)^2)

#Problem 3.1
library(caret)
set.seed(111)
fitControl = trainControl(method = "cv", number = 10)
cartGrid = expand.grid(.cp = seq(0.01, 0.5, 0.01) )
train(Life.Exp ~ ., data=statedata, method="rpart", trControl = fitControl, tuneGrid = cartGrid)

#Problem 3.2
CARTModel4 <- rpart(Life.Exp~., data = statedata, cp = 0.12)
prp(CARTModel4)

#Problem 3.3
CARTPredict4 <- predict(CARTModel4)
sum((statedata$Life.Exp-CARTPredict4)^2)

#Problem 3.5
set.seed(111)
train(Life.Exp ~ Area, data=statedata, method="rpart", trControl = fitControl, tuneGrid = cartGrid )
CARTmodel5 = rpart(Life.Exp ~ Area, data=statedata, cp=0.02)
prp(CARTmodel5)

#Problem 3.7
CARTPredict5 = predict(CARTmodel5)
sum((statedata$Life.Exp - CARTPredict5)^2)








































