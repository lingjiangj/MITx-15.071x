setwd("~/文档/15.071x/Unit 4/Assignment 4")

#Problem 1.1
census <- read.csv("census.csv")
library(caTools)
set.seed(2000)
split <- sample.split(census$over50k,SplitRatio = 0.6)
train <- subset(census,split == TRUE)
test <- subset(census,split==FALSE)
censusglm <- glm(over50k~.,data = train,family = binomial)
summary(censusglm)

#Problem 1.2
censusPred <- predict(censusglm,newdata = test,type = "response")
table(test$over50k,censusPred >=0.5)
(9051+1888)/(9051+1888+662+1190)

#Problem 1.3
table(test$over50k)
9713/(9713+3078)

#Problem 1.4
library(ROCR)
ROCRpred = prediction(censusPred,test$over50k)
as.numeric(performance(ROCRpred,"auc")@y.values)

#Problem 2.1/2.2/2.3
library(rpart)
library(rpart.plot)
censusCART <- rpart(over50k~.,data = train, method = "class")
prp(censusCART)

#Problem 2.4
CARTpred <- predict(censusCART,newdata = test, type = "class")
table(test$over50k,CARTpred)
(9243+1596)/(9243+470+1482+1596)

#Problem 2.5
library(ROCR)
predictTest <- predict(censusCART,newdata = test)
predictTest <- predictTest[,2]
ROCRpredict <- prediction(predictTest,test$over50k)
as.numeric(performance(ROCRpredict,"auc")@y.values)

#Problem 3.1
set.seed(1)
trainSmall = train[sample(nrow(train), 2000), ]
set.seed(1)
censusrf = randomForest(over50k ~ . , data = trainSmall)
predictTest = predict(censusrf, newdata=test)
table(test$over50k, predictTest)
(9614+1050)/nrow(test)

#Problem 3.2
vu = varUsed(censusrf, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(censusrf$forest$xlevels[vusorted$ix]))

#Problem 3.3
varImpPlot(censusrf)

#Problem 4.1
library(caret)
set.seed(2)
fitControl = trainControl( method = "cv", number = 10 )
cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002))
train(over50k~.,data = train, method = "rpart",trControl = fitControl,tuneGrid = cartGrid)

#Problem 4.2
model <- rpart(over50k~., data = train, method = "class", cp = 0.002)
predict <- predict(model,newdata = test, type = "class")
table(test$over50k,predict)
(9178+1838)/(9178+535+1240+1838)
prp(model)
























