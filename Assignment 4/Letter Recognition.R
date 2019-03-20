setwd("~/文档/15.071x/Unit 4/Assignment 4")
letters <- read.csv("letters_ABPR.csv")

#Problem 1.1
letters$isB <- as.factor(letters$letter=="B")
library(caTools)
set.seed(1000)
split = sample.split(letters$isB,SplitRatio = 0.5)
train = subset(letters, split == TRUE)
test = subset(letters,split == FALSE)
table(test$isB)
1175/(1175+383)

#Problem1.2
library(rpart)
CARTb = rpart(isB~.-letter, data = train, method = "class")
prediction = predict(CARTb,newdata = test, type = "class")
table(test$isB,prediction)
(1118+340)/nrow(test)

#Problem 1.3
library(randomForest)
set.seed(1000)
RFb = randomForest(isB~.-letter,data = train)
prediction = predict(RFb,newdata = test)
table(test$isB,prediction)
(1163+374)/nrow(test)

#Problem 2.1
letters$letter = as.factor(letters$letter)
set.seed(2000)
split = sample.split(letters$letter,SplitRatio = 0.5)
train2 = subset(letters, split == TRUE)
test2 = subset(letters, split == FALSE)
table(test2$letter)
401/nrow(test)

#Problem 2.2
CARTletter = rpart(letter~.-isB,data = train2, method = "class")
predictionLetter = predict(CARTletter,newdata = test2, type = "class")
table(test2$letter,predictionLetter)
(348+318+363+340)/nrow(test2)

#Problem 2.3
RFletter = randomForest(letter~.-isB,data = train2)
predictionRF = predict(RFletter,newdata = test2)
table(test2$letter,predictionRF)
(391+380+394+365)/nrow(test2)










