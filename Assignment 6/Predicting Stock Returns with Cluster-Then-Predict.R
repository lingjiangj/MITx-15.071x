setwd("~/文档/15.071x/Unit6/Assignment 6")

#Problem 1.1
stocks = read.csv("StocksCluster.csv")
str(stocks)

#Problem 1.2
prop.table(table(stocks$PositiveDec))

#Problem 1.3
cor(stocks)

#Problem 1.4
which.max(colMeans(stocks[1:11]))
which.min(colMeans(stocks[1:11]))

#Problem 2.1
library(caTools)
set.seed(144)
spl = sample.split(stocks$PositiveDec, SplitRatio = 0.7)
stocksTrain = subset(stocks, spl == TRUE)
stocksTest = subset(stocks, spl == FALSE)
StocksModel = glm(PositiveDec~.,data = stocksTrain,family = binomial)
PredictTrain = predict(StocksModel,type = "response")
table(stocksTrain$PositiveDec,PredictTrain > 0.5)
(990+3640)/nrow(stocksTrain)

#Problem 2.2
PredictTest = predict(StocksModel,newdata = stocksTest,type = "response")
table(stocksTest$PositiveDec,PredictTest > 0.5)
(417+1553)/nrow(stocksTest)

#Problem 2.3
table(stocksTest$PositiveDec)
1897/nrow(stocksTest)

#Problem 3.1
limitedTrain = stocksTrain
limitedTrain$PositiveDec = NULL
limitedTest = stocksTest
limitedTest$PositiveDec = NULL
## Needing to know the dependent variable value to assign an observation to a cluster defeats the purpose of the methodology

#Problem 3.2
library(caret)
preproc = preProcess(limitedTrain)
normtrain = predict(preproc,limitedTrain)
normtest = predict(preproc,limitedTest)
colMeans(normtrain)
colMeans(normtest)

#Problem 3.3
##The distribution of the ReturnJan variable is different in the training and testing set 

#Problem 3.4
k = 3
set.seed(144)
km = kmeans(normtrain, centers = k)
str(km)

#Problem 3.5
install.packages("flexclust")
library(flexclust)
km.kcca = as.kcca(km, normtrain)
clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata=normtest)
table(clusterTrain)
table(clusterTest)

#Problem 4.1
stocksTrain1 = subset(stocksTrain,clusterTrain == 1)
stocksTrain2 = subset(stocksTrain,clusterTrain == 2)
stocksTrain3 = subset(stocksTrain,clusterTrain == 3)
mean(stocksTrain1$PositiveDec)
mean(stocksTrain2$PositiveDec)
mean(stocksTrain3$PositiveDec)

#Problem 4.2
StocksModel1 = glm(PositiveDec~.,data = stocksTrain1,family = binomial)
StocksModel2 = glm(PositiveDec~.,data = stocksTrain2,family = binomial)
StocksModel3 = glm(PositiveDec~.,data = stocksTrain3,family = binomial)
StocksModel1$coefficients
StocksModel2$coefficients
StocksModel3$coefficients
rowVec = c(StocksModel1$coefficients[2:12],StocksModel2$coefficients[2:12],StocksModel3$coefficients[2:12])
dim(rowVec) = c(11,3)
rownames(rowVec) = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov")
rowVec

#Problem 4.3
stocksTest1 = subset(stocksTest, clusterTest == 1)
stocksTest2 = subset(stocksTest, clusterTest == 2)
stocksTest3 = subset(stocksTest, clusterTest == 3)
PredictTest1 = predict(StocksModel1, newdata=stocksTest1, type="response")>0.5
PredictTest2 = predict(StocksModel2, newdata=stocksTest2, type="response")>0.5
PredictTest3 = predict(StocksModel3, newdata=stocksTest3, type="response")>0.5
table(stocksTest1$PositiveDec,PredictTest1)
(30+774)/nrow(stocksTest1)

table(stocksTest2$PositiveDec,PredictTest2)
(388+757)/nrow(stocksTest2)

table(stocksTest3$PositiveDec,PredictTest3)
(49+13)/nrow(stocksTest3)

#Problem 4.4
AllPredictions = c(PredictTest1, PredictTest2, PredictTest3)
AllOutcomes = c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec)
table(AllPredictions, AllOutcomes)
(467+1544)/(467+353+1110+1544)
























