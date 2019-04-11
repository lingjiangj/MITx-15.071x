setwd("~/文档/15.071x/Unit 7/Assignment 7")
library(ggplot2)
library(maps)
library(ggmap)
statesMap = map_data("state")

#Problem 1.1
str(statesMap)
table(statesMap$group)

#Problem 1.2
ggplot(statesMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black")

#Problem 2.1
polling = read.csv("PollingImputed.csv")
Train = subset(polling, Year == 2004|Year == 2008)
Test = subset(polling, Year == 2012)
mod2 = glm(Republican~SurveyUSA+DiffCount, data=Train, family="binomial")
TestPrediction = predict(mod2, newdata=Test, type="response")
TestPredictionBinary = as.numeric(TestPrediction > 0.5)
predictionDataFrame = data.frame(TestPrediction, TestPredictionBinary, Test$State)
table(TestPredictionBinary)
mean(TestPrediction)

#Problem 2.2
predictionDataFrame$region = tolower(predictionDataFrame$Test.State)
predictionMap = merge(statesMap, predictionDataFrame, by = "region")
predictionMap = predictionMap[order(predictionMap$order),]
str(predictionMap)
str(statesMap)

#Problem 2.3
## Because we only make predictions for 45 states, we no longer have observations for some of the states. These observations were removed in the merging process. correct

#Problem 2.4
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) + geom_polygon(color = "black")
## Light blue

#Problem 2.5
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary))+ geom_polygon(color = "black") + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction)) + geom_polygon(color = "black") + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks = c(0, 1), labels = c("Democrat", "Republican"), name = "Prediction 2012")
## The two maps look very similar. This is because most of our predicted probabilities are close to 0 or close to 1. correct

#Problem 3.1
## We incorrectly predicted this state by predicting that it would be won by the Republican party.

#Problem 3.2
predictionDataFrame[predictionDataFrame$region=="florida",]
## Our prediction model did not do a very good job of correctly predicting the state of Florida, and we were very confident in our incorrect prediction. correct

#Problem 4.1
## linetype


#Problem 4.2
## size


























