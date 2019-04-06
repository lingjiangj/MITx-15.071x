setwd("~/文档/15.071x/Unit6/Assignment 6")

#Problem 1.1
airlines = read.csv("AirlinesCluster.csv")
summary(airlines)

#Problem 1.2
##If we don't normalize the data, the clustering will be dominated by the variables that are on a larger scale.

#Problem 1.3
library(caret)
preproc = preProcess(airlines)
airlinesNorm = predict(preproc, airlines)
summary(airlinesNorm)

#Problem 2.1
distances = dist(airlinesNorm,method = "euclidean")
clusterAirlines = hclust(distances,method = "ward.D")
plot(clusterAirlines)

#Problem 2.2
clusterGroups = cutree(clusterAirlines, k = 5)
cluster1 = subset(airlinesNorm, clusterGroups ==1)
str(cluster1)

#Problem 2.3
B = tapply(airlines$Balance, clusterGroups, mean)
QM = tapply(airlines$QualMiles, clusterGroups, mean)
BM = tapply(airlines$BonusMiles, clusterGroups, mean)
BT = tapply(airlines$BonusTrans, clusterGroups, mean)
FM = tapply(airlines$FlightMiles, clusterGroups, mean)
FT = tapply(airlines$FlightTrans, clusterGroups, mean)
DSE = tapply(airlines$DaysSinceEnroll, clusterGroups, mean)
Vec = c(B,QM,BM,BT,FM,FT,DSE)
dim(Vec) = c(5,7)
colnames(Vec) = c("Balance", "QualMiles", "BonusMiles", "BonusTrans", "FlightMiles","FlightTrans", "DaysEnroll")
Vec

#Problem 2.4/2.5/2.6/2.7
sapply(1:7,function(x)which.max(Vec[,x]))

#Problem 3.1
k = 5
set.seed(88)
KMC.airlines = kmeans(airlinesNorm,centers = k, iter.max = 1000)
KMC.airlines$size

#Problem 3.2
B2 = tapply(airlines$Balance, KMC.airlines$cluster, mean)
QM2 = tapply(airlines$QualMiles, KMC.airlines$cluster, mean)
BM2 = tapply(airlines$BonusMiles, KMC.airlines$cluster, mean)
BT2= tapply(airlines$BonusTrans, KMC.airlines$cluster, mean)
FM2 = tapply(airlines$FlightMiles, KMC.airlines$cluster, mean)
FT2 = tapply(airlines$FlightTrans, KMC.airlines$cluster, mean)
DSE2 = tapply(airlines$DaysSinceEnroll, KMC.airlines$cluster, mean)
Vec2 = c(B2,QM2,BM2,BT2,FM2,FT2,DSE2)
dim(Vec2) = c(5,7)
colnames(Vec2) = c("Balance", "QualMiles", "BonusMiles", "BonusTrans", "FlightMiles", "FlightTrans", "DaysEnroll")
Vec2
Vec














