setwd("~/文档/15.071x/Unit6/Assignment 6")

#Problem 1.1
dailykos = read.csv("dailykos.csv")
distances = dist(dailykos,method = "euclidean")
kosClust = hclust(distances,method = "ward.D")

#Problem 1.2/1.3
plot(kosClust)
##1.2 - 2/3
##1.3 - 7/8

#Problem 1.4
ClusterGroups = cutree(kosClust, k = 7)
kos.1 = subset(dailykos,ClusterGroups == 1)
kos.2 = subset(dailykos,ClusterGroups == 2)
kos.3 = subset(dailykos,ClusterGroups == 3)
kos.4 = subset(dailykos,ClusterGroups == 4)
kos.5 = subset(dailykos,ClusterGroups == 5)
kos.6 = subset(dailykos,ClusterGroups == 6)
kos.7 = subset(dailykos,ClusterGroups == 7)
nrow(kos.3)
which.max(c(dim(kos.1)[1],dim(kos.2)[1],dim(kos.3)[1],dim(kos.4)[1],dim(kos.5)[1],dim(kos.6)[1],dim(kos.7)[1]))
which.min(c(dim(kos.1)[1],dim(kos.2)[1],dim(kos.3)[1],dim(kos.4)[1],dim(kos.5)[1],dim(kos.6)[1],dim(kos.7)[1]))
table(ClusterGroups)

#Problem 1.5
tail(sort(colMeans(kos.1)))

#Problem 1.6
tail(sort(colMeans(kos.2)))
tail(sort(colMeans(kos.3)))
tail(sort(colMeans(kos.4)))
tail(sort(colMeans(kos.5)))
tail(sort(colMeans(kos.6)))
tail(sort(colMeans(kos.7)))

#Problem 2.1
k = 7
set.seed(1000)
KMC.kos = kmeans(dailykos,centers = k)
kos.KMC1 = subset(dailykos,KMC.kos$cluster == 1)
kos.KMC2 = subset(dailykos,KMC.kos$cluster == 2)
kos.KMC3 = subset(dailykos,KMC.kos$cluster == 3)
kos.KMC4 = subset(dailykos,KMC.kos$cluster == 4)
kos.KMC5 = subset(dailykos,KMC.kos$cluster == 5)
kos.KMC6 = subset(dailykos,KMC.kos$cluster == 6)
kos.KMC7 = subset(dailykos,KMC.kos$cluster == 7)
nrow(kos.KMC3)
which.max(c(nrow(kos.KMC1),nrow(kos.KMC2),nrow(kos.KMC3),nrow(kos.KMC4),nrow(kos.KMC5),nrow(kos.KMC6),nrow(kos.KMC7)))
which.min(c(nrow(kos.KMC1),nrow(kos.KMC2),nrow(kos.KMC3),nrow(kos.KMC4),nrow(kos.KMC5),nrow(kos.KMC6),nrow(kos.KMC7)))

#Problem 2.2
tail(sort(colMeans(kos.KMC1)))
tail(sort(colMeans(kos.KMC2)))
tail(sort(colMeans(kos.KMC3)))
tail(sort(colMeans(kos.KMC4)))
tail(sort(colMeans(kos.KMC5)))
tail(sort(colMeans(kos.KMC6)))
tail(sort(colMeans(kos.KMC7)))

#Problem 2.3
## Hierarchical Cluster 7 

#Problem 2.4
## Hierarchical Cluster 5

#Problem 2.5
## No Hierarchical Cluster contains at least half of the points in K-Means Cluster 7. 

#Problem 2.6
## Hierarchical Cluster 2






