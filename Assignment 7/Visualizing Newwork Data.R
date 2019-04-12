setwd("~/文档/15.071x/Unit 7/Assignment 7")
#Problem 1.1
edges = read.csv("edges.csv")
users = read.csv("users.csv")
str(users)
sum(table(edges$V1),table(edges$V2))/nrow(users)

#Problem 1.2
table(users$school,users$locale)
## locale B

#Problem 1.3
table(users$gender,users$school)
## No

#Problem 2.1
install.packages("igraph")
library(igraph)
?graph.data.frame
g <- graph.data.frame(edges, FALSE, users)

#Problem 2.2
plot(g, vertex.size=5, vertex.label=NA)
## 4
## 7

#Problem 2.3
sum((degree(g) >= 10))

#Problem 2.4
V(g)$size = degree(g)/2+2
plot(g, vertex.label=NA)
max(V(g)$size)
min(V(g)$size)

#Problem 3.1
V(g)$color = "black"
V(g)$color[V(g)$gender == "A"] = "red"
V(g)$color[V(g)$gender == "B"] = "gray"
plot(g,vertex.label=NA)
## Gender B

#Problem 3.2
V(g)$color[V(g)$school == "A"] = "red"
V(g)$color[V(g)$school == "AB"] = "gray"
plot(g,vertex.label=NA)
## Yes
# Some, but not all, of the high-degree users attended school A correct

#Problem 3.3
table(users$locale)
V(g)$color[V(g)$locale == "A"] = "red"
V(g)$color[V(g)$locale == "B"] = "gray"
plot(g,vertex.label=NA)
## Locale B
## Locale A

#Problem 4
?igraph.plotting
## rglplot
## edge.width





