setwd("~/文档/15.071x/Unit3/Assignment 3")
baseball <- read.csv("baseball.csv")

#Problem 1.1
nrow(baseball)

#Problem 1.2
length(table(baseball$Year))

#Problem 1.3
baseball <- subset(baseball, Playoffs == 1)
nrow(baseball)

#Problem 1.4
table(baseball$Year)

#Problem 2.1
PlayoffTable = table(baseball$Year)
names(PlayoffTable)

#Problem 2.2
PlayoffTable[c("1990","2001")]

#Problem 2.3
baseball$NumCompetitors <- PlayoffTable[as.character(baseball$Year)]

#Problem 2.4
table(baseball$NumCompetitors)

#Problem 3.1
baseball$WorldSeries <- as.numeric(baseball$RankPlayoffs == 1)
table(baseball$WorldSeries)

#Problem 3.2
summary(glm(WorldSeries~Year+RS+RA+W+OBP+SLG+BA+RankSeason+OOBP+OSLG+NumCompetitors+League, data=baseball, family="binomial"))

#Problem 4.1
LogModel <- glm(WorldSeries~Year+RA+RankSeason+NumCompetitors,data = baseball, family = "binomial")
summary(LogModel)

#Problem 4.2
cor(baseball[c("Year", "RA", "RankSeason", "NumCompetitors")])

#Problem 4.3
model1 <- glm(WorldSeries~Year+RA,data = baseball, family = "binomial")
model2 <- glm(WorldSeries~Year+RankSeason,data = baseball, family = "binomial")
model3 <- glm(WorldSeries~Year+NumCompetitors,data = baseball, family = "binomial")
model4 <- glm(WorldSeries~RankSeason+RA,data = baseball, family = "binomial")
model5 <- glm(WorldSeries~NumCompetitors+RA,data = baseball, family = "binomial")
model6 <- glm(WorldSeries~RankSeason+NumCompetitors,data = baseball, family = "binomial")

















