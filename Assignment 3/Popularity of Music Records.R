setwd("~/文档/15.071x/Unit3/Assignment 3")

#Problem 1.1
songs <- read.csv("songs.csv")
nrow(subset(songs,year == 2010))

#Problem 1.2
nrow(subset(songs,artistname == "Michael Jackson"))

#Problem 1.3
a <- subset(songs,artistname == "Michael Jackson")
a$songtitle[which(a$Top10==TRUE)]

#Problem 1.4
table(songs$timesignature)

#Problem 1.5
songs$songtitle[which.max(songs$tempo)]

#Problem 2.1
SongsTrain <- subset(songs,year <= 2009)
SongsTest <- subset(songs,year > 2009)
str(SongsTrain)

#Problem 2.2/2.3/2.4
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]
SongsLog1 <- glm(Top10~.,data = SongsTrain,family = binomial)
summary(SongsLog1)

#Problem 3.1
cor(SongsTrain$loudness,SongsTrain$energy)

#Problem 3.2
SongsLog2 = glm(Top10 ~ . - loudness, data=SongsTrain, family=binomial)
summary(SongsLog2)

#Problem 3.3
SongsLog3 = glm(Top10 ~ . - energy, data=SongsTrain, family=binomial)
summary(SongsLog3)

#Problem 4.1
testPredict <- predict(SongsLog3,newdata = SongsTest,type = "response")
b <- table(SongsTest$Top10,testPredict >= 0.45)
(b["0","FALSE"]+b["1","TRUE"])/sum(b)

#Problem 4.2
table(SongsTest$Top10)

#Problem 4.3/4.4
table(SongsTest$Top10,testPredict >= 0.45)
19/(40+19)
309/(309+5)














