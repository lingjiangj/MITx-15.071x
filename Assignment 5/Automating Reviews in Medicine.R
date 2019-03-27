setwd("~/文档/15.071x/Unit5/Assignment5")

#Problem 1.1
trials = read.csv("clinical_trial.csv", stringsAsFactors=FALSE)
summary(nchar(trials$abstract))

#Problem 1.2
table(nchar(trials$abstract)==0)

#Problem 1.3
trials$title[which.min(nchar(trials$title))]

#Problem 2.1
library(tm)
library(SnowballC)
corpusTitle = VCorpus(VectorSource(trials$title))
corpusTitle = tm_map(corpusTitle, tolower)
corpusTitle = tm_map(corpusTitle, PlainTextDocument)
corpusTitle = tm_map(corpusTitle, removePunctuation)
corpusTitle = tm_map(corpusTitle, removeWords, stopwords("english"))
corpusTitle = tm_map(corpusTitle, stemDocument)
dtmTitle = DocumentTermMatrix(corpusTitle)
dtmTitle = removeSparseTerms(dtmTitle, 0.95)
dtmTitle = as.data.frame(as.matrix(dtmTitle))
str(dtmTitle)

corpusAbstract = VCorpus(VectorSource(trials$abstract))
corpusAbstract = tm_map(corpusAbstract,tolower)
corpusAbstract = tm_map(corpusAbstract,PlainTextDocument)
corpusAbstract = tm_map(corpusAbstract,removePunctuation)
corpusAbstract = tm_map(corpusAbstract,removeWords,stopwords("english"))
corpusAbstract = tm_map(corpusAbstract,stemDocument)
dtmAbstract = DocumentTermMatrix(corpusAbstract)
dtmAbstract = removeSparseTerms(dtmAbstract,0.95)
dtmAbstract = as.data.frame(as.matrix(dtmAbstract))
str(dtmAbstract)

#Problem 2.3
which.max(colSums(dtmAbstract))

#Problem 3.1
colnames(dtmTitle) = paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))

#Problem 3.2
dtm = cbind(dtmTitle, dtmAbstract)
dtm$trial = trials$trial
ncol(dtm)

#Problem 3.3
library(caTools)
set.seed(144)
split = sample.split(dtm$trial,SplitRatio = 0.7)
train = subset(dtm,split == TRUE)
test = subset(dtm,split == FALSE)
table(test$trial)
313/nrow(test)

#Problem 3.4
library(rpart)
library(rpart.plot)
trialCART = rpart(trial~., data = train,method = "class" )
prp(trialCART)

#Problem 3.5
preTrain = predict(trialCART)[,2]
summary(preTrain)

#Problem 3.7
table(train$trial,preTrain >= 0.5)
(631+441)/nrow(train)
441/(441+131)
631/(631+99)

#Problem 4.1
predTest = predict(trialCART,newdata = test)[,2]
table(test$trial,predTest >= 0.5)
(261+162)/nrow(test)

#Problem 4.2
library(ROCR)
pred = prediction(predTest,test$trial)
as.numeric(performance(pred,"auc")@y.values)


















































