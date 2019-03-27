setwd("~/文档/15.071x/Unit5/Assignment5")

#Problem 1.1
emails = read.csv("emails.csv",stringsAsFactors = FALSE)
str(emails)

#Problem 1.2
table(emails$spam)

#Problem 1.3
emails$text[1]

#Problem 1.5/1.6
max(nchar(emails$text))
which.min(nchar(emails$text))

#Problem 2.1
library(SnowballC)
corpus = VCorpus(VectorSource(emails$text))
corpus = tm_map(corpus,tolower)
corpus = tm_map(corpus,PlainTextDocument)
corpus = tm_map(corpus,removePunctuation)
corpus = tm_map(corpus, removeWords,stopwords("english"))
corpus = tm_map(corpus,stemDocument)
dtm = DocumentTermMatrix(corpus)
dtm

#Problem 2.2
spdtm = removeSparseTerms(dtm,0.95)
spdtm

#Problem 2.3
emailsSparse = as.data.frame(as.matrix(spdtm))
colnames(emailsSparse) = make.names(colnames(emailsSparse))
frequency = colSums(emailsSparse)
which.max(frequency)

#Problem 2.4/2.5
emailsSparse$spam = emails$spam
a <- colSums(subset(emailsSparse,spam == 0))
a[a>=5000]
b <- colSums(subset(emailsSparse,spam == 1))
b[b>=1000]

#Problem 3.1
emailsSparse$spam = as.factor(emailsSparse$spam)
library(caTools)
set.seed(123)
split = sample.split(emailsSparse$spam,SplitRatio = 0.7)
train = subset(emailsSparse,split == TRUE)
test = subset(emailsSparse,split == FALSE)

library(rpart)
library(randomForest)
spamLog = glm(spam~.,data = train,family = "binomial")
spamCART = rpart(spam~.,data = train,method = "class")
spamRF = randomForest(spam~., data=train)

predTrainLog = predict(spamLog,type = "response")
predTrainCART = predict(spamCART)[,2]
predTrainRF = predict(spamRF, type="prob")[,2]

table(predTrainLog < 0.00001)
table(predTrainLog > 0.99999)
table(predTrainLog >= 0.00001 &predTrainLog <= 0.99999)

#Problem 3.2
summary(spamLog)

#Problem 3.3
prp(spamCART)

#Problem 3.4
table(train$spam,predTrainLog > 0.5)
(3052+954)/nrow(train)

#Problem 3.5
library(ROCR)
predROCR = prediction(predTrainLog,train$spam)
performance(predROCR,"auc")@y.values

#Problem 3.6
table(train$spam,predTrainCART > 0.5)
(2885+894)/nrow(train)

#Problem 3.7
ROCRpred = prediction(predTrainCART,train$spam)
as.numeric(performance(ROCRpred,"auc")@y.values)

#Problem 3.8
table(train$spam,predTrainRF > 0.5)
(3018+907)/nrow(train)

#Problem 3.9
ROCRpredRF = prediction(predTrainRF,train$spam)
as.numeric(performance(ROCRpredRF,"auc")@y.values)

#Problem 4.1
predTestLog = predict(spamLog,newdata = test,type = "response")
table(test$spam,predTestLog > 0.5)
(1257+376)/nrow(test)

#Problem 4.2
ROCRTestLog = prediction(predTestLog,test$spam)
as.numeric(performance(ROCRTestLog,"auc")@y.values)

#Problem 4.3
predTestCART = predict(spamCART,newdata = test)[,2]
table(test$spam,predTestCART > 0.5)
(1228+386)/nrow(test)

#Problem 4.4
ROCRTestCART = prediction(predTestCART,test$spam)
as.numeric(performance(ROCRTestCART,"auc")@y.values)

#Problem 4.5
predTestRF = predict(spamRF,newdata = test,type = "prob")[,2]
table(test$spam,predTestRF > 0.5)
(1290+383)/nrow(test)

#Problem 4.6
ROCRTestRF = prediction(predTestRF,test$spam)
as.numeric(performance(ROCRTestRF,"auc")@y.values)







































